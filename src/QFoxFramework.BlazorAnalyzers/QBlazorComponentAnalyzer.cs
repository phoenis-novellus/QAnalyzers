using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.RegularExpressions;
using AngleSharp.Dom;
using AngleSharp.Html.Dom;
using AngleSharp.Html.Parser;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace QFoxFramework.BlazorAnalyzers
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class QBlazorComponentAnalyzer : DiagnosticAnalyzer
    {
        private static readonly LocalizableString UnknownBlazorOrHtmlTagTitle = new LocalizableResourceString(
            nameof(Resources.UnknownBlazorOrHtmlTagTitle),
            Resources.ResourceManager,
            typeof(Resources));

        private static readonly LocalizableString UnknownBlazorOrHtmlTagMessageFormat = new LocalizableResourceString(
            nameof(Resources.UnknownBlazorOrHtmlTagMessageFormat),
            Resources.ResourceManager,
            typeof(Resources));


        private static readonly LocalizableString UnknownBlazorComponentParameterTitle = new LocalizableResourceString(
            nameof(Resources.UnknownBlazorComponentParameterTitle),
            Resources.ResourceManager,
            typeof(Resources));

        private static readonly LocalizableString UnknownBlazorComponentParameterMessageFormat =
            new LocalizableResourceString(
                nameof(Resources.UnknownBlazorComponentParameterMessageFormat),
                Resources.ResourceManager,
                typeof(Resources));

        private static readonly DiagnosticDescriptor UnknownBlazorOrHtmlTag = new(
            DiagnosticIds.UnknownBlazorOrHtmlTag,
            UnknownBlazorOrHtmlTagTitle,
            UnknownBlazorOrHtmlTagMessageFormat,
            DiagnosticCategories.Blazor,
            DiagnosticSeverity.Error,
            true);

        private static readonly DiagnosticDescriptor UnknownBlazorComponentParameter = new(
            DiagnosticIds.UnknownBlazorComponentParameter,
            UnknownBlazorComponentParameterTitle,
            UnknownBlazorComponentParameterMessageFormat,
            DiagnosticCategories.Blazor,
            DiagnosticSeverity.Error,
            true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
            => ImmutableArray.Create(
                UnknownBlazorOrHtmlTag,
                UnknownBlazorComponentParameter
            );

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze |
                                                   GeneratedCodeAnalysisFlags.ReportDiagnostics);
            context.EnableConcurrentExecution();

            // See https://github.com/dotnet/roslyn/blob/master/docs/analyzers/Analyzer%20Actions%20Semantics.md for more information
            
            context.RegisterSyntaxNodeAction(AnalyzeUnknownBlazorOrHtmlTag, SyntaxKind.InvocationExpression);
            context.RegisterSyntaxNodeAction(AnalyzeUnknownBlazorComponentParameter, SyntaxKind.MethodDeclaration);
        }

        private static void AnalyzeUnknownBlazorComponentParameter(SyntaxNodeAnalysisContext syntaxNodeAnalysisContext)
        {
            var declaration = (MethodDeclarationSyntax) syntaxNodeAnalysisContext.Node;
            var invocations = declaration.DescendantNodes()
                .OfType<InvocationExpressionSyntax>();

            Stack<ITypeSymbol?> componentStack = new();

            foreach (var invocation in invocations)
            {
                var methodSymbol = syntaxNodeAnalysisContext
                    .SemanticModel
                    .GetSymbolInfo(invocation).Symbol as IMethodSymbol;

                if (methodSymbol is null) continue;

                var methodName = methodSymbol.Name;

                switch (methodName)
                {
                    case "OpenElement":
                        componentStack.Push(null);
                        break;
                    case "OpenComponent":
                        var typeSymbol = GetComponentTypeSymbol(syntaxNodeAnalysisContext, methodSymbol, invocation);
                        
                        if (typeSymbol is not null)
                        {
                            componentStack.Push(typeSymbol);
                        }

                        break;
                    case "CloseElement":
                        if (componentStack.Peek() is null)
                        {
                            componentStack.Pop();
                        }
                        break;
                    case "CloseComponent":
                        if (componentStack.Peek() is not null)
                        {
                            componentStack.Pop();
                        }
                        break;
                    case "AddAttribute":
                        var currentComponentType = componentStack.Peek();
                        ValidateAddAttribute(syntaxNodeAnalysisContext, currentComponentType, invocation);

                        break;
                }
            }
        }

        private static ITypeSymbol? GetComponentTypeSymbol(
            SyntaxNodeAnalysisContext syntaxNodeAnalysisContext,
            IMethodSymbol methodSymbol, 
            InvocationExpressionSyntax invocation)
        {
            ITypeSymbol? typeSymbol = null;

            if (methodSymbol.IsGenericMethod)
            {
                typeSymbol = methodSymbol.TypeArguments[0];
            }
            else if (invocation.ArgumentList.Arguments[1].Expression is TypeOfExpressionSyntax
                typeOfExpression)
            {
                typeSymbol = (ITypeSymbol?) syntaxNodeAnalysisContext
                    .SemanticModel
                    .GetSymbolInfo(typeOfExpression.Type)
                    .Symbol;
            }

            return typeSymbol;
        }

        private static void ValidateAddAttribute(SyntaxNodeAnalysisContext syntaxNodeAnalysisContext,
            ITypeSymbol? currentComponentType,
            InvocationExpressionSyntax invocation)
        {
            var argumentIsStringLiteral = invocation
                .ArgumentList
                .Arguments[1]
                .Expression
                .IsKind(SyntaxKind.StringLiteralExpression);

            if (currentComponentType is null || !argumentIsStringLiteral) return;

            var parameterName = invocation.ArgumentList.Arguments[1].Expression.GetFirstToken().ValueText;
            var hasParameter = currentComponentType
                .IncludeBaseTypes()
                .Any(t =>
                    t.GetMembers(parameterName)
                        .Any(x =>
                            x.Kind == SymbolKind.Property
                            && x.GetAttributes()
                                .Any(a => a.AttributeClass?.Name == "ParameterAttribute")));

            var hasCatchAllParameter = currentComponentType
                .IncludeBaseTypes()
                .Any(t =>
                    t.GetMembers()
                        .Any(x =>
                            x.Kind == SymbolKind.Property
                            && x.GetAttributes()
                                .Any(a =>
                                    a.AttributeClass?.Name == "ParameterAttribute"
                                    && a.NamedArguments
                                        .Any(na =>
                                            na.Key == "CaptureUnmatchedValues"))));

            //Parameter(CaptureUnmatchedValues = true)
            if (hasParameter || hasCatchAllParameter) return;

            var diagnostic = Diagnostic.Create(UnknownBlazorComponentParameter,
                invocation.GetLocation(), parameterName, currentComponentType.MetadataName);

            syntaxNodeAnalysisContext.ReportDiagnostic(diagnostic);
        }

        private static void AnalyzeUnknownBlazorOrHtmlTag(SyntaxNodeAnalysisContext syntaxNodeAnalysisContext)
        {
            var invocation = (InvocationExpressionSyntax) syntaxNodeAnalysisContext.Node;
            var methodSymbol =
                syntaxNodeAnalysisContext.SemanticModel.GetSymbolInfo(invocation).Symbol as IMethodSymbol;
            var methodName = methodSymbol?.Name;

            if (methodName != "OpenElement" && methodName != "AddMarkupContent") return;
            if (!invocation.ArgumentList.Arguments[1].Expression.IsKind(SyntaxKind.StringLiteralExpression)) return;

            var markupString = invocation.ArgumentList.Arguments[1].Expression.GetFirstToken().ValueText;

            if (methodName == "OpenElement")
            {
                markupString = $"<{markupString}></{markupString}>";
            }

            var document = new HtmlParser().ParseDocument(markupString);
            document.DescendentsAndSelf<IHtmlUnknownElement>()
                .Select(x =>
                    Diagnostic.Create(UnknownBlazorOrHtmlTag, invocation.GetLocation(),
                        GetOriginalTagName(x.LocalName, markupString)))
                .ToList()
                .ForEach(syntaxNodeAnalysisContext.ReportDiagnostic);
        }

        private static string GetOriginalTagName(string tagName, string markupString)
            => Regex.Match(markupString, tagName, RegexOptions.IgnoreCase).Value;
    }
}