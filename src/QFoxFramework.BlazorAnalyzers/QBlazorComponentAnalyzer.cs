using System.Collections.Generic;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
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
            context.RegisterCompilationStartAction(ctx =>
            {
                ctx.RegisterSyntaxNodeAction(AnalyzeUnknownBlazorOrHtmlTag, SyntaxKind.InvocationExpression);
                ctx.RegisterSyntaxNodeAction(AnalyzeUnknownBlazorComponentParameter, SyntaxKind.MethodDeclaration);
            });
        }

        private static void AnalyzeUnknownBlazorComponentParameter(SyntaxNodeAnalysisContext syntaxNodeAnalysisContext)
        {
            var declaration = (MethodDeclarationSyntax) syntaxNodeAnalysisContext.Node;
            var invocations = declaration.DescendantNodes()
                .OfType<InvocationExpressionSyntax>();

            List<(ExpressionSyntax builder, ITypeSymbol? componentType)> componentList = new();
            
            foreach (var invocation in invocations)
            {
                var memberAccess = invocation.Expression as MemberAccessExpressionSyntax;
                var methodSymbol = syntaxNodeAnalysisContext
                    .SemanticModel
                    .GetSymbolInfo(invocation).Symbol as IMethodSymbol;

                if (methodSymbol is null) continue;
                if (memberAccess is null) continue;
                
                var methodName = methodSymbol.Name;
                
                switch (methodName)
                {
                    case "OpenComponent":
                    {
                      
                        componentList
                            .Where(x => x.builder.IsEquivalentTo(memberAccess.Expression))
                            .ToList()
                            .ForEach(x => componentList.Remove(x));
                        
                        componentList.Add((memberAccess.Expression,  methodSymbol.TypeArguments[0]));

                        break;
                    }
                    case "CloseComponent":
                    {
                        componentList
                            .Where(x => x.builder.IsEquivalentTo(memberAccess.Expression))
                            .ToList()
                            .ForEach(x => componentList.Remove(x));
                 
                        break;
                    }
                    case "AddAttribute":
                    {
                        
                        var (_, componentType) = componentList
                            .FirstOrDefault(x => x.builder.IsEquivalentTo(memberAccess.Expression));

                        var argumentIsStringLiteral = invocation
                            .ArgumentList
                            .Arguments[1]
                            .Expression
                            .IsKind(SyntaxKind.StringLiteralExpression);
                        
                        if(componentType is null || !argumentIsStringLiteral) break;
                            
                        var parameterName = invocation.ArgumentList.Arguments[1].Expression.GetFirstToken().ValueText;
                        var hasParameter = componentType
                            .IncludeBaseTypes()
                            .Any(t =>
                                t.GetMembers()
                                    .Any(x =>
                                        x.Name == parameterName
                                        && x.Kind == SymbolKind.Property
                                        && x.GetAttributes()
                                            .Any(a => a.AttributeClass?.Name == "ParameterAttribute")));

                        var hasCatchAllParameter = componentType
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
                        if (!hasParameter && !hasCatchAllParameter)
                        {
                            var diagnostic = Diagnostic.Create(UnknownBlazorComponentParameter,
                                invocation.GetLocation(), parameterName, componentType.MetadataName);

                            syntaxNodeAnalysisContext.ReportDiagnostic(diagnostic);
                        }

                        break;
                    }
                }
            }
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