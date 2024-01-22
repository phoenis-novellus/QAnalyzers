using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;

namespace QFoxFramework.BlazorAnalyzers.Analyzers
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    // ReSharper disable once UnusedType.Global
    public class UnknownBlazorComponentParameterAnalyzer : DiagnosticAnalyzer
    {
        private static readonly LocalizableString UnknownBlazorComponentParameterTitle = new LocalizableResourceString(
            nameof(Resources.UnknownBlazorComponentParameterTitle),
            Resources.ResourceManager,
            typeof(Resources));

        private static readonly LocalizableString UnknownBlazorComponentParameterMessageFormat =
            new LocalizableResourceString(
                nameof(Resources.UnknownBlazorComponentParameterMessageFormat),
                Resources.ResourceManager,
                typeof(Resources));

        private static readonly DiagnosticDescriptor UnknownBlazorComponentParameter = new(
            DiagnosticIds.UnknownBlazorComponentParameter,
            UnknownBlazorComponentParameterTitle,
            UnknownBlazorComponentParameterMessageFormat,
            DiagnosticCategories.Blazor,
            DiagnosticSeverity.Error,
            true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
            => ImmutableArray.Create(UnknownBlazorComponentParameter);

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze |
                                                   GeneratedCodeAnalysisFlags.ReportDiagnostics);
            context.EnableConcurrentExecution();
            
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
                    case "OpenComponent":
                        var typeSymbol = GetComponentTypeSymbol(syntaxNodeAnalysisContext, methodSymbol, invocation);
                        componentStack.Push(typeSymbol);

                        break;
                    case "CloseComponent":
                        componentStack.Pop();

                        break;
                    case "AddAttribute":
                    case "AddComponentParameter":
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

        private static void ValidateAddAttribute(
            SyntaxNodeAnalysisContext syntaxNodeAnalysisContext,
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
            
            if (hasParameter || hasCatchAllParameter) return;

            var diagnostic = Diagnostic.Create(UnknownBlazorComponentParameter,
                invocation.GetLocation(), parameterName, currentComponentType.MetadataName);

            syntaxNodeAnalysisContext.ReportDiagnostic(diagnostic);
        }

    }
}