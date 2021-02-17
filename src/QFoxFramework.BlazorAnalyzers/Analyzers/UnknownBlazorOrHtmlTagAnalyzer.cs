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

namespace QFoxFramework.BlazorAnalyzers.Analyzers
{
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    // ReSharper disable once UnusedType.Global
    public class UnknownBlazorOrHtmlTagAnalyzer : DiagnosticAnalyzer
    {
        private static readonly LocalizableString UnknownBlazorOrHtmlTagTitle = new LocalizableResourceString(
            nameof(Resources.UnknownBlazorOrHtmlTagTitle),
            Resources.ResourceManager,
            typeof(Resources));

        private static readonly LocalizableString UnknownBlazorOrHtmlTagMessageFormat = new LocalizableResourceString(
            nameof(Resources.UnknownBlazorOrHtmlTagMessageFormat),
            Resources.ResourceManager,
            typeof(Resources));
        
        private static readonly DiagnosticDescriptor UnknownBlazorOrHtmlTag = new(
            DiagnosticIds.UnknownBlazorOrHtmlTag,
            UnknownBlazorOrHtmlTagTitle,
            UnknownBlazorOrHtmlTagMessageFormat,
            DiagnosticCategories.Blazor,
            DiagnosticSeverity.Error,
            true);

        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics
            => ImmutableArray.Create(                UnknownBlazorOrHtmlTag);

        public override void Initialize(AnalysisContext context)
        {
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.Analyze |
                                                   GeneratedCodeAnalysisFlags.ReportDiagnostics);
            context.EnableConcurrentExecution();
            
            context.RegisterSyntaxNodeAction(AnalyzeUnknownBlazorOrHtmlTag, SyntaxKind.InvocationExpression);
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

            using var document = new HtmlParser().ParseDocument(markupString);
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