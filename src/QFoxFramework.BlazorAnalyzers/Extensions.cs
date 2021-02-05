using System.Collections.Generic;
using Microsoft.CodeAnalysis;

namespace QFoxFramework.BlazorAnalyzers
{
    public static class Extensions
    {
        public static IEnumerable<ITypeSymbol> IncludeBaseTypes(this ITypeSymbol type)
        {
            var current = type;
            while (current is not null)
            {
                yield return current;
                current = current.BaseType;
            }
        }
    }
}