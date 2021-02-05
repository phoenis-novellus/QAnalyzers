using Microsoft.AspNetCore.Components;
using Microsoft.AspNetCore.Components.Rendering;

namespace QFoxFramework.BlazorAnalyzers.Demo.Pages
{
    public class CustomRenderTreeBuilder : ComponentBase
    {
        protected override void BuildRenderTree(RenderTreeBuilder builder)
        {
            var i = 0;
            
            builder.OpenComponent(i++, typeof(Counter));
            builder.AddAttribute(i++, "Test");
            builder.CloseComponent();
            
            builder.OpenComponent<Counter>(i++);
            
            builder.AddAttribute(i, "Test2");
            builder.CloseComponent();
        }
    }
}