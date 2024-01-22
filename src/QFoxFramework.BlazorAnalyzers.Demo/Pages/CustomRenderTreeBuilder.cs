using Microsoft.AspNetCore.Components;
using Microsoft.AspNetCore.Components.Rendering;

namespace QFoxFramework.BlazorAnalyzers.Demo.Pages
{
    public class CustomRenderTreeBuilder : ComponentBase
    {
        protected override void BuildRenderTree(RenderTreeBuilder builder)
        {
            builder.OpenComponent(0, typeof(Counter));
            builder.AddAttribute(1, "Test");
            builder.CloseComponent();
            
            builder.OpenComponent<Counter>(2);
            
            builder.AddAttribute(3, "Test2");
            builder.CloseComponent();
        }
    }
}