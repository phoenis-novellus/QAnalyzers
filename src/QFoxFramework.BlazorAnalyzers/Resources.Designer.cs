﻿//------------------------------------------------------------------------------
// <auto-generated>
//     This code was generated by a tool.
//
//     Changes to this file may cause incorrect behavior and will be lost if
//     the code is regenerated.
// </auto-generated>
//------------------------------------------------------------------------------

namespace QFoxFramework.BlazorAnalyzers {
    using System;
    
    
    [System.CodeDom.Compiler.GeneratedCodeAttribute("System.Resources.Tools.StronglyTypedResourceBuilder", "4.0.0.0")]
    [System.Diagnostics.DebuggerNonUserCodeAttribute()]
    [System.Runtime.CompilerServices.CompilerGeneratedAttribute()]
    internal class Resources {
        
        private static System.Resources.ResourceManager resourceMan;
        
        private static System.Globalization.CultureInfo resourceCulture;
        
        [System.Diagnostics.CodeAnalysis.SuppressMessageAttribute("Microsoft.Performance", "CA1811:AvoidUncalledPrivateCode")]
        internal Resources() {
        }
        
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        internal static System.Resources.ResourceManager ResourceManager {
            get {
                if (object.Equals(null, resourceMan)) {
                    System.Resources.ResourceManager temp = new System.Resources.ResourceManager("QFoxFramework.BlazorAnalyzers.Resources", typeof(Resources).Assembly);
                    resourceMan = temp;
                }
                return resourceMan;
            }
        }
        
        [System.ComponentModel.EditorBrowsableAttribute(System.ComponentModel.EditorBrowsableState.Advanced)]
        internal static System.Globalization.CultureInfo Culture {
            get {
                return resourceCulture;
            }
            set {
                resourceCulture = value;
            }
        }
        
        internal static string UnknownBlazorOrHtmlTagMessageFormat {
            get {
                return ResourceManager.GetString("UnknownBlazorOrHtmlTagMessageFormat", resourceCulture);
            }
        }
        
        internal static string UnknownBlazorOrHtmlTagTitle {
            get {
                return ResourceManager.GetString("UnknownBlazorOrHtmlTagTitle", resourceCulture);
            }
        }
        
        internal static string UnknownBlazorComponentParameterTitle {
            get {
                return ResourceManager.GetString("UnknownBlazorComponentParameterTitle", resourceCulture);
            }
        }
        
        internal static string UnknownBlazorComponentParameterMessageFormat {
            get {
                return ResourceManager.GetString("UnknownBlazorComponentParameterMessageFormat", resourceCulture);
            }
        }
    }
}
