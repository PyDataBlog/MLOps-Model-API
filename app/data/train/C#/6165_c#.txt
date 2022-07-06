using Autofac.Integration.Mvc;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Web;

namespace CambodiaProvider.Core.Infrastructure.DependencyManagement
{
    public class AutofacRequestLifetimeHttpModule : IHttpModule
    {
        #region New

        public void Init(HttpApplication context)
        {
            Guard.ArgumentNotNull(() => context);

            context.EndRequest += OnEndRequest;
        }

        public static void OnEndRequest(object sender, EventArgs e)
        {
            if (LifetimeScopeProvider != null)
            {
                LifetimeScopeProvider.EndLifetimeScope();
            }
        }

        public static void SetLifetimeScopeProvider(ILifetimeScopeProvider lifetimeScopeProvider)
        {
            if (lifetimeScopeProvider == null)
            {
                throw new ArgumentNullException("lifetimeScopeProvider");
            }
            LifetimeScopeProvider = lifetimeScopeProvider;
        }


        internal static ILifetimeScopeProvider LifetimeScopeProvider
        {
            get;
            set;
        }

        public void Dispose()
        {
        }

        #endregion
    }
}
