using Microsoft.Owin;
using Owin;

[assembly: OwinStartupAttribute(typeof(BuildSite.Startup))]
namespace BuildSite
{
    public partial class Startup
    {
        public void Configuration(IAppBuilder app)
        {
            ConfigureAuth(app);
        }
    }
}
