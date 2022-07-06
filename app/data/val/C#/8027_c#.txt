using System;
using System.Configuration;

namespace StaticService.Configuration
{
    public static class AppSettings
    {
        private static readonly GlobalSection Global;

        static AppSettings()
        {
            Global = (GlobalSection)ConfigurationManager.GetSection("global");
            Urls = Global.Url.Split(new[] { ',', ';' }, StringSplitOptions.RemoveEmptyEntries);
            CultureResources = Global.CultureResources.Split(new[] { ',', ';' }, StringSplitOptions.RemoveEmptyEntries);
        }

        public static string Description => Global.Description;

        public static string DisplayName => Global.DisplayName;

        public static string ServiceName => Global.ServiceName;

        public static string Root => Global.Root;

        public static string[] Urls { get; }

        public static string[] CultureResources { get; }
    }
}