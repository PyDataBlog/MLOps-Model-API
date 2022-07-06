using Sputnik.Selenium;

namespace Sputnik.Core.Commands
{
    public class GotoCommand
    {
        /// <summary>
        /// Will navigate to the Url set on a page's Url property
        /// </summary>
        /// <param name="Url">The Url to navigate to</param>
        public void ByUrl(string Url)
        {
            Driver.Instance.Navigate().GoToUrl(Url);
        }
    }
}
