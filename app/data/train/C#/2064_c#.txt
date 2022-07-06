using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace AppRequestGenerator
{
    public class TabSettings
    {
        public TabSettings(string name)
        {
            this.Name = name;
            this.Settings = new Dictionary<string, object>();
        }

        public string Name { get; set; }
        public Dictionary<string, object> Settings;
    }
}
