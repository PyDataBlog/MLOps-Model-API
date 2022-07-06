using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace TelegramBot.Models
{
    public class Entity
    {
        public string type { get; set; }
        public long offset { get; set; }
        public long length { get; set; }
    }
}
