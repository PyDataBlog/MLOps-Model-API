using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Marmalade_Global
{
    class EmployeesSchedule
    {
        public DateTime Day { get; set; }
        public IEmployee Employee { get; set; }
        public List<Task> Tasks { get; set; } = new List<Task>();

        public EmployeesSchedule()
        {
        }
    }
}
