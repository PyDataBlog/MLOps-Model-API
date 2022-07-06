using System;
using System.Collections.Generic;
using System.Text;

namespace DesignPatterns.Behavioral.Visitor
{
    public class Director : Employee
    {
        public Director(string name, double income, int vacationDays)
            : base(name, income, vacationDays) { }
    }
}
