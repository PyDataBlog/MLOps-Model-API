using System;
using System.Collections.Generic;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Threading.Tasks;

namespace _01_Problem
{
    class StartUp
    {
        static void Main(string[] args)
        {
            var n = int.Parse(Console.ReadLine());
            var list = new List<Person>();

            for (int i = 0; i < n; i++)
            {
                var line = Console.ReadLine().Split(' ');
                var age = int.Parse(line[1]);

                var Person = new Person(line[0], age);

                if (age > 30)
                {
                    list.Add(Person);
                }
            }
            

            foreach (var item in list.OrderBy(i => i.Name))
            {
                Console.WriteLine($"{item.Name} - {item.Age}");
            }
            

        }
    }
}
