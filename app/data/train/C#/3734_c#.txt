using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static System.Console;

namespace HashTableNamespace
{
    class Program
    {
        static void Main(string[] args)
        {
            Hash tmp;
            WriteLine(" Choose 1 or 2 variant of hash-function");
            int number = int.Parse(ReadLine());
            if (number == 1)
            {
                tmp = new Hash(new HashFunctionOne());
            }
            else tmp = new Hash(new HashFunctionTwo());
            tmp.Add("trep");
            tmp.Add("pert");
            tmp.Add("trop");
            tmp.Add("treerered");
            tmp.Add("trep");
            tmp.Add("123");
            Console.WriteLine(tmp.Search("pert"));
            tmp.Print();
            Console.WriteLine(tmp.Delete("123"));
            tmp.Print();
        }
    }
}
