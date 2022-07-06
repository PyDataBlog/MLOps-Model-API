using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _02.Get_largest_number
{
    class Program
    {
        static int GetingMax(int firstMax, int secondMax)
        {
            return (firstMax > secondMax ? firstMax : secondMax);
        }
        static void Main()
        {
            int[] array = Console.ReadLine().Split(' ').Select(int.Parse).ToArray();
            int firstMax = array[0];
            int secondMax = array[1];
            int thirdMax = array[2];

            if (GetingMax(firstMax, secondMax) > thirdMax )
            {
                Console.WriteLine(GetingMax(firstMax, secondMax));
            }
            else
            {
                Console.WriteLine(thirdMax);
            }
        }
    }
}
