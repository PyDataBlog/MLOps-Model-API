using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Numerics;

namespace _03.Endurance_Rally
{
    class Program
    {
        static void Main(string[] args)
        {
            var driversNames = Console.ReadLine().Split(' ').ToArray();
            var zones = Console.ReadLine().Split(' ').Select(double.Parse).ToArray();
            var checkpointsIndex = Console.ReadLine().Split(' ').Select(int.Parse).ToArray();

            foreach (var driver in driversNames)
            {
                double currentFuel = (int)driver[0];
                var isFinish = false;

                for (int i = 0; i < zones.Length; i++)
                {
                    if (checkpointsIndex.Contains(i))
                    {
                        currentFuel += zones[i];
                    }
                    else
                    {

                        currentFuel -= zones[i];


                    }

                    if (currentFuel <= 0)
                    {
                        var reachedIndex = i;
                        Console.WriteLine("{0} - reached {1}", driver, reachedIndex);
                        isFinish = true;
                        break;
                    }
                }
                if (!isFinish)
                {
                    Console.WriteLine("{0} - fuel left {1:F2}", driver, currentFuel);
                }
            }
        }
    }
}
