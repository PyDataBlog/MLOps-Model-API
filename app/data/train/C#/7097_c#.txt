using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _05Lab_Bomb_Numbers
{
    class Program
    {
        static void Main(string[] args)
        {
            //Write a program that reads sequence of numbers and special bomb number with a certain power. 
            //Your task is to detonate every occurrence of the special bomb number and according to its power his neighbors from left and right.
            //Detonations are performed from left to right and all detonated numbers disappear. 
            //Finally print the sum of the remaining elements in the sequence.

            List<int> sequence = Console.ReadLine()
                                .Split(' ')
                                .Select(int.Parse)
                                .ToList();
            List<int> bombRow = Console.ReadLine()
                                .Split(' ')
                                .Select(int.Parse)
                                .ToList();
            int bombNumber = bombRow[0];
            int bombPower = bombRow[1];

            while (sequence.IndexOf(bombNumber)!=-1)
            {
                int startIndexBomb = sequence.IndexOf(bombNumber)-bombPower;
                int endIndexBomb = sequence.IndexOf(bombNumber) + bombPower;
                if (startIndexBomb<0)
                {
                    startIndexBomb = 0;
                }
                if (endIndexBomb>sequence.Count-1)
                {
                    endIndexBomb = sequence.Count - 1;
                }

                sequence.RemoveRange(startIndexBomb, endIndexBomb - startIndexBomb+1);

            }

            int result = sequence.Sum();
            Console.WriteLine(result);
        }
    }
}
