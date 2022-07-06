using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace _2.Rotate_and_Sum
{
    class Program
    {
        static void Main(string[] args)
        {
            var array = Console.ReadLine().Split(' ').Select(int.Parse).ToArray();
            var rotation = int.Parse(Console.ReadLine());

            int[] sum = new int[array.Length];

            for (int i = 0; i < rotation; i++)
            {
                RotationArray(array, sum);
            }

            
            Console.WriteLine(string.Join(" ",sum));

              

        }

        public static void RotationArray (int[] input, int[] sum)
        {
            var last = input.Length - 1;
            for (var i = 0; i < input.Length - 1; i += 1)
            {
                input[i] ^= input[last];
                input[last] ^= input[i];
                input[i] ^= input[last];
            }
            for (int i = 0; i < input.Length; i++)
            {
                sum[i] += input[i];
            }
        }
    }
}
