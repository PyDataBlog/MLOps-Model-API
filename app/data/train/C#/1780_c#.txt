namespace _8.ArraySymmetry
{
    using System;
    using System.Linq;

    public class ArraySymmetry
    {
        public static void Main()
        {
            var words = Console.ReadLine().Split(' ').ToArray();

            var isSymmetric = true;

            for (int i = 0; i < words.Length / 2; i++)
            {
                var firstPart = words[i];
                var secondPart = words[words.Length - 1 - i];

                if (firstPart != secondPart)
                {
                    isSymmetric = false;
                }
            }

            if (isSymmetric)
                Console.WriteLine("Yes");
            else
                Console.WriteLine("No");

        }
    }
}
