//Draw at the console a filled square of size n like in the example:
//--------
//-\/\/\/-
//-\/\/\/-
//--------



namespace Draw_a_Filled_Square
{
    using System;

    class DrawAFilledSquare
    {
        static void Main()
        {
            int n = int.Parse(Console.ReadLine());
            PrintHeaderRow(n);

            for (int i = 0; i < n-2; i++)
            {
                PrintBody(n);
            }

            PrintHeaderRow(n);

        }

        private static void PrintBody(int n)
        {
            Console.Write('-');

            for (int i = 1; i < n; i++)
            {
                Console.Write("\\/");
            }

            Console.WriteLine('-');

        }

        static void PrintHeaderRow(int n)
        {
            Console.WriteLine(new string('-', 2 * n));
        }
    }
}
