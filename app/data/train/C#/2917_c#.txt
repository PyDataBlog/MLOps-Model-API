using System;

namespace NullValuesArithmetic
{
    class NullValuesArithmetic

             //Problem 12. Null Values Arithmetic
             //Create a program that assigns null values to an integer and to a double variable.
             //Try to print these variables at the console.
             //Try to add some number or the null literal to these variables and print the result.

    {
        static void Main()
        {
            int? ValueInteger = null;
            double? valueDouble = null;
            bool check = (ValueInteger == null && valueDouble == null);

            Console.WriteLine("Nullable value of integer number is:" + ValueInteger);
            Console.WriteLine("Nullable value of double is: " + valueDouble);
            Console.WriteLine("Is there a null check = " + check);

            int secValueInteger = 7;
            double secValueDouble = 9.5;

            Console.WriteLine("The value of Integer after been modified: " + secValueInteger);
            Console.WriteLine("The value of Double after been modified: " + secValueDouble);

        }
    }
}
