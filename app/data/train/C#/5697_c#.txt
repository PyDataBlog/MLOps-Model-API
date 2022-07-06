/*
 Create a class Person with two fields – name and age. Age can be left unspecified (may contain null value. Override ToString() to display the information of a person and if age is not specified – to say so.
 Write a program to test this functionality.
 */
namespace Problem_4.Person_class
{
    using System;
    public class StartProgram
    {
        public static void Main()
        {
            Person ivan = new Person();
            ivan.Name = "Ivan";
            ivan.Age = 50;
            Console.WriteLine("Printing ivan (ivan has setted age) ->");
            Console.WriteLine(ivan);
            Person pesho = new Person();
            pesho.Name = "Pesho";
            Console.WriteLine("Printing pesho (pesho hasnt setted age) ->");
            Console.WriteLine(pesho);
        }
    }
}
