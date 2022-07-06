using System;

class BonusScore
{
    static void Main()
    {
        Console.Write("Enter score of the student: ");
        int score = int.Parse(Console.ReadLine());

        if (score > 0 && score <= 3)
        {
            Console.WriteLine(score * 10);
        }
        else if (score > 3 && score <= 6)
        {
            Console.WriteLine(score * 100);
        }
        else if (score > 6 && score <= 9)
        {
            Console.WriteLine(score * 1000);
        }
        else if(score == 0 || score >= 9)
        {
            Console.WriteLine("Invalid score");
        }
    }
}
