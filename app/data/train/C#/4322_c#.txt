// Write a program that prints to the console which day of the week is today.
// Use System.DateTime.

using System;


class DayOfTheWeek
{
    static void WhatDayOfTheWeekIs()
    {
        DateTime dt = DateTime.Now;
        
        Console.WriteLine("The day today is : {0}", dt.DayOfWeek);

    }

    static void Main()
    {
        WhatDayOfTheWeekIs();
    }
}

