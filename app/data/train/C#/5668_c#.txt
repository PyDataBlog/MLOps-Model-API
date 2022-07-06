namespace _1.CountWorkingDays
{
    using System;
    using System.Collections.Generic;
    using System.Globalization;

    /// Program that reads two dates in format dd-MM-yyyy
    /// and prints the number of working days between these
    /// two dates inclusive.
    public class Program
    {
        public static void Main()
        {
            DateTime startDate = DateTime.ParseExact(Console.ReadLine(), "dd-MM-yyyy", CultureInfo.InvariantCulture);
            DateTime endDate = DateTime.ParseExact(Console.ReadLine(), "dd-MM-yyyy", CultureInfo.InvariantCulture);

            List<DateTime> holidays = AddOfficialHolidays();
            int dayCounter = 0;
            
            for (DateTime currDate = startDate; currDate <= endDate; currDate = currDate.AddDays(1))
            {
                DateTime checkDate = new DateTime(2016, currDate.Month, currDate.Day);

                if (currDate.DayOfWeek != DayOfWeek.Saturday
                    && currDate.DayOfWeek != DayOfWeek.Sunday
                    && !holidays.Contains(checkDate))
                {
                    dayCounter++;
                }
            }

            Console.WriteLine(dayCounter);
        }

        public static List<DateTime> AddOfficialHolidays()
        {
            List<DateTime> holidays = new List<DateTime>();

            holidays.Add(DateTime.ParseExact("01-01-2016", "dd-MM-yyyy", CultureInfo.InvariantCulture));
            holidays.Add(DateTime.ParseExact("03-03-2016", "dd-MM-yyyy", CultureInfo.InvariantCulture));
            holidays.Add(DateTime.ParseExact("01-05-2016", "dd-MM-yyyy", CultureInfo.InvariantCulture));
            holidays.Add(DateTime.ParseExact("06-05-2016", "dd-MM-yyyy", CultureInfo.InvariantCulture));
            holidays.Add(DateTime.ParseExact("24-05-2016", "dd-MM-yyyy", CultureInfo.InvariantCulture));
            holidays.Add(DateTime.ParseExact("06-09-2016", "dd-MM-yyyy", CultureInfo.InvariantCulture));
            holidays.Add(DateTime.ParseExact("22-09-2016", "dd-MM-yyyy", CultureInfo.InvariantCulture));
            holidays.Add(DateTime.ParseExact("01-11-2016", "dd-MM-yyyy", CultureInfo.InvariantCulture));
            holidays.Add(DateTime.ParseExact("24-12-2016", "dd-MM-yyyy", CultureInfo.InvariantCulture));
            holidays.Add(DateTime.ParseExact("25-12-2016", "dd-MM-yyyy", CultureInfo.InvariantCulture));
            holidays.Add(DateTime.ParseExact("26-12-2016", "dd-MM-yyyy", CultureInfo.InvariantCulture));

            return holidays;
        }
    }
}
