using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;


class Program
{
    static void Main()
    {
        int cakesWanted = int.Parse(Console.ReadLine());
        double flourNeededFor1Cake = double.Parse(Console.ReadLine());        
        double flourAvailable = double.Parse(Console.ReadLine());
        double truffelAvailable = double.Parse(Console.ReadLine());
        double priceForTruffel = double.Parse(Console.ReadLine());

        double cakes = Math.Floor(flourAvailable / flourNeededFor1Cake);
        double totalTruffelPrice = truffelAvailable * priceForTruffel;
        double cakePrice = (totalTruffelPrice / cakesWanted) * 1.25;

        if (cakes < cakesWanted)
        {
            Console.WriteLine("Can make only {0} cakes, need {1:f2} kg more flour", cakes, cakesWanted * flourNeededFor1Cake - flourAvailable);
        }
        else if (cakes > cakesWanted)
        {
            Console.WriteLine("All products available, price of a cake: {0:f2}", cakePrice);
        }
    }
}

