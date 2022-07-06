namespace ClassChef.Models
{
    using System;

    using Contracts;
    using Factory;

    public class Kitchen
    {
        public void Cook()
        {
            Factory factory = new Factory();
            var potato = factory.GetPotato();
            var carrot = factory.GetCarrot();

            Bowl bowl = new Bowl();
            this.Peel(potato);
            this.Peel(carrot);
            bowl = this.GetBowl();
            this.Cut(potato);
            this.Cut(carrot);
            bowl.Add(carrot);
            bowl.Add(potato);


            //Task 2 
            //if (potato != null)
            //{
            //    if (!potato.HasNotBeenPeeled && !potato.IsRotten)
            //    {
            //        Cook(potato);
            //    }
            //}

            //if ((x >= MIN_X && x =< MAX_X) && 
            //   (MAX_Y >= y && MIN_Y <= y) && 
            //   !isViseted))
            //{
            //    VisitCell();
            //}

            //Task 3

            //var isFound = false;

            //for (int i = 0; i < array.Length; i++)
            //{
            //    if (i % 10 == 0)
            //    {
            //        Console.WriteLine(array[i]);

            //        if (array[i] == expectedValue)
            //        {
            //           isFound = true;
            //        }
            //    }

            //    else
            //    {
            //        Console.WriteLine(array[i]);
            //    }
            //}
            //// More code here
            //if (isFound)
            //{
            //    Console.WriteLine("Value Found");
            //}


        }

        public void Cut(IVegetable potato)
        {
            //...
        }

        private Bowl GetBowl()
        {
            return new Bowl(); 
        }

        private void Peel(IVegetable vegitable)
        {
            //some logic
        }
    }
}
