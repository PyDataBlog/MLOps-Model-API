using System;

class Disk
{
    static void Main()
    {
        int n = int.Parse(Console.ReadLine());
        int r = int.Parse(Console.ReadLine());

        int diskCenterRow = n / 2;
        int diskCenterCol = n / 2;

        for (int row = 0; row < n; row++)
        {
            for (int col = 0; col < n; col++)
            {
                int deltaX = col - diskCenterCol;
                int deltaY = row - diskCenterRow;
                double distanceFromCenter = Math.Sqrt(deltaX * deltaX + deltaY * deltaY);
                bool isWithinCircle = distanceFromCenter <= r;

                if (isWithinCircle)
                {
                    Console.Write("*");
                }
                else
                {
                    Console.Write(".");
                }
                
            }
            Console.WriteLine();
        }
    }
}
