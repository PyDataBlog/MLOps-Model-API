using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Circle_Intersection
{
    public class Point
    {
        public double X { get; set; }

        public double Y { get; set; }

        public Point(int x, int y)
        {
            X = x;
            Y = y;
        }

        public static int CalculateDistance(Point p1, Point p2)
        {
            return (int)Math.Abs(Math.Sqrt(Math.Pow(p1.X + p1.Y, 2) + Math.Pow(p2.X + p2.Y, 2)));
        }
    }
}
