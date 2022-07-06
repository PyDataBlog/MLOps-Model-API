using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Net;

public class Point
{
    public double X { get; set; }

    public double Y { get; set; }
}
class Program
{
    static void Main()
    {
        var pointOne = ReadPointCoordinates();
        var pointTwo = ReadPointCoordinates();
        Console.WriteLine(CalculateDistance(pointOne, pointTwo));
    }

    public static Point ReadPointCoordinates()
    {
        var coordinates = Console.ReadLine().Split().Select(double.Parse).ToArray();
        var p = new Point() { X = coordinates[0], Y = coordinates[1] };
        return p;
    }

    public static double CalculateDistance(Point pointOne, Point pointTwo)
    {
        var a = pointOne.X - pointTwo.X;
        var b = pointOne.Y - pointTwo.Y;
        var sum2 = a * a + b * b;
        var distance = Math.Sqrt(sum2);
        return distance;
    }
}

