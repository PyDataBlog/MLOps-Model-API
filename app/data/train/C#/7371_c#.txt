using System.IO;
using System.Text.RegularExpressions;

namespace Advent_of_code.Day_4
{
    public class Runner
    {
        public void Run()
        {
            Part1();
            Part2();
        }

        private void Part1()
        {
            var sum = 0;
            using (var sr = new StreamReader("e:/Data/coding/Various/Advent of code 2016/Day 4/input.txt"))
            {
                string line;
                Room room;
                while ((line = sr.ReadLine()) != null)
                {
                    room = new Room(line);
                    sum += room.IsReal() ? room.Number : 0;
                }
            }

            System.Console.WriteLine($"Day 4, part 1: {sum}");
        }
        private void Part2()
        {
            int realId = 0;
            using (var sr = new StreamReader("e:/Data/coding/Various/Advent of code 2016/Day 4/input.txt"))
            {
                string line;
                while ((line = sr.ReadLine()) != null)
                {
                    var room = new Room(line);
                    if (room.DecryptName().Contains("northpole object storage"))
                    {
                        realId = room.Number;
                        break;
                    }
                }
            }

            System.Console.WriteLine($"Day 4, part 2: {realId}");
        }
    }
}