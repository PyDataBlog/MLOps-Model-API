using System;
using System.Collections.Generic;
using System.Linq;

class Program
{
    static void Main()
    {
        List<int> nums =
             Console.ReadLine().Split(' ')
             .Select(int.Parse)
             .ToList();

        int[] options = Console.ReadLine()
            .Split(' ')
            .Select(int.Parse)
            .ToArray();


        for (int i = 0; i < nums.Count; i++)
        {
            //if bomb number is found:
            if (options[0] == nums[i])
            {
                if ((i + 1 - options[1] >= 0) && i <= nums.Count - 1 - options[1])
                {
                    //remove range from i - options[1] to i + options[1]
                    nums.RemoveRange(i - options[1], (2 * options[1]) + 1);

                }
                else if ((i + options[1]) >= nums.Count && i <= options[1])
                {
                    nums.RemoveRange(i - options[1], options[1] + 1 + nums.Count - i);
                }
                else if (true)
                {

                }
            }
        }

        //Console.WriteLine(string.Join(" ", nums));
        int sum = nums.Sum();
        Console.WriteLine(sum);
    }
}
