namespace _7.MaxSequenceIncreasingElements
{
    using System;
    using System.Linq;

    /// Find and print the longest increasing sequence in 
    /// a given number string
    public class Program
    {
        public static void Main(string[] args)
        {
            int[] numberArray = Console.ReadLine().Trim()
                        .Split(' ')
                        .Select(int.Parse).ToArray();

            int startCurrentId = 0;
            int maxStartID = 0;
            int currentIncreaasingSeqLenght = 1;
            int maxIncreasingSeqLenght = 1;

            for (int i = 0; i < numberArray.Length - 1; i++)
            {
                if (numberArray[i + 1] > numberArray[i])
                {
                    currentIncreaasingSeqLenght++;
                    if (currentIncreaasingSeqLenght > maxIncreasingSeqLenght)
                    {
                        maxIncreasingSeqLenght = currentIncreaasingSeqLenght;
                        maxStartID = startCurrentId;
                    }
                }
                else
                {
                    if (currentIncreaasingSeqLenght > maxIncreasingSeqLenght)
                    {
                        maxIncreasingSeqLenght = currentIncreaasingSeqLenght;
                        currentIncreaasingSeqLenght = 1;   
                        maxStartID = startCurrentId;
                    }
                    startCurrentId = i + 1;
                    currentIncreaasingSeqLenght = 1;
                }
            }

            int[] resultArray = new int[maxIncreasingSeqLenght];
            for (int i = 0; i < resultArray.Length; i++)
            {
                resultArray[i] = numberArray[maxStartID + i];
            }
            Console.WriteLine(String.Join(" ", resultArray));
        }
    }
}
