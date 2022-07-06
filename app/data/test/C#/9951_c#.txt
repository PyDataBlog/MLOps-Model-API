using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace ExerProblems
{
    class _13SrabskoUnleashed
    {
        public static void Run()
        {
            Dictionary<string, Dictionary<string, long>> events = new Dictionary<string, Dictionary<string, long>>();
            while (true)
            {
                string inputLine = Console.ReadLine();

                if (inputLine == "End")
                {
                    break;
                }
                string[] splitData = inputLine.Split(new string[] { " @" }, StringSplitOptions.RemoveEmptyEntries).ToArray();
                if (splitData.Length < 2)
                {
                    continue;
                }


                string singer = splitData[0].Trim();
                string[] madaFaka = splitData[1].Trim().Split(' ').ToArray();
                int vCount = madaFaka.Length;
                if (madaFaka.Length < 3)
                {
                    continue;
                }
                int tryLong;
                int tryLongSecond;

                bool res = int.TryParse(madaFaka[vCount - 1], out tryLong);
                bool ressec = int.TryParse(madaFaka[vCount - 2], out tryLongSecond);

                if (!res || !ressec)
                {
                    continue;
                }

                int price = tryLong;
                int ticketCount = tryLongSecond;
                string venue = string.Empty;

                for (int i = 0; i < madaFaka.Length - 2; i++)
                {
                    venue += " " + madaFaka[i];
                }

                venue.Trim();
                if (!events.ContainsKey(venue))
                {
                    events.Add(venue, new Dictionary<string, long>());
                    events[venue].Add(singer, (ticketCount * price));
                }
                else
                {
                    if (!events[venue].ContainsKey(singer))
                    {
                        events[venue].Add(singer, (ticketCount * price));

                    }
                    else
                    {
                        events[venue][singer] += ticketCount * price;
                    }
                }

            }

            foreach (var ev in events)
            {
                Console.WriteLine(ev.Key.TrimStart('@'));
                foreach (var singer in ev.Value.OrderByDescending(b => b.Value))
                {
                    Console.WriteLine($"#  {singer.Key} -> {singer.Value}");
                }

            }

        }
    }
}

