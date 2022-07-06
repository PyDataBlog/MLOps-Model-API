using Hanafuda.OpenApi.Entities;
using Hanafuda.OpenApi.Enums;
using Hanafuda.OpenApi.Expressions;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Hanafuda.OpenApi.Extensions
{
    /// <summary>
    /// Extensions for Enumerable
    /// </summary>
    public static class EnumerableExtension
    {
        /// <summary>
        /// Return a Shuffled enumerable of this enumerable
        /// </summary>
        /// <typeparam name="T">T type</typeparam>
        /// <param name="enumerable">Enumerable extend</param>
        /// <returns>Shuffled enumerable</returns>
        public static IEnumerable<T> Shuffle<T>(this IEnumerable<T> enumerable)
        {
            return enumerable.OrderBy(x => Guid.NewGuid()).ToList();
        }

        /// <summary>
        /// Draw a random item
        /// </summary>
        /// <typeparam name="T">T type</typeparam>
        /// <param name="enumerable">Enumerable extend</param>
        /// <returns>Item draw</returns>
        public static T Draw<T>(this IEnumerable<T> enumerable)
        {
            return enumerable.Shuffle().First();
        }

        /// <summary>
        /// True if a card enumeration has a Teshi, otherwise false
        /// </summary>
        /// <param name="cards">IEnumarable extends</param>
        /// <param name="month">Month</param>
        /// <returns>True if a card enumeration has a Teshi, otherwise false</returns>
        public static bool HasTeshi(this IEnumerable<Card> cards, MonthEnum month)
        {
            return cards.Count(x => x.Month == month) == 4;
        }

        /// <summary>
        /// True if a card enumeration has a Kuttsuki, otherwise false
        /// </summary>
        /// <param name="cards">IEnumarable extends</param>
        /// <returns>True if a card enumeration has a Kuttsuki, otherwise false</returns>
        public static bool HasKuttsuki(this IEnumerable<Card> cards)
        {
            var cardArray = cards as Card[] ?? cards.ToArray();

            //4 month pair
            return cardArray.Select(x => x.Month).Distinct().Count(m => cardArray.Count(x => x.Month == m) == 2) == 4;
        }

        /// <summary>
        /// Return all Yakus find in a collection of cards.
        /// </summary>
        /// <param name="cards">Cards</param>
        /// <param name="currentMonth">Current month</param>
        /// <returns>Enumerable of YakuEnum</returns>
        public static IEnumerable<YakuEnum> GetYakus(this IEnumerable<Card> cards, MonthEnum currentMonth)
        {
            var cardArray = cards as Card[] ?? cards.ToArray();
            var yakus = new List<YakuEnum>();

            //SANKO
            if (cardArray.Count(YakuLambda.NoRainMan) == 3)
                yakus.Add(YakuEnum.Sanko);

            //SHIKO
            if (cardArray.Count(YakuLambda.NoRainMan) == 4)
            {
                yakus.Remove(YakuEnum.Sanko);
                yakus.Add(YakuEnum.Shiko);
            }

            //AME_SHIKO
            if (cardArray.Count(YakuLambda.NoRainMan) == 3 && cardArray.Any(YakuLambda.RainMan))
            {
                yakus.Remove(YakuEnum.Sanko);
                yakus.Add(YakuEnum.AmeShiko);
            }

            //GOKO
            if (cardArray.Count(YakuLambda.Special) == 5)
            {
                yakus.Remove(YakuEnum.Shiko);
                yakus.Add(YakuEnum.Goko);
            }

            //INOSHIKACHO
            if (cardArray.Count(YakuLambda.Inoshikacho) == 3)
                yakus.Add(YakuEnum.Inoshikacho);

            //TANE
            if (cardArray.Count(YakuLambda.Animal) >= 5)
                yakus.Add(YakuEnum.Tane);

            //AKATAN
            if (cardArray.Count(YakuLambda.Poem) == 3)
                yakus.Add(YakuEnum.Akatan);

            //AOTAN
            if (cardArray.Count(YakuLambda.Blue) == 3)
                yakus.Add(YakuEnum.Aotan);

            //AKATAN_AOTAN_NO_CHOFUKU
            if(yakus.Contains(YakuEnum.Akatan) && yakus.Contains(YakuEnum.Aotan))
                yakus.Add(YakuEnum.AkatanAotanNoChofuku);

            //TAN
            if (cardArray.Count(YakuLambda.Ribbon) >= 5)
                yakus.Add(YakuEnum.Tan);

            //TSUKIMI
            if (cardArray.Any(YakuLambda.Moon) && cardArray.Any(YakuLambda.SakeCup))
                yakus.Add(YakuEnum.TsukimiZake);

            //HANAMI
            if (cardArray.Any(YakuLambda.Curtain) && cardArray.Any(YakuLambda.SakeCup))
                yakus.Add(YakuEnum.HanamiZake);

            //KASU
            if (cardArray.Count(YakuLambda.Normal) >= 10)
                yakus.Add(YakuEnum.Kasu);

            //BAKE_FUDA
            if (cardArray.Count(YakuLambda.Normal) == 9 && cardArray.Any(YakuLambda.SakeCup))
            {
                yakus.Add(YakuEnum.BakeFuda);
            }

            //TSUKI_FUDA
            if (cardArray.Count(x => x.Month == currentMonth) == 4)
                yakus.Add(YakuEnum.TsukiFuda);

            return yakus;
        }
    }
}
