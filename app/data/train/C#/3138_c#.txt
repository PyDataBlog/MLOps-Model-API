namespace ArmyOfCreatures.Extended.Specialties
{
    using System;
    using System.Globalization;

    using ArmyOfCreatures.Logic.Specialties;

    public class DoubleDamage : Specialty
    {
        private int rounds;

        public DoubleDamage(int rounds)
        {
            if (rounds <= 0)
            {
                throw new ArgumentOutOfRangeException("rounds", "The number of rounds should be greater than 0");
            }

            if (rounds > 10)
            {
                throw new ArgumentOutOfRangeException("rounds", "The number of rounds should be lesser than 11");
            }

            this.rounds = rounds;
        }

        public override decimal ChangeDamageWhenAttacking(
            Logic.Battles.ICreaturesInBattle attackerWithSpecialty,
            Logic.Battles.ICreaturesInBattle defender,
            decimal currentDamage)
        {
            if (attackerWithSpecialty == null)
            {
                throw new ArgumentNullException("attackerWithSpecialty");
            }

            if (defender == null)
            {
                throw new ArgumentNullException("defender");
            }

            return currentDamage * 2;
        }

        public override string ToString()
        {
            return string.Format(CultureInfo.InvariantCulture, "{0}({1})", base.ToString(), this.rounds);
        }
    }
}
