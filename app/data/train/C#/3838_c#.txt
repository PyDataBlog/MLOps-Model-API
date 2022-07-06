namespace TeamElderberryProject
{
    using System;
    using System.Windows.Forms;

    public struct TransactionData
    {
        private decimal amount;
        private DateTime date;

        public TransactionData(decimal amount, DateTime date)
            : this()
        {
            this.Amount = amount;
            this.Date = date;
        }

        public decimal Amount
        {
            get
            {
                return this.amount;
            }

            private set
            {
                this.amount = value;
            }
        }

        public DateTime Date
        {
            get
            {
                return this.date;
            }

            private set
            {
                this.date = value;
            }
        }
    }
}