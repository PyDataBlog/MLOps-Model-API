using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace BankMachine
{
    public class AccountDatabase
    {
        public List<Person> People { get; set; }

        public AccountDatabase()
        {
            People = new List<Person>(); 
            // People.Add (new Person ("Yuriy"))
        }

        public void initDefault()
        {
            var a0 = new Account(2345234, AccountType.Chequings);
            a0.deposit(400.00f, DateTime.Parse("June 8 2015"));
            a0.withdraw(20f, DateTime.Parse("June 12 2015"));

            var a1 = new Account(9954, AccountType.Savings);
            a1.deposit(63.32f, DateTime.Parse("July 4 2015"));
            a1.deposit(600.00f, DateTime.Parse("Oct 9 2015"));

            var p0 = new Person("Yuriy", "1234", "5678");
            p0.addAccount(a0); p0.addAccount(a1);
            People.Add(p0);
        }

        public void addPerson (Person a) { People.Add(a); }

        public Person lookupAccntNum (string num) {
            return People.Find(p => p.AccountNumber == num);
        }

    }
}
 