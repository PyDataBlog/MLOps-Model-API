using System;
using System.Collections.Generic;
using System.Text;
using System.Linq;


public class Family
{
    private List<Person> familyList;
   

    public List<Person> Names
    {
        get { return this.familyList; }
        set { this.familyList = value; }
    }

    
    public Family()
    {
        this.familyList = new List<Person>();
    }

    public void AddMember(Person name)
    {
        familyList.Add(name);
    }

    public Person GetOlderMember()
    {
        var result = Names.OrderByDescending(x => x.Age).FirstOrDefault();
       
        return result;
    }


}

