#include "person.h"

Person::Person()
{
    hunger=0;
    toilet=0;
    WaitForDo=0;
}



void Person::AddToDo(Deal todo, float time)
{
    ToDoList.append(qMakePair(todo,time));
    if (WaitForDo==0) WaitForDo=time;
}

void Person::CheckDeal()
{
    WaitForDo-=1;
    if (WaitForDo<0)
    {
        if (!ToDoList.isEmpty())
        {
           ToDoList.removeFirst();
           while(true)
           {
               if((WaitForDo+ToDoList.at(0).second)>0) break;
               if (!ToDoList.isEmpty()) ToDoList.removeFirst();
           }
        }
        else WaitForDo=0;
    }
}

Deal::Deal()
{
    name="noname";
    timeToDeal=0;
}

Deal::Deal(int time, QString n)
{
    name=n;
    timeToDeal=time;
}

bool Deal::Execution()
{
    return true;
}

Deal Person::Eat(float NutritionalValue, float timeToEat)
{
    hunger-=NutritionalValue;
    Deal noname(1,"n");
    return noname;
}
