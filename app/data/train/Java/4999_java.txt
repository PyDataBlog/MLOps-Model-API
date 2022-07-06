public class Schuler
{
	private String name;
	private int test1;
	private int test2;
	private int test3;
	
	public Schuler()
	{
		name = "";
		test1 = 0;
		test2 = 0;
		test3 = 0;
	}
	public void setName(String nm)
	{
		name = nm;
	}
	public String getName()
	{
		return name;
	}
	public void setPunkte(int i, int punkte)
	{
		if (i==1) test1=punkte;
		if (i==2) test2=punkte;
		if (i==3) test3=punkte;
	}
	public int getPunkte(int i)
	{
		if (i==1) return test1;
		if (i==2) return test2;
		if (i==3) return test3;
		return -1;
	}	
	public int getAverage()
	{
		return (int)Math.round((test1+test2+test3)/3.0);
	}
	public String toString()
	{
		return "Name: "+name+"\nTest 1: "+test1+"\nTest 2: "+test2+"\nTest 3: "+test3+"\nAverage: " +getAverage();
	}
}
