import java.io.*;

public class TestObjectStream {
	public static void main(String[] args) {
		T t=new T();
		t.d=100;
		try {
			FileOutputStream fos=new FileOutputStream("/home/wusicheng/workspace/the.txt");
			ObjectOutputStream ops=new ObjectOutputStream(fos);
			ops.writeObject(t);
			ops.flush();
			ops.close();
			
			FileInputStream fis=new FileInputStream("/home/wusicheng/workspace/the.txt");
			ObjectInputStream oos=new ObjectInputStream(fis);
			T t1=(T) oos.readObject();
			System.out.println(t1.d);
			System.out.println(t1.x);
			System.out.println(t1.y);
			System.out.println(t1.z);
		} catch (FileNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
}
class T implements Serializable{
		int x=10;
		int y=19;
		int z=103;
		double d=10.9;
}
