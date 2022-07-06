import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

public class ReadSavedData 
{

	public static void StartRead() throws IOException
	{
		FileReader file = new FileReader("C:/Users/Public/Documents/SavedData.txt");
		BufferedReader reader = new BufferedReader(file);
		
		String text = "";
		String line = reader.readLine();
		while (line != null)
		{
			text += line;
			line = reader.readLine();
		}
		reader.close();
		System.out.println(text);
		
	}
	
}