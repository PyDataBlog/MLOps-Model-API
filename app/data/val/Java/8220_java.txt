import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

import config.io.FolderManager;
import config.misc.GlobalIgnoreList;
import config.misc.GlobalSettings;

public class KspModuleEditor {

	public static void main(String[] args){

		try {
			Scanner scanner = new Scanner(new File("GlobalSettings.txt"));
			while(scanner.hasNext())
			{
				String inLine = scanner.next().replaceAll("\\s", "");
				if(inLine.startsWith("AddMechJeb="))
				{
					GlobalSettings.addMechjeb = Boolean.parseBoolean(inLine.replace("AddMechJeb=", ""));
				}
				else if(inLine.startsWith("AddProtractor="))
				{
					GlobalSettings.addProtractor = Boolean.parseBoolean(inLine.replace("AddProtractor=", ""));
				}
				else if(inLine.startsWith("AddDeadlyReentry="))
				{
					GlobalSettings.addDeadlyReentry = Boolean.parseBoolean(inLine.replace("AddDeadlyReentry=", ""));
				}
			}
			
			scanner.close();
		} catch (FileNotFoundException e) {
			// TODO proper error handling
			// we'll just keep the defaults for now...
			e.printStackTrace();
		}
		
		
		try{
			Scanner scanner = new Scanner(new File("FilesAndFoldersToIgnore.txt"));
			boolean fileMode = true;
			while(scanner.hasNext())
			{
				String inLine = scanner.next().trim();
				if(inLine.equals(""))
				{
					continue;
				}
				else if(inLine.equals("#Folders"))
				{
					fileMode = false;
				}
				else if(inLine.equals("#Files"))
				{
					fileMode = true;
				}
				else if(fileMode)
				{
					GlobalIgnoreList.filesToIgnore.add(inLine);
				}
				else
				{
					GlobalIgnoreList.foldersToIgnore.add(inLine);
				}
				
			}
			scanner.close();
		} catch (FileNotFoundException e) {
			// TODO proper error handling
			e.printStackTrace();
		}
		
		FolderManager.applyChanges();
		
		
	}
	
}
