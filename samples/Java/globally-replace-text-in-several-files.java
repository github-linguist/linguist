import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;

public class GlobalReplace
{
	public static void main(String[] args)
	{
		//Enter names of the files here
		String[] filename={"foobar0.txt","foobar1.txt","foobar2.txt","foobar3.txt","foobar4.txt"};
		//Enter text to replace here
		String from="Goodbye London!";
		//Enter replacing text here
		String to="Hello New York!";
		
		GlobalReplace now=new GlobalReplace();
		for(int i=0;i<filename.length;i++)
			now.delete(filename[i],from,to);
	}
	void delete(String filename, String from, String to)
	{
		try
		{
			BufferedReader br=new BufferedReader(new FileReader(filename));
			
			//String buffer to store contents of the file
			StringBuffer sb=new StringBuffer("");
			
			String line;
			
			while((line=br.readLine())!=null)
			{
				//If from appears, replace it
				if(line.contains(from))
					sb.append(line.replace(from, to)+"\n");
				else
					sb.append(line+"\n");
				
			}
			br.close();
			
			FileWriter fw=new FileWriter(new File(filename));
			//Write entire string buffer into the file
			fw.write(sb.toString());
			fw.close();
		}
		catch (Exception e)
		{
			System.out.println("Something went horribly wrong: "+e.getMessage());
		}
	}
}
