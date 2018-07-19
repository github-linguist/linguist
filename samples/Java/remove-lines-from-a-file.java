import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.FileWriter;

public class RemoveLines
{
	public static void main(String[] args)
	{
		//Enter name of the file here
		String filename="foobar.txt";
		//Enter starting line here
		int startline=1;
		//Enter number of lines here.
		int numlines=2;
		
		RemoveLines now=new RemoveLines();
		now.delete(filename,startline,numlines);
	}
	void delete(String filename, int startline, int numlines)
	{
		try
		{
			BufferedReader br=new BufferedReader(new FileReader(filename));
			
			//String buffer to store contents of the file
			StringBuffer sb=new StringBuffer("");
			
			//Keep track of the line number
			int linenumber=1;
			String line;
			
			while((line=br.readLine())!=null)
			{
				//Store each valid line in the string buffer
				if(linenumber<startline||linenumber>=startline+numlines)
					sb.append(line+"\n");
				linenumber++;
			}
			if(startline+numlines>linenumber)
				System.out.println("End of file reached.");
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
