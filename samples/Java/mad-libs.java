import java.util.Map;
import java.util.HashMap;
import java.util.Scanner;
import java.util.StringTokenizer;

public class MadLibs
{
	public static void main(String[] args)
	{
		Scanner s=new Scanner(System.in);
		String line;
		StringBuffer storybuffer=new StringBuffer();
		
		//Accept lines until empty line is entered
		while(!(line=s.nextLine()).isEmpty())
			storybuffer.append(" "+line);
		
		//Remove first space
		storybuffer.delete(0, 1);
		String story=storybuffer.toString();
		//Split
		StringTokenizer str=new StringTokenizer(story);
		String word;
		StringBuffer finalstory=new StringBuffer();
		
		//Store added elements
		Map<String,String> hash=new HashMap<String,String>();
		while(str.hasMoreTokens())
		{
			word=str.nextToken();
			if(word.contains("<"))
			{
				String add="";
				//Element prompt could be more than one word
				if(!word.contains(">"))
				{
					//Build multi-word prompt
					String phrase="";
					do{
						phrase+=word+" ";
					}while(!(word=str.nextToken()).contains(">"));
					word=phrase+word;
				}
				//Account for element placeholder being immediately followed by . or , or whatever.
				if(word.charAt(word.length()-1)!='>')
					add=word.substring(word.lastIndexOf('>')+1);
				
				//Store id of element in hash table
				String id=word.substring(0,word.lastIndexOf('>')+1);
				String value;
				
				if(!hash.containsKey(id))
				{
					//New element
					System.out.println("Enter a "+ id);
					value=s.nextLine()+add;
					hash.put(id, value);
				}
				//Previously entered element
				else
					value=hash.get(id);
				word=value;
			}
			finalstory.append(word+" ");
		}
		System.out.println(finalstory.toString());
		s.close();
	}
}
