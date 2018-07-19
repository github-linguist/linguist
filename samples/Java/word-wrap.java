package rosettacode;

import java.util.StringTokenizer;

public class WordWrap
{
	int defaultLineWidth=80;
	int defaultSpaceWidth=1;
	void minNumLinesWrap(String text)
	{
		minNumLinesWrap(text,defaultLineWidth);
	}
	void minNumLinesWrap(String text,int LineWidth)
	{
		StringTokenizer st=new StringTokenizer(text);
		int SpaceLeft=LineWidth;
		int SpaceWidth=defaultSpaceWidth;
		while(st.hasMoreTokens())
		{
			String word=st.nextToken();
			if((word.length()+SpaceWidth)>SpaceLeft)
			{
				System.out.print("\n"+word+" ");
				SpaceLeft=LineWidth-word.length();
			}
			else
			{
				System.out.print(word+" ");
				SpaceLeft-=(word.length()+SpaceWidth);
			}
		}
	}
	public static void main(String[] args)
	{
		WordWrap now=new WordWrap();
		String wodehouse="Old Mr MacFarland (_said Henry_) started the place fifteen years ago. He was a widower with one son and what you might call half a daughter. That's to say, he had adopted her. Katie was her name, and she was the child of a dead friend of his. The son's name was Andy. A little freckled nipper he was when I first knew him--one of those silent kids that don't say much and have as much obstinacy in them as if they were mules. Many's the time, in them days, I've clumped him on the head and told him to do something; and he didn't run yelling to his pa, same as most kids would have done, but just said nothing and went on not doing whatever it was I had told him to do. That was the sort of disposition Andy had, and it grew on him. Why, when he came back from Oxford College the time the old man sent for him--what I'm going to tell you about soon--he had a jaw on him like the ram of a battleship. Katie was the kid for my money. I liked Katie. We all liked Katie.";
		System.out.println("DEFAULT:");
		now.minNumLinesWrap(wodehouse);
		System.out.println("\n\nLINEWIDTH=120");
		now.minNumLinesWrap(wodehouse,120);
	}

}
