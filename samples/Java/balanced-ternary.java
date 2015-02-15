/*
 * Test case
 * With balanced ternaries a from string "+-0++0+", b from native integer -436, c "+-++-":
 * Write out a, b and c in decimal notation;
 * Calculate a × (b − c), write out the result in both ternary and decimal notations.
 */
public class BalancedTernary
{
	public static void main(String[] args)
	{
 		BTernary a=new BTernary("+-0++0+");
		BTernary b=new BTernary(-436);
		BTernary c=new BTernary("+-++-");
		
		System.out.println("a="+a.intValue());
		System.out.println("b="+b.intValue());
		System.out.println("c="+c.intValue());
		System.out.println();
		
		//result=a*(b-c)
		BTernary result=a.mul(b.sub(c));
		
		System.out.println("result= "+result+" "+result.intValue());
	}
	
	
	public static class BTernary
	{
		String value;
		public BTernary(String s)
		{
			int i=0;
			while(s.charAt(i)=='0')
				i++;
			this.value=s.substring(i);
		}
		public BTernary(int v)
		{
			this.value="";
			this.value=convertToBT(v);
		}
		
		private String convertToBT(int v)
		{
			if(v<0)
				return flip(convertToBT(-v));
			if(v==0)
				return "";
			int rem=mod3(v);
			if(rem==0)
				return convertToBT(v/3)+"0";
			if(rem==1)
				return convertToBT(v/3)+"+";
			if(rem==2)
				return convertToBT((v+1)/3)+"-";
			return "You can't see me";
		}
		private String flip(String s)
		{
			String flip="";
			for(int i=0;i<s.length();i++)
			{
				if(s.charAt(i)=='+')
					flip+='-';
				else if(s.charAt(i)=='-')
					flip+='+';
				else
					flip+='0';
			}
			return flip;
		}
		private int mod3(int v)
		{
			if(v>0)
				return v%3;
			v=v%3;
			return (v+3)%3;
		}
		
		public int intValue()
		{
			int sum=0;
			String s=this.value;
			for(int i=0;i<s.length();i++)
			{
				char c=s.charAt(s.length()-i-1);
				int dig=0;
				if(c=='+')
					dig=1;
				else if(c=='-')
					dig=-1;
				sum+=dig*Math.pow(3, i);
			}
			return sum;
		}
		
		
		public BTernary add(BTernary that)
		{
			String a=this.value;
			String b=that.value;
			
			String longer=a.length()>b.length()?a:b;
			String shorter=a.length()>b.length()?b:a;
			
			while(shorter.length()<longer.length())
				shorter=0+shorter;
			
			a=longer;
			b=shorter;
			
			char carry='0';
			String sum="";
			for(int i=0;i<a.length();i++)
			{
				int place=a.length()-i-1;
				String digisum=addDigits(a.charAt(place),b.charAt(place),carry);
				if(digisum.length()!=1)
					carry=digisum.charAt(0);
				else
					carry='0';
				sum=digisum.charAt(digisum.length()-1)+sum;
			}
			sum=carry+sum;
			
			return new BTernary(sum);
		}
		private String addDigits(char a,char b,char carry)
		{
			String sum1=addDigits(a,b);
			String sum2=addDigits(sum1.charAt(sum1.length()-1),carry);
			//System.out.println(carry+" "+sum1+" "+sum2);
			if(sum1.length()==1)
				return sum2;
			if(sum2.length()==1)
				return sum1.charAt(0)+sum2;
			return sum1.charAt(0)+"";
		}
		private String addDigits(char a,char b)
		{
			String sum="";
			if(a=='0')
				sum=b+"";
			else if (b=='0')
				sum=a+"";
			else if(a=='+')
			{
				if(b=='+')
					sum="+-";
				else
					sum="0";
			}
			else
			{
				if(b=='+')
					sum="0";
				else
					sum="-+";
			}
			return sum;
		}
		
		public BTernary neg()
		{
			return new BTernary(flip(this.value));
		}
		
		public BTernary sub(BTernary that)
		{
			return this.add(that.neg());
		}
		
		public BTernary mul(BTernary that)
		{
			BTernary one=new BTernary(1);
			BTernary zero=new BTernary(0);
			BTernary mul=new BTernary(0);
			
			int flipflag=0;
			if(that.compareTo(zero)==-1)
			{
				that=that.neg();
				flipflag=1;
			}
			for(BTernary i=new BTernary(1);i.compareTo(that)<1;i=i.add(one))
				mul=mul.add(this);
			
			if(flipflag==1)
				mul=mul.neg();
			return mul;
		}
		
		public boolean equals(BTernary that)
		{
			return this.value.equals(that.value);
		}
		public int compareTo(BTernary that)
		{
			if(this.intValue()>that.intValue())
				return 1;
			else if(this.equals(that))
				return 0;
			 return -1;
		}
		
		public String toString()
		{
			return value;
		}
	}
}
