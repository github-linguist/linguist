public class Logic{
	public static enum Trit{
		TRUE, MAYBE, FALSE;
		
		public Trit and(Trit other){
			if(this == TRUE){
				return other;
			}else if(this == MAYBE){
				return (other == FALSE) ? FALSE : MAYBE;
			}else{
				return FALSE;
			}
		}
		
		public Trit or(Trit other){
			if(this == TRUE){
				return TRUE;
			}else if(this == MAYBE){
				return (other == TRUE) ? TRUE : MAYBE;
			}else{
				return other;
			}
		}
		
		public Trit tIf(Trit other){
			if(this == TRUE){
				return other;
			}else if(this == MAYBE){
				return (other == TRUE) ? TRUE : MAYBE;
			}else{
				return TRUE;
			}
		}
		
		public Trit not(){
			if(this == TRUE){
				return FALSE;
			}else if(this == MAYBE){
				return MAYBE;
			}else{
				return TRUE;
			}
		}
		
		public Trit equals(Trit other){
			if(this == TRUE){
				return other;
			}else if(this == MAYBE){
				return MAYBE;
			}else{
				return other.not();
			}
		}
	}
	public static void main(String[] args){
		for(Trit a:Trit.values()){
			System.out.println("not " + a + ": " + a.not());
		}
		for(Trit a:Trit.values()){
			for(Trit b:Trit.values()){
				System.out.println(a+" and "+b+": "+a.and(b)+
						"\t "+a+" or "+b+": "+a.or(b)+
						"\t "+a+" implies "+b+": "+a.tIf(b)+
						"\t "+a+" = "+b+": "+a.equals(b));
			}
		}
	}
}
