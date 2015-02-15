public class VersCheck {
	public static void main(String[] args) {
		String vers = System.getProperty("java.version");
		vers = vers.substring(0,vers.indexOf('.')) + "." +  //some String fiddling to get the version number into a usable form
			vers.substring(vers.indexOf('.')+1,vers.lastIndexOf('.'));
		if(Double.parseDouble(vers) >= 1.5){
			System.out.println("YAY!");
		}else{
			System.err.println("Must use Java >=1.5");
		}
	}
}
