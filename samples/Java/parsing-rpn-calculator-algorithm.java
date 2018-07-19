import java.util.LinkedList;

public class RPN{
	public static void evalRPN(String expr){
		String cleanExpr = cleanExpr(expr);
		LinkedList<Double> stack = new LinkedList<Double>();
		System.out.println("Input\tOperation\tStack after");
		for(String token:cleanExpr.split("\\s")){
			System.out.print(token+"\t");
			Double tokenNum = null;
			try{
				tokenNum = Double.parseDouble(token);
			}catch(NumberFormatException e){}
			if(tokenNum != null){
				System.out.print("Push\t\t");
				stack.push(Double.parseDouble(token+""));
			}else if(token.equals("*")){
				System.out.print("Operate\t\t");
				double secondOperand = stack.pop();
				double firstOperand = stack.pop();
				stack.push(firstOperand * secondOperand);
			}else if(token.equals("/")){
				System.out.print("Operate\t\t");
				double secondOperand = stack.pop();
				double firstOperand = stack.pop();
				stack.push(firstOperand / secondOperand);
			}else if(token.equals("-")){
				System.out.print("Operate\t\t");
				double secondOperand = stack.pop();
				double firstOperand = stack.pop();
				stack.push(firstOperand - secondOperand);
			}else if(token.equals("+")){
				System.out.print("Operate\t\t");
				double secondOperand = stack.pop();
				double firstOperand = stack.pop();
				stack.push(firstOperand + secondOperand);
			}else if(token.equals("^")){
				System.out.print("Operate\t\t");
				double secondOperand = stack.pop();
				double firstOperand = stack.pop();
				stack.push(Math.pow(firstOperand, secondOperand));
			}else{//just in case
				System.out.println("Error");
				return;
			}
			System.out.println(stack);
		}
		System.out.println("Final answer: " + stack.pop());
	}
	
	private static String cleanExpr(String expr){
		//remove all non-operators, non-whitespace, and non digit chars
		return expr.replaceAll("[^\\^\\*\\+\\-\\d/\\s]", "");
	}
	
	public static void main(String[] args){
		evalRPN("3 4 2 * 1 5 - 2 3 ^ ^ / +");
	}
}
