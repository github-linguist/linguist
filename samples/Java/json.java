import com.google.gson.Gson;

public class JsonExample {

	public static void main(String[] args) {
		Gson gson = new Gson();
		String json = "{ \"foo\": 1, \"bar\": [ \"10\", \"apples\"] }";
		
		MyJsonObject obj = gson.fromJson(json, MyJsonObject.class);
		
		System.out.println(obj.getFoo());

		for(String bar : obj.getBar()) {
			System.out.println(bar);
		}
		
		obj = new MyJsonObject(2, new String[] { "20", "oranges" });
		json = gson.toJson(obj);
		
		System.out.println(json);
	}
	
}

class MyJsonObject {
	
	private int foo;
	private String[] bar;
	
	public MyJsonObject(int foo, String[] bar) {
		this.foo = foo;
		this.bar = bar;
	}
	
	public int getFoo() {
		return foo;
	}
	
	public String[] getBar() {
		return bar;
	}
	
}
