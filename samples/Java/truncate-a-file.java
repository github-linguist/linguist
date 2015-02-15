import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.channels.FileChannel;

public class TruncFile {
	public static void main(String[] args) throws IOException{
		if(args.length < 2){
			System.out.println("Usage: java TruncFile fileName newSize");
			return;
		}
		//turn on "append" so it doesn't clear the file
		FileChannel outChan = new FileOutputStream(args[0], true).getChannel();
		long newSize = Long.parseLong(args[1]);
		outChan.truncate(newSize);
		outChan.close();
	}
}
