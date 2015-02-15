import java.nio.file.Paths;
//... other class code here
public static boolean isEmptyDir(String dirName){
    return Paths.get(dirName).toFile().listFiles().length == 0;
}
