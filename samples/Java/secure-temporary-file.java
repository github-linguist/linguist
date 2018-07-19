import java.io.File;

try {
    // Create temp file
    File filename = File.createTempFile("prefix", ".suffix");

    // Delete temp file when program exits
    filename.deleteOnExit();

    System.out.println(filename);

} catch (IOException e) {
}
