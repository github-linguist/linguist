URL url = new URL("https://sourceforge.net");
HttpsURLConnection connection = (HttpsURLConnection) url.openConnection();
Scanner scanner = new Scanner(connection.getInputStream());

while (scanner.hasNext()) {
    System.out.println(scanner.next());
}
