import java.util.ArrayList;
import ScreenScrape;

public class CountProgramExamples {
    private static final String baseURL = "http://rosettacode.org/wiki/";
    private static final String rootURL = "http://www.rosettacode.org/w/"
        + "api.php?action=query&list=categorymembers"
        + "&cmtitle=Category:Programming_Tasks&cmlimit=500&format=xml";
    private static final String taskBegin = "title=\"";
    private static final String taskEnd = "\"";
    private static final String exmplBegin = "<span class=\"tocnumber\">";
    private static final String exmplEnd = "</span>";
    private static final String editBegin = "<span class=\"editsection\">";

    /**
     * @param args
     */
    public static void main(String[] args) {
        int exTotal = 0;
        try {
            // Get root query results
            ArrayList<String> tasks = new ArrayList<String>();
            ScreenScrape ss = new ScreenScrape();
            String rootPage = ss.read(rootURL);
            while (rootPage.contains(taskBegin)) {
                rootPage = rootPage.substring(rootPage.indexOf(taskBegin)
                    + taskBegin.length());
                String title = rootPage.substring(0, rootPage.indexOf(taskEnd));
                if (!title.contains("Category:")) {
                    tasks.add(title);
                }
                rootPage = rootPage.substring(rootPage.indexOf(taskEnd));
            }
            // Loop through each task and print count
            for (String task : tasks) {
                String title = task.replaceAll("&#039;", "'");
                String taskPage = ss.read(baseURL + title.replaceAll(" ", "_"));
                int exSubTot;
                if (taskPage.contains(exmplBegin)) {
                    int startPos = taskPage.lastIndexOf(exmplBegin)
                        + exmplBegin.length();
                    String countStr = taskPage.substring(startPos,
                        taskPage.indexOf(exmplEnd, startPos));
                    exSubTot = Integer
                        .parseInt(countStr.contains(".") ? countStr
                            .substring(0, countStr.indexOf("."))
                            : countStr);
                } else {
                    exSubTot = 0;
                    while (taskPage.contains(editBegin)) {
                        taskPage = taskPage.substring(taskPage
                            .indexOf(editBegin) + editBegin.length());
                        exSubTot++;
                    }
                }
                exTotal += exSubTot;
                System.out.println(title + ": " + exSubTot + " examples.");
            }
            // Print total
            System.out.println("\nTotal: " + exTotal + " examples.");
        } catch (Exception e) {
            System.out.println(title);
            System.out.println(startPos + ":"
                + taskPage.indexOf(exmplEnd, startPos));
            System.out.println(taskPage);
            e.printStackTrace(System.out);
        }
    }
}
