import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.lang3.StringUtils;

/**
 * Aligns fields into columns, separated by "|"
 */
public class ColumnAligner {
    private List<String[]> words = new ArrayList<>();
    private int columns = 0;
    private List<Integer> columnWidths = new ArrayList<>();

    /**
     * Initialize columns aligner from lines in a single string
     *
     * @param s
     *            lines in a single string. Empty string does form a column.
     */
    public ColumnAligner(String s) {
        String[] lines = s.split("\\n");
        for (String line : lines) {
            processInputLine(line);
        }
    }

    /**
     * Initialize columns aligner from lines in a list of strings
     *
     * @param lines
     *            lines in a single string. Empty string does form a column.
     */
    public ColumnAligner(List<String> lines) {
        for (String line : lines) {
            processInputLine(line);
        }
    }

    private void processInputLine(String line) {
        String[] lineWords = line.split("\\$");
        words.add(lineWords);
        columns = Math.max(columns, lineWords.length);
        for (int i = 0; i < lineWords.length; i++) {
            String word = lineWords[i];
            if (i >= columnWidths.size()) {
                columnWidths.add(word.length());
            } else {
                columnWidths.set(i, Math.max(columnWidths.get(i), word.length()));
            }
        }
    }

    interface AlignFunction {
        String align(String s, int length);
    }

    /**
     * Left-align all columns
     *
     * @return Lines, terminated by "\n" of columns, separated by "|"
     */
    public String alignLeft() {
        return align(new AlignFunction() {
            @Override
            public String align(String s, int length) {
                return StringUtils.rightPad(s, length);
            }
        });
    }

    /**
     * Right-align all columns
     *
     * @return Lines, terminated by "\n" of columns, separated by "|"
     */
    public String alignRight() {
        return align(new AlignFunction() {
            @Override
            public String align(String s, int length) {
                return StringUtils.leftPad(s, length);
            }
        });
    }

    /**
     * Center-align all columns
     *
     * @return Lines, terminated by "\n" of columns, separated by "|"
     */
    public String alignCenter() {
        return align(new AlignFunction() {
            @Override
            public String align(String s, int length) {
                return StringUtils.center(s, length);
            }
        });
    }

    private String align(AlignFunction a) {
        StringBuilder result = new StringBuilder();
        for (String[] lineWords : words) {
            for (int i = 0; i < lineWords.length; i++) {
                String word = lineWords[i];
                if (i == 0) {
                    result.append("|");
                }
                result.append(a.align(word, columnWidths.get(i)) + "|");
            }
            result.append("\n");
        }
        return result.toString();
    }

    public static void main(String args[]) throws IOException {
        if (args.length < 1) {
            System.out.println("Usage: ColumnAligner file [left|right|center]");
            return;
        }
        String filePath = args[0];
        String alignment = "left";
        if (args.length >= 2) {
            alignment = args[1];
        }
        ColumnAligner ca = new ColumnAligner(Files.readAllLines(Paths.get(filePath), StandardCharsets.UTF_8));
        switch (alignment) {
        case "left":
            System.out.print(ca.alignLeft());
            break;
        case "right":
            System.out.print(ca.alignRight());
            break;
        case "center":
            System.out.print(ca.alignCenter());
            break;
        default:
            System.err.println(String.format("Error! Unknown alignment: '%s'", alignment));
            break;
        }
    }
}
