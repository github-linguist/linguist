import java.util.*;
import java.util.regex.*;

class Range implements Enumeration {
  private int clower, cupper;
  private int value;
  private boolean inrange;
  private Scanner ps = null;
  private String ss;

  private static String del = "\\s*,\\s*";

  public Range(String s) {
    ss = s;
    reset();
  }

  public boolean hasMoreElements() {
    return (inrange && (value >= clower && value <= cupper)) || ps.hasNext();
  }

  public Object nextElement() throws NoSuchElementException {
    if (!hasMoreElements())
      throw new NoSuchElementException();
    if (inrange && (value >= clower && value <= cupper)) {
      value++;
      return value-1;
    }
    inrange = false;
    String n = ps.next();
    if (n.matches("[+-]?\\d+-[+-]?\\d+")) {
      Scanner ls = new Scanner(n);
      ls.findInLine("([+-]?\\d+)-([+-]?\\d+)");
      MatchResult r = ls.match();
      clower = Integer.parseInt(r.group(1));
      cupper = Integer.parseInt(r.group(2));
      value = clower+1;
      inrange = true;
      ls.close();
      return clower;
    }
    return Integer.parseInt(n);
  }

  public void reset() {
    if (ps != null)
      ps.close();
    ps = new Scanner(ss).useDelimiter(del);
    inrange = false;
  }

  protected void finalize() throws Throwable {
    ps.close();
    super.finalize();
  }
}

class rangexp {
  public static void main(String[] args) {
    Range r = new Range("-6,-3--1,3-5,7-11,14,15,17-20");
    while (r.hasMoreElements()) {
      System.out.print(r.nextElement() + " ");
    }
    System.out.println();
  }
}
