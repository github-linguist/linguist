import javax.swing.*;

public class GetInputSwing {
    public static void main(String[] args) throws Exception {
        int number = Integer.parseInt(
                JOptionPane.showInputDialog ("Enter an Integer"));
        String string = JOptionPane.showInputDialog ("Enter a String");
    }
}
