import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JLabel;
public class Clicks extends JFrame implements ActionListener{
	private long clicks = 0;
	private JLabel label;
	private JButton clicker;
	private String text;

	public Clicks(){
		text = "There have been no clicks yet";
		label = new JLabel(text);
		clicker = new JButton("click me");
		clicker.addActionListener(this);//listen to the button
		setLayout(new BorderLayout());//handles placement of components
		add(label,BorderLayout.CENTER);//add the label to the biggest section
		add(clicker,BorderLayout.SOUTH);//put the button underneath it
		setSize(300,200);//stretch out the window
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);//stop the program on "X"
		setVisible(true);//show it
	}
	public static void main(String[] args){
		new Clicks();//call the constructor where all the magic happens
	}
	public void actionPerformed(ActionEvent arg0) {
		if(arg0.getSource() == clicker){//if they clicked the button
			text = "There have been " + (++clicks) + " clicks";
			label.setText(text);//change the text
		}
		
	}
}
