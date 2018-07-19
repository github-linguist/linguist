import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;

import javax.swing.JButton;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

public class Interact extends JFrame{
	final JTextField numberField;
	final JButton incButton, decButton;
	
	public Interact(){
		//stop the GUI threads when the user hits the X button
		setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		
		numberField = new JTextField();
		incButton = new JButton("Increment");
		decButton = new JButton("Decrement");
		
		numberField.setText("0");//start at 0
		decButton.setEnabled(false);//we're already at 0
		
		//listen for button presses in the text field
		numberField.addKeyListener(new KeyListener(){
			@Override
			public void keyTyped(KeyEvent e) {
				//if the entered character is not a digit
				if(!Character.isDigit(e.getKeyChar())){
					//eat the event (i.e. stop it from being processed)
					e.consume();
				}else if(Character.isDigit(e.getKeyChar())){
					//This method is executed from the event thread and updating the GUI
					//from there doesn't always work. invokeLater will ensure that the
					//GUI is updated
					SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
							String text = numberField.getText();
							if(text.isEmpty()){//default to 0 when all text is erased
								numberField.setText("0");
								decButton.setEnabled(false);
								incButton.setEnabled(true);
								return;
							}
							if(Long.valueOf(text) <= 0){
								decButton.setEnabled(false);
								incButton.setEnabled(true);
							}else if(Long.valueOf(text) >= 10){
								incButton.setEnabled(false);
								decButton.setEnabled(true);
							}else{
								incButton.setEnabled(true);
								decButton.setEnabled(true);
							}
						}
					});
				}
			}
			@Override
			public void keyReleased(KeyEvent e){}
			@Override
			public void keyPressed(KeyEvent e){
				//backspace and delete don't register in keyTyped because they don't
				//display a Unicode character, so they must be handled here
				if(e.getKeyCode() == KeyEvent.VK_BACK_SPACE ||
						e.getKeyCode() == KeyEvent.VK_DELETE){
					SwingUtilities.invokeLater(new Runnable() {
						@Override
						public void run() {
							String text = numberField.getText();
							if(text.isEmpty()){
								numberField.setText("0");
								decButton.setEnabled(false);
								incButton.setEnabled(true);
								return;
							}
							if(Long.valueOf(text) <= 0){
								decButton.setEnabled(false);
								incButton.setEnabled(true);
							}else if(Long.valueOf(text) >= 10){
								incButton.setEnabled(false);
								decButton.setEnabled(true);
							}else{
								incButton.setEnabled(true);
								decButton.setEnabled(true);
							}
						}
					});
				}
			}
		});
		
		//listen for button clicks on the increment button
		incButton.addActionListener(new ActionListener(){
			@Override
			public void actionPerformed(ActionEvent e) {
				String text = numberField.getText();
				numberField.setText((Long.valueOf(text) + 1) + "");
				if(Long.valueOf(text) + 1 >= 10){
					incButton.setEnabled(false);
				}
				
				if(Long.valueOf(text) + 1 > 0){
					decButton.setEnabled(true);
				}
			}
		});
		
		//listen for button clicks on the random button
		decButton.addActionListener(new ActionListener(){
			@Override
			public void actionPerformed(ActionEvent e) {
				String text = numberField.getText();
				numberField.setText((Long.valueOf(text) - 1) + "");
				if(Long.valueOf(text) - 1 <= 0){
					decButton.setEnabled(false);
				}
				
				if(Long.valueOf(text) - 1 < 10){
					incButton.setEnabled(true);
				}
			}
		});
		
		//arrange the components in a grid with 2 rows and 1 column
		setLayout(new GridLayout(2, 1));
		
		//a secondary panel for arranging both buttons in one grid space in the window
		JPanel buttonPanel = new JPanel();
		
		//the buttons are in a grid with 1 row and 2 columns
		buttonPanel.setLayout(new GridLayout(1, 2));
		//add the buttons
		buttonPanel.add(incButton);
		buttonPanel.add(decButton);
		
		//put the number field on top of the buttons
		add(numberField);
		add(buttonPanel);
		//size the window appropriately
		pack();
		
	}

	public static void main(String[] args){
		new Interact().setVisible(true);
	}
}
