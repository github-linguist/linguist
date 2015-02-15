import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JFrame;

public class ColorFrame extends JFrame {
	public ColorFrame(int width, int height) {
		this.setDefaultCloseOperation(EXIT_ON_CLOSE);
		this.setSize(width, height);
		this.setVisible(true);
	}

	@Override
	public void paint(Graphics g) {
		Color[] colors = { Color.black, Color.red, Color.green, Color.blue,
				Color.pink, Color.CYAN, Color.yellow, Color.white };

		for (int i = 0; i < colors.length; i++) {
			g.setColor(colors[i]);
			g.fillRect(this.getWidth() / colors.length * i, 0, this.getWidth()
					/ colors.length, this.getHeight());
		}
	}

	public static void main(String args[]) {
		new ColorFrame(200, 200);
	}
}
