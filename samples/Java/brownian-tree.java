import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.util.*;
import javax.swing.JFrame;

public class BrownianTree extends JFrame implements Runnable {

    BufferedImage I;
    private List<Particle> particles;
    static Random rand = new Random();

    public BrownianTree() {
        super("Brownian Tree");
        setBounds(100, 100, 400, 300);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        I = new BufferedImage(getWidth(), getHeight(), BufferedImage.TYPE_INT_RGB);
        I.setRGB(I.getWidth() / 2, I.getHeight() / 2, 0xff00);
        particles = new LinkedList<Particle>();
    }

    @Override
    public void paint(Graphics g) {
        g.drawImage(I, 0, 0, this);
    }

    public void run() {
        for (int i = 0; i < 20000; i++) {
            particles.add(new Particle());
        }
        while (!particles.isEmpty()) {
            for (Iterator<Particle> it = particles.iterator(); it.hasNext();) {
                if (it.next().move()) {
                    it.remove();
                }
            }
            repaint();
        }
    }

    public static void main(String[] args) {
        BrownianTree b = new BrownianTree();
        b.setVisible(true);
        new Thread(b).start();
    }

    private class Particle {

        private int x, y;

        private Particle() {
            x = rand.nextInt(I.getWidth());
            y = rand.nextInt(I.getHeight());
        }

        /* returns true if either out of bounds or collided with tree */
        private boolean move() {
            int dx = rand.nextInt(3) - 1;
            int dy = rand.nextInt(3) - 1;
            if ((x + dx < 0) || (y + dy < 0)
                    || (y + dy >= I.getHeight()) || (x + dx >= I.getWidth())) {
                return true;
            }
            x += dx;
            y += dy;
            if ((I.getRGB(x, y) & 0xff00) == 0xff00) {
                I.setRGB(x - dx, y - dy, 0xff00);
                return true;
            }
            return false;
        }
    }
}
