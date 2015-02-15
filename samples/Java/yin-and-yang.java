package org.rosettacode.yinandyang;

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Image;
import java.awt.image.BufferedImage;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;

public class YinYangGenerator
{
    private final int size;

    public YinYangGenerator(final int size)
    {
        this.size = size;
    }

    /**
     *  Draw a yin yang symbol on the given graphics context.
     */
    public void drawYinYang(final Graphics graphics)
    {
        // Preserve the color for the caller
        final Color colorSave = graphics.getColor();

        graphics.setColor(Color.WHITE);
        // Use fillOval to draw a filled in circle
        graphics.fillOval(0, 0, size-1, size-1);

        graphics.setColor(Color.BLACK);
        // Use fillArc to draw part of a filled in circle
        graphics.fillArc(0, 0, size-1, size-1, 270, 180);
        graphics.fillOval(size/4, size/2, size/2, size/2);

        graphics.setColor(Color.WHITE);
        graphics.fillOval(size/4, 0, size/2, size/2);
        graphics.fillOval(7*size/16, 11*size/16, size/8, size/8);

        graphics.setColor(Color.BLACK);
        graphics.fillOval(7*size/16, 3*size/16, size/8, size/8);
        // Use drawOval to draw an empty circle for the outside border
        graphics.drawOval(0, 0, size-1, size-1);

        // Restore the color for the caller
        graphics.setColor(colorSave);
    }

    /**
     *  Create an image containing a yin yang symbol.
     */
    public Image createImage(final Color bg)
    {
        // A BufferedImage creates the image in memory
        final BufferedImage image = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB);
        // Get the graphics object for the image; note in many
        // applications you actually use Graphics2D for the
        // additional API calls
        final Graphics graphics = image.getGraphics();
        // Color in the background of the image
        graphics.setColor(bg);
        graphics.fillRect(0,0,size,size);
        drawYinYang(graphics);
        return image;
    }

    public static void main(final String args[])
    {
        final int size = Integer.parseInt(args[0]);
        final YinYangGenerator generator = new YinYangGenerator(size);

        final JFrame frame = new JFrame("Yin Yang Generator");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        final Image yinYang = generator.createImage(frame.getBackground());
        // Use JLabel to display an image
        frame.add(new JLabel(new ImageIcon(yinYang)));
        frame.pack();
        frame.setVisible(true);
    }
}
