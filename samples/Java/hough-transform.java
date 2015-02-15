import java.awt.image.*;
import java.io.File;
import java.io.IOException;
import javax.imageio.*;

public class HoughTransform
{
  public static ArrayData houghTransform(ArrayData inputData, int thetaAxisSize, int rAxisSize, int minContrast)
  {
    int width = inputData.width;
    int height = inputData.height;
    int maxRadius = (int)Math.ceil(Math.hypot(width, height));
    int halfRAxisSize = rAxisSize >>> 1;
    ArrayData outputData = new ArrayData(thetaAxisSize, rAxisSize);
    // x output ranges from 0 to pi
    // y output ranges from -maxRadius to maxRadius
    double[] sinTable = new double[thetaAxisSize];
    double[] cosTable = new double[thetaAxisSize];
    for (int theta = thetaAxisSize - 1; theta >= 0; theta--)
    {
      double thetaRadians = theta * Math.PI / thetaAxisSize;
      sinTable[theta] = Math.sin(thetaRadians);
      cosTable[theta] = Math.cos(thetaRadians);
    }

    for (int y = height - 1; y >= 0; y--)
    {
      for (int x = width - 1; x >= 0; x--)
      {
        if (inputData.contrast(x, y, minContrast))
        {
          for (int theta = thetaAxisSize - 1; theta >= 0; theta--)
          {
            double r = cosTable[theta] * x + sinTable[theta] * y;
            int rScaled = (int)Math.round(r * halfRAxisSize / maxRadius) + halfRAxisSize;
            outputData.accumulate(theta, rScaled, 1);
          }
        }
      }
    }
    return outputData;
  }

  public static class ArrayData
  {
    public final int[] dataArray;
    public final int width;
    public final int height;

    public ArrayData(int width, int height)
    {
      this(new int[width * height], width, height);
    }

    public ArrayData(int[] dataArray, int width, int height)
    {
      this.dataArray = dataArray;
      this.width = width;
      this.height = height;
    }

    public int get(int x, int y)
    {  return dataArray[y * width + x];  }

    public void set(int x, int y, int value)
    {  dataArray[y * width + x] = value;  }

    public void accumulate(int x, int y, int delta)
    {  set(x, y, get(x, y) + delta);  }

    public boolean contrast(int x, int y, int minContrast)
    {
      int centerValue = get(x, y);
      for (int i = 8; i >= 0; i--)
      {
        if (i == 4)
          continue;
        int newx = x + (i % 3) - 1;
        int newy = y + (i / 3) - 1;
        if ((newx < 0) || (newx >= width) || (newy < 0) || (newy >= height))
          continue;
        if (Math.abs(get(newx, newy) - centerValue) >= minContrast)
          return true;
      }
      return false;
    }

    public int getMax()
    {
      int max = dataArray[0];
      for (int i = width * height - 1; i > 0; i--)
        if (dataArray[i] > max)
          max = dataArray[i];
      return max;
    }
  }

  public static ArrayData getArrayDataFromImage(String filename) throws IOException
  {
    BufferedImage inputImage = ImageIO.read(new File(filename));
    int width = inputImage.getWidth();
    int height = inputImage.getHeight();
    int[] rgbData = inputImage.getRGB(0, 0, width, height, null, 0, width);
    ArrayData arrayData = new ArrayData(width, height);
    // Flip y axis when reading image
    for (int y = 0; y < height; y++)
    {
      for (int x = 0; x < width; x++)
      {
        int rgbValue = rgbData[y * width + x];
        rgbValue = (int)(((rgbValue & 0xFF0000) >>> 16) * 0.30 + ((rgbValue & 0xFF00) >>> 8) * 0.59 + (rgbValue & 0xFF) * 0.11);
        arrayData.set(x, height - 1 - y, rgbValue);
      }
    }
    return arrayData;
  }

  public static void writeOutputImage(String filename, ArrayData arrayData) throws IOException
  {
    int max = arrayData.getMax();
    BufferedImage outputImage = new BufferedImage(arrayData.width, arrayData.height, BufferedImage.TYPE_INT_ARGB);
    for (int y = 0; y < arrayData.height; y++)
    {
      for (int x = 0; x < arrayData.width; x++)
      {
        int n = Math.min((int)Math.round(arrayData.get(x, y) * 255.0 / max), 255);
        outputImage.setRGB(x, arrayData.height - 1 - y, (n << 16) | (n << 8) | 0x90 | -0x01000000);
      }
    }
    ImageIO.write(outputImage, "PNG", new File(filename));
    return;
  }

  public static void main(String[] args) throws IOException
  {
    ArrayData inputData = getArrayDataFromImage(args[0]);
    int minContrast = (args.length >= 4) ? 64 : Integer.parseInt(args[4]);
    ArrayData outputData = houghTransform(inputData, Integer.parseInt(args[2]), Integer.parseInt(args[3]), minContrast);
    writeOutputImage(args[1], outputData);
    return;
  }
}
