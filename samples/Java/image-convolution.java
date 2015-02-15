import java.awt.image.*;
import java.io.File;
import java.io.IOException;
import javax.imageio.*;

public class ImageConvolution
{
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
  }

  private static int bound(int value, int endIndex)
  {
    if (value < 0)
      return 0;
    if (value < endIndex)
      return value;
    return endIndex - 1;
  }

  public static ArrayData convolute(ArrayData inputData, ArrayData kernel, int kernelDivisor)
  {
    int inputWidth = inputData.width;
    int inputHeight = inputData.height;
    int kernelWidth = kernel.width;
    int kernelHeight = kernel.height;
    if ((kernelWidth <= 0) || ((kernelWidth & 1) != 1))
      throw new IllegalArgumentException("Kernel must have odd width");
    if ((kernelHeight <= 0) || ((kernelHeight & 1) != 1))
      throw new IllegalArgumentException("Kernel must have odd height");
    int kernelWidthRadius = kernelWidth >>> 1;
    int kernelHeightRadius = kernelHeight >>> 1;

    ArrayData outputData = new ArrayData(inputWidth, inputHeight);
    for (int i = inputWidth - 1; i >= 0; i--)
    {
      for (int j = inputHeight - 1; j >= 0; j--)
      {
        double newValue = 0.0;
        for (int kw = kernelWidth - 1; kw >= 0; kw--)
          for (int kh = kernelHeight - 1; kh >= 0; kh--)
            newValue += kernel.get(kw, kh) * inputData.get(
                          bound(i + kw - kernelWidthRadius, inputWidth),
                          bound(j + kh - kernelHeightRadius, inputHeight));
        outputData.set(i, j, (int)Math.round(newValue / kernelDivisor));
      }
    }
    return outputData;
  }

  public static ArrayData[] getArrayDatasFromImage(String filename) throws IOException
  {
    BufferedImage inputImage = ImageIO.read(new File(filename));
    int width = inputImage.getWidth();
    int height = inputImage.getHeight();
    int[] rgbData = inputImage.getRGB(0, 0, width, height, null, 0, width);
    ArrayData reds = new ArrayData(width, height);
    ArrayData greens = new ArrayData(width, height);
    ArrayData blues = new ArrayData(width, height);
    for (int y = 0; y < height; y++)
    {
      for (int x = 0; x < width; x++)
      {
        int rgbValue = rgbData[y * width + x];
        reds.set(x, y, (rgbValue >>> 16) & 0xFF);
        greens.set(x, y, (rgbValue >>> 8) & 0xFF);
        blues.set(x, y, rgbValue & 0xFF);
      }
    }
    return new ArrayData[] { reds, greens, blues };
  }

  public static void writeOutputImage(String filename, ArrayData[] redGreenBlue) throws IOException
  {
    ArrayData reds = redGreenBlue[0];
    ArrayData greens = redGreenBlue[1];
    ArrayData blues = redGreenBlue[2];
    BufferedImage outputImage = new BufferedImage(reds.width, reds.height,
                                                  BufferedImage.TYPE_INT_ARGB);
    for (int y = 0; y < reds.height; y++)
    {
      for (int x = 0; x < reds.width; x++)
      {
        int red = bound(reds.get(x, y), 256);
        int green = bound(greens.get(x, y), 256);
        int blue = bound(blues.get(x, y), 256);
        outputImage.setRGB(x, y, (red << 16) | (green << 8) | blue | -0x01000000);
      }
    }
    ImageIO.write(outputImage, "PNG", new File(filename));
    return;
  }

  public static void main(String[] args) throws IOException
  {
    int kernelWidth = Integer.parseInt(args[2]);
    int kernelHeight = Integer.parseInt(args[3]);
    int kernelDivisor = Integer.parseInt(args[4]);
    System.out.println("Kernel size: " + kernelWidth + "x" + kernelHeight +
                       ", divisor=" + kernelDivisor);
    int y = 5;
    ArrayData kernel = new ArrayData(kernelWidth, kernelHeight);
    for (int i = 0; i < kernelHeight; i++)
    {
      System.out.print("[");
      for (int j = 0; j < kernelWidth; j++)
      {
        kernel.set(j, i, Integer.parseInt(args[y++]));
        System.out.print(" " + kernel.get(j, i) + " ");
      }
      System.out.println("]");
    }

    ArrayData[] dataArrays = getArrayDatasFromImage(args[0]);
    for (int i = 0; i < dataArrays.length; i++)
      dataArrays[i] = convolute(dataArrays[i], kernel, kernelDivisor);
    writeOutputImage(args[1], dataArrays);
    return;
  }
}
