public class GateLogic
{
  // Basic gate interfaces
  public interface OneInputGate
  {  boolean eval(boolean input);  }

  public interface TwoInputGate
  {  boolean eval(boolean input1, boolean input2);  }

  public interface MultiGate
  {  boolean[] eval(boolean... inputs);  }

  // Create NOT
  public static OneInputGate NOT = new OneInputGate() {
    public boolean eval(boolean input)
    {  return !input;  }
  };

  // Create AND
  public static TwoInputGate AND = new TwoInputGate() {
    public boolean eval(boolean input1, boolean input2)
    {  return input1 && input2;  }
  };

  // Create OR
  public static TwoInputGate OR = new TwoInputGate() {
    public boolean eval(boolean input1, boolean input2)
    {  return input1 || input2;  }
  };

  // Create XOR
  public static TwoInputGate XOR = new TwoInputGate() {
    public boolean eval(boolean input1, boolean input2)
    {
      return OR.eval(
               AND.eval(input1, NOT.eval(input2)),
               AND.eval(NOT.eval(input1), input2)
             );
    }
  };

  // Create HALF_ADDER
  public static MultiGate HALF_ADDER = new MultiGate() {
    public boolean[] eval(boolean... inputs)
    {
      if (inputs.length != 2)
        throw new IllegalArgumentException();
      return new boolean[] {
        XOR.eval(inputs[0], inputs[1]),  // Output bit
        AND.eval(inputs[0], inputs[1])   // Carry bit
      };
    }
  };

  // Create FULL_ADDER
  public static MultiGate FULL_ADDER = new MultiGate() {
    public boolean[] eval(boolean... inputs)
    {
      if (inputs.length != 3)
        throw new IllegalArgumentException();
      // Inputs: CarryIn, A, B
      // Outputs: S, CarryOut
      boolean[] haOutputs1 = HALF_ADDER.eval(inputs[0], inputs[1]);
      boolean[] haOutputs2 = HALF_ADDER.eval(haOutputs1[0], inputs[2]);
      return new boolean[] {
        haOutputs2[0],                         // Output bit
        OR.eval(haOutputs1[1], haOutputs2[1])  // Carry bit
      };
    }
  };

  public static MultiGate buildAdder(final int numBits)
  {
    return new MultiGate() {
      public boolean[] eval(boolean... inputs)
      {
        // Inputs: A0, A1, A2..., B0, B1, B2...
        if (inputs.length != (numBits << 1))
          throw new IllegalArgumentException();
        boolean[] outputs = new boolean[numBits + 1];
        boolean[] faInputs = new boolean[3];
        boolean[] faOutputs = null;
        for (int i = 0; i < numBits; i++)
        {
          faInputs[0] = (faOutputs == null) ? false : faOutputs[1];  // CarryIn
          faInputs[1] = inputs[i];                                   // Ai
          faInputs[2] = inputs[numBits + i];                         // Bi
          faOutputs = FULL_ADDER.eval(faInputs);
          outputs[i] = faOutputs[0];                                 // Si
        }
        if (faOutputs != null)
          outputs[numBits] = faOutputs[1];                           // CarryOut
        return outputs;
      }
    };
  }

  public static void main(String[] args)
  {
    int numBits = Integer.parseInt(args[0]);
    int firstNum = Integer.parseInt(args[1]);
    int secondNum = Integer.parseInt(args[2]);
    int maxNum = 1 << numBits;
    if ((firstNum < 0) || (firstNum >= maxNum))
    {
      System.out.println("First number is out of range");
      return;
    }
    if ((secondNum < 0) || (secondNum >= maxNum))
    {
      System.out.println("Second number is out of range");
      return;
    }

    MultiGate multiBitAdder = buildAdder(numBits);
    // Convert input numbers into array of bits
    boolean[] inputs = new boolean[numBits << 1];
    String firstNumDisplay = "";
    String secondNumDisplay = "";
    for (int i = 0; i < numBits; i++)
    {
      boolean firstBit = ((firstNum >>> i) & 1) == 1;
      boolean secondBit = ((secondNum >>> i) & 1) == 1;
      inputs[i] = firstBit;
      inputs[numBits + i] = secondBit;
      firstNumDisplay = (firstBit ? "1" : "0") + firstNumDisplay;
      secondNumDisplay = (secondBit ? "1" : "0") + secondNumDisplay;
    }

    boolean[] outputs = multiBitAdder.eval(inputs);
    int outputNum = 0;
    String outputNumDisplay = "";
    String outputCarryDisplay = null;
    for (int i = numBits; i >= 0; i--)
    {
      outputNum = (outputNum << 1) | (outputs[i] ? 1 : 0);
      if (i == numBits)
        outputCarryDisplay = outputs[i] ? "1" : "0";
      else
        outputNumDisplay += (outputs[i] ? "1" : "0");
    }
    System.out.println("numBits=" + numBits);
    System.out.println("A=" + firstNumDisplay + " (" + firstNum + "), B=" + secondNumDisplay + " (" + secondNum + "), S=" + outputCarryDisplay + " " + outputNumDisplay + " (" + outputNum + ")");
    return;
  }

}
