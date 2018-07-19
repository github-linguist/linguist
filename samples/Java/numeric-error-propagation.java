public class Approx {
    private double value;
    private double error;

    public Approx(){this.value = this.error = 0;}

    public Approx(Approx b){
        this.value = b.value;
        this.error = b.error;
    }

    public Approx(double value, double error){
        this.value = value;
        this.error = error;
    }

    public Approx add(Approx b){
        value+= b.value;
        error = Math.sqrt(error * error + b.error * b.error);
        return this;
    }

    public Approx add(double b){
        value+= b;
        return this;
    }

    public Approx sub(Approx b){
        value-= b.value;
        error = Math.sqrt(error * error + b.error * b.error);
        return this;
    }

    public Approx sub(double b){
        value-= b;
        return this;
    }

    public Approx mult(Approx b){
        double oldVal = value;
        value*= b.value;
        error = Math.sqrt(value * value * (error*error) / (oldVal*oldVal) +
                                  (b.error*b.error) / (b.value*b.value));
        return this;
    }

    public Approx mult(double b){
        value*= b;
        error = Math.abs(b * error);
        return this;
    }

    public Approx div(Approx b){
        double oldVal = value;
        value/= b.value;
        error = Math.sqrt(value * value * (error*error) / (oldVal*oldVal) +
                                  (b.error*b.error) / (b.value*b.value));
        return this;
    }

    public Approx div(double b){
        value/= b;
        error = Math.abs(b * error);
        return this;
    }

    public Approx pow(double b){
        double oldVal = value;
        value = Math.pow(value, b);
        error = Math.abs(value * b * (error / oldVal));
        return this;
    }

    @Override
    public String toString(){return value+"±"+error;}

    public static void main(String[] args){
        Approx x1 = new Approx(100, 1.1);
        Approx x2 = new Approx(50, 1.2);
        Approx y1 = new Approx(200, 2.2);
        Approx y2 = new Approx(100, 2.3);

        x1.sub(x2).pow(2).add(y1.sub(y2).pow(2)).pow(0.5);

        System.out.println(x1);
    }
}
