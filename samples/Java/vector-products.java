public class VectorProds{
    public static class Vector3D<T extends Number>{
        private T a, b, c;

        public Vector3D(T a, T b, T c){
            this.a = a;
            this.b = b;
            this.c = c;
        }

        public double dot(Vector3D<?> vec){
            return (a.doubleValue() * vec.a.doubleValue() +
                    b.doubleValue() * vec.b.doubleValue() +
                    c.doubleValue() * vec.c.doubleValue());
        }

        public Vector3D<Double> cross(Vector3D<?> vec){
            Double newA = b.doubleValue()*vec.c.doubleValue() - c.doubleValue()*vec.b.doubleValue();
            Double newB = c.doubleValue()*vec.a.doubleValue() - a.doubleValue()*vec.c.doubleValue();
            Double newC = a.doubleValue()*vec.b.doubleValue() - b.doubleValue()*vec.a.doubleValue();
            return new Vector3D<Double>(newA, newB, newC);
        }

        public double scalTrip(Vector3D<?> vecB, Vector3D<?> vecC){
            return this.dot(vecB.cross(vecC));
        }

        public Vector3D<Double> vecTrip(Vector3D<?> vecB, Vector3D<?> vecC){
            return this.cross(vecB.cross(vecC));
        }

        @Override
        public String toString(){
            return "<" + a.toString() + ", " + b.toString() + ", " + c.toString() + ">";
        }
    }

    public static void main(String[] args){
        Vector3D<Integer> a = new Vector3D<Integer>(3, 4, 5);
        Vector3D<Integer> b = new Vector3D<Integer>(4, 3, 5);
        Vector3D<Integer> c = new Vector3D<Integer>(-5, -12, -13);

        System.out.println(a.dot(b));
        System.out.println(a.cross(b));
        System.out.println(a.scalTrip(b, c));
        System.out.println(a.vecTrip(b, c));
    }
}
