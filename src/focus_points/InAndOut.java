public class InAndOut {

    // In Java, there is no way to explicitly declare if a parameter is used for input, output, or both.
    // All parameters are passed by value — but if you pass an object, Java copies the reference,
    // so you can change the object’s content.

    static class Data {
        public int value;
    }

    // Example of modifying an object (acts like an "in out" parameter)
    static void updateObject(Data d) {
        // Modifies the original object because the reference is passed
        d.value = 42;
    }

    // Example of modifying a primitive type (acts like an "in" parameter)
    static void updatePrimitive(int x) {
        // This does not affect the original value because x is just a copy
        x = 42;
    }

    public static void main(String[] args) {
        // Example with an object
        Data data = new Data();
        data.value = 10;
        System.out.println("Before updateObject: " + data.value);
        updateObject(data); // Modifies the original object
        System.out.println("After updateObject: " + data.value);

        // Example with a primitive type
        int number = 10;
        System.out.println("Before updatePrimitive: " + number);
        updatePrimitive(number); // Does not modify the original value
        System.out.println("After updatePrimitive: " + number);
    }
}
