
public class DeclarativeRegions {
    public static void main(String[] args) {
        // The variable x is effectively final, similar to Ada's outer variable being
        // visible in inner scope.
        int x = 10;

        // The Java inner class here emulates Ada's nested procedure.
        class Inner {
            void display() {
                int y = x + 5; // x is visible here
                System.out.println("Y = " + y);
            }
        }

        Inner inner = new Inner();
        inner.display();
    }
}