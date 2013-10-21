import org.ff4j.FF4j;

public class GettingStarted {

    public static void main(String[] args) {

        FF4j ff4j = new FF4j("ff4j.xml").enable("sayHello");

        if (ff4j.isFlipped("sayHello")) {
            System.out.println("Hello World !");
        }
    }

}
