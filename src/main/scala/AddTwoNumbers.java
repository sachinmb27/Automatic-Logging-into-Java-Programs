class numbers {
    int x = 100;
}

public class AddTwoNumbers extends numbers {

    public static void main(String[] args) {

        AddTwoNumbers s = new AddTwoNumbers();
        int num1 = 5, num2 = s.x, sum;
        sum = num1 + num2;

        System.out.println("Sum of these numbers: "+sum);
        boolean i = false;
        while(i){
            System.out.println("hi");
        }
        add();
    }

    public static int add() {
        int a = 10, b = 20;
        return a + b;
    }
}