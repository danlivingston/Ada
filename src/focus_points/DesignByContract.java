public class DesignByContract {
    // Java doesn’t have built-in support for DbC, but similar behavior can be
    // emulated using assertions, exceptions, or tools like Java Modeling Language
    // (JML).
    // In this Java example, assertions are used to check preconditions and
    // postconditions. However, assertions can be disabled at runtime, making them
    // less reliable than Ada's built-in contract checking.
    public static class BankAccount {
        private double balance = 0.0;

        public void deposit(double amount) {
            assert amount > 0 : "Deposit must be positive";
            double oldBalance = balance;
            balance += amount;
            assert balance == oldBalance + amount : "Postcondition failed";
        }

        public void withdraw(double amount) {
            assert amount > 0 && amount <= balance : "Invalid withdraw amount";
            double oldBalance = balance;
            balance -= amount;
            assert balance == oldBalance - amount : "Postcondition failed";
        }

        public double getBalance() {
            return balance;
        }
    }

    public static void main(String[] args) {
        BankAccount account = new BankAccount();

        System.out.println("Initial balance: " + account.getBalance());

        System.out.println("Depositing 100.0...");
        account.deposit(100.0);
        System.out.println("Balance after deposit: " + account.getBalance());

        System.out.println("Withdrawing 30.0...");
        account.withdraw(30.0);
        System.out.println("Balance after withdrawal: " + account.getBalance());

        // Uncommenting the following line will trigger an assertion error
        // System.out.println("Attempting to withdraw 1000.0...");
        // account.withdraw(1000.0);

        System.out.println("Done.");
    }
}