public class BankAccount {
  private String accountNumber;
  private double balance;

  public BankAccount(String accountNumber, double initialBalance) {
      this.accountNumber = accountNumber;
      this.balance = initialBalance;
  }

  public void deposit(double amount) {
      if (amount > 0) {
          balance += amount;
          System.out.println("Deposited: " + amount);
      }
  }

  public void withdraw(double amount) {
      if (amount > 0 && amount <= balance) {
          balance -= amount;
          System.out.println("Withdrew: " + amount);
      } else {
          System.out.println("Insufficient funds or invalid amount.");
      }
  }

  public void displayBalance() {
      System.out.println("Account Balance: " + balance);
  }

  public static void main(String[] args) {
      BankAccount account = new BankAccount("123456", 500.0);
      account.deposit(150.0);
      account.withdraw(100.0);
      account.displayBalance();
  }
}
