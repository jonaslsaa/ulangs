public class StringReversal {
  public static void main(String[] args) {
      String original = "Hello";
      String reversed = new StringBuilder(original).reverse().toString();
      System.out.println("Reversed: " + reversed);
  }
}
