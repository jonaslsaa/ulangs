public class ArrayAverage {
  public static void main(String[] args) {
      int[] numbers = {2, 4, 6, 8, 10};
      int sum = 0;
      for (int num : numbers) {
          sum += num;
      }
      double average = (double) sum / numbers.length;
      System.out.println("Average: " + average);
  }
}