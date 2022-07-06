/**
 * Created by avinashvundyala on 09/07/16.
 */
public class SelectionSort {

  public int[] iterativeSelectionSort(int[] arr) {
    int temp, smallIdx;
    for(int i = 0; i < arr.length; i++) {
      smallIdx = i;
      for(int j = i; j < arr.length; j++) {
        if(arr[smallIdx] > arr[j]) {
          smallIdx = j;
        }
      }
      temp = arr[i];
      arr[i] = arr[smallIdx];
      arr[smallIdx] = temp;
    }
    return arr;
  }

  public int[] recursiveSelectionSort(int[] arr, int startIdx) {
    if(startIdx == arr.length) {
      return arr;
    }
    int temp, smallIdx;
    smallIdx = startIdx;
    for(int j = startIdx; j < arr.length; j++) {
      if(arr[smallIdx] > arr[j]) {
        smallIdx = j;
      }
    }
    temp = arr[startIdx];
    arr[startIdx] = arr[smallIdx];
    arr[smallIdx] = temp;

    return recursiveSelectionSort(arr, startIdx + 1);
  }



  public void printArray(int[] array) {
        for(int number: array) {
            System.out.print(String.valueOf(number) + " ");
        }
    }

  public static void main(String args[]) {
        SelectionSort ss = new SelectionSort();

        int[] unsortedArray1 = {6,4,2,1,9,0};
        System.out.print("Unsoreted Array Sorting: ");
        ss.printArray(ss.iterativeSelectionSort(unsortedArray1));

        int[] unsortedArray2 = {1,2,3,4,5,6};
        System.out.print("\nAlready Soreted Array Sorting: ");
        ss.printArray(ss.iterativeSelectionSort(unsortedArray2));

        int[] unsortedArray3 = {6,5,4,3,2,1};
        System.out.print("\nUnsoreted Array Sorting: ");
        ss.printArray(ss.iterativeSelectionSort(unsortedArray3));

        int[] unsortedArray4 = {6,4,2,1,9,0};
        System.out.print("\nUnsoreted Array Sorting: ");
        ss.printArray(ss.recursiveSelectionSort(unsortedArray4, 0));

        int[] unsortedArray5 = {1,2,3,4,5,6};
        System.out.print("\nAlready Soreted Array Sorting: ");
        ss.printArray(ss.recursiveSelectionSort(unsortedArray5, 0));

        int[] unsortedArray6 = {6,5,4,3,2,1};
        System.out.print("\nUnsoreted Array Sorting: ");
        ss.printArray(ss.recursiveSelectionSort(unsortedArray6, 0));
    }

}
