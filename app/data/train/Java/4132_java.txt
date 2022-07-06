import java.lang.IndexOutOfBoundsException;

public interface Sequence
{
    public int size();           // Return number of elements in sequence.
    public void addFirst(int e); // Insert e at the front of the sequence.
    public void addLast(int e);  // Insert e at the back of the sequence.
    // Inserts an element e to be at index i.
    public void add(int i, int e) throws IndexOutOfBoundsException;
    // Returns the element at index i, without removing it.
    public int get(int i) throws IndexOutOfBoundsException;
    // Removes and returns the element at index i.
    public int remove(int i) throws IndexOutOfBoundsException;
}