package tweaking.concurrency.reetrantLockExample;

public class MainClass {
    
    public static void main(final String[] args) {
        
        final ThreadSafeArrayList<String> threadSafeList = new ThreadSafeArrayList<>();
        
        final ListAdderThread threadA = new ListAdderThread("threadA", threadSafeList);
        final ListAdderThread threadB = new ListAdderThread("threadB", threadSafeList);
        final ListAdderThread threadC = new ListAdderThread("threadC", threadSafeList);
        
        threadA.start();
        threadB.start();
        threadC.start();
    }
}
