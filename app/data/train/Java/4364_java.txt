package Thread;

/**
 * Created by cnkaptan on 5/8/15.
 */
public class KarisikSayma {

    public static void main(String[] args){
        ThreadTest thrd = new ThreadTest("Thread");
        Thread runnableThread = new Thread(new RunnableTest("Runnable"));

        thrd.start();
        runnableThread.start();
    }
}


class RunnableTest implements Runnable{
    String name;

    public RunnableTest(String name){
        this.name = name;
    }
    @Override
    public void run() {
        for (int i = 0; i < 21 ; i++){
            System.out.println(name+"\t"+i);
        }
    }
}


class ThreadTest extends Thread{
    String name;
    public ThreadTest(String name){
        this.name = name;
    }
    @Override
    public void run() {
        for (int i = 0; i < 21 ; i++){
            System.out.println(this.getName()+"\t"+i);
        }
    }
}
