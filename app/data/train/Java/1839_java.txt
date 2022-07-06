package com.jason.showcase.lambdas;

/**
 * Created by Qinjianf on 2016/7/19.
 */
public class Lambda {

    public void execute(Action action) {
        action.run("Hello Lambda!");
    }

    public void test() {
        execute(System.out::println);
    }

    public static void main(String[] args) {
        new Lambda().test();
    }
}
