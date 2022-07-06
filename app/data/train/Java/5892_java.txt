package com.jikexueyuan.classdemo;

/**
 * Created by zmzp on 14-12-4.
 */
class Student{
    public void tell(){
        System.out.println("Hello Jikexueyuan");
    }
}
public class ClassDemo03 {
    public static void main(String[] args) {
//        Student stu = new Student();
//        stu.tell();
        //匿名对象
        new Student().tell();
    }
}
