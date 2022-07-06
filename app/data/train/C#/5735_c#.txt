using System;
using System.Collections.Generic;

namespace ConsoleApplication
{
    public class Node
    {
        private int data;
        private Node next;

        public int Data { get => data; set => data = value; }
        public Node Next { get => next; set => next = value; }

        public Node(int d)
        {
            Data = d;
            Next = null;
        }
    }

    public class Program
    {
        public static Node RemoveDuplicates(Node head)
        {
            //Write your code here
            var h = new HashSet<int>();
            Node n = null;
            while (head != null)
            {
                if (!h.Contains(head.Data))
                {
                    System.Console.WriteLine(head.Data);
                    h.Add(head.Data);
                    n = insert(n, head.Data);
                }

                head = head.Next;
            }

            return n;
        }

        public static Node insert(Node head, int data)
        {
            Node p = new Node(data);


            if (head == null)
                head = p;
            else if (head.Next == null)
                head.Next = p;
            else
            {
                Node start = head;
                while (start.Next != null)
                    start = start.Next;
                start.Next = p;

            }
            return head;
        }

        public static void display(Node head)
        {
            Node start = head;
            while (start != null)
            {
                Console.Write(start.Data + " ");
                start = start.Next;
            }
        }

        public static void Main(string[] args)
        {
            Node head = null;

            head = insert(head, 1);
            head = insert(head, 2);
            head = insert(head, 2);
            head = insert(head, 3);
            head = insert(head, 3);
            head = insert(head, 4);
            head = RemoveDuplicates(head);
            display(head);
            Console.ReadLine();
        }
    }
}
