using System;

namespace BinarySearchTree
{
    class Program
    {
        static void Main(string[] args)
        {
            Tree testTree = new Tree();

            Node root = null;

            root = testTree.Add(root, 2);

            testTree.Add(root, 1);
            testTree.Add(root, 3);

            Console.Read();
        }
    }
}
