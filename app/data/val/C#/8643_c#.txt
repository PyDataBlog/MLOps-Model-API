using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Kaleidoscope
{
    class MostRecentStack<T> : IEnumerable<T>, IStack<T>
    {
        private int capacity;
        private T[] list;
        private int headIndex;
        private int count;

        public class ItemDroppedEventArgs : EventArgs
        {
            public T Item { get; set; }

            public ItemDroppedEventArgs(T item)
            {
                Item = item;
            }
        }

        public event EventHandler<ItemDroppedEventArgs> ItemDropped;
        protected virtual void OnItemDropped(ItemDroppedEventArgs eventArgs)
        {
            if (ItemDropped != null)
                ItemDropped(this, eventArgs);
        }

        public MostRecentStack(int capacity)
        {
            this.capacity = capacity;
            if (capacity == 0) throw new ArgumentOutOfRangeException(this.GetType().Name + " can't have a capacity of zero.");
            list = new T[capacity];
            headIndex = 0;
            count = 0;
        }

        private int GetArrayIndex(int listIndex)
        {
            return (headIndex + listIndex) % capacity;
        }

        private void OffsetHeadIndexBack()
        {
            headIndex--;
            if (headIndex < 0) headIndex = capacity - 1;
        }

        private void OffsetHeadIndexFwd()
        {
            headIndex++;
            if (headIndex >= capacity) headIndex = 0;
        }

        public class Enumerator : IEnumerator<T>
        {
            private MostRecentStack<T> list;
            private int curIndex;

            public Enumerator(MostRecentStack<T> list)
            {
                this.list = list;
                curIndex = -1;
            }

            public bool MoveNext()
            {
                curIndex++;
                return (curIndex < list.count);
            }

            public void Reset()
            {
                curIndex = -1;
            }

            public T Current
            {
                get { return list.list[list.GetArrayIndex(curIndex)]; }
            }

            object System.Collections.IEnumerator.Current
            {
                get { return Current; }
            }

            void IDisposable.Dispose() { }
        }

        public void Push(T item)
        {
            OffsetHeadIndexBack();
            T previous = list[headIndex];
            list[headIndex] = item;

            if (count < capacity) {
                count++;
            } else {
                OnItemDropped(new ItemDroppedEventArgs(previous));
            }
        }

        public T Pop()
        {
            if (count > 0) {
                T previous = list[headIndex];
                OffsetHeadIndexFwd();
                count--;
                return previous;
            } else {
                throw new InvalidOperationException("Can't remove an item from an empty list.");
            }
        }

        public T ElementAt(int index)
        {
            if (index >= count) {
                string msg = String.Format("Attempted to access index {0} in a list with only {1} element(s).", index, count);
                throw new IndexOutOfRangeException(msg);
            } else {
                return list[GetArrayIndex(index)];
            }
        }

        public T this[int index]
        {
            get { return ElementAt(index); }
        }

        public int Count
        {
            get { return count; }
        }

        public void Clear()
        {
            headIndex = 0;
            count = 0;
        }

        public IEnumerator<T> GetEnumerator()
        {
            return new Enumerator(this);
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator()
        {
            return GetEnumerator();
        }
    }
}
