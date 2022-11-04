import edu.princeton.cs.algs4.StdOut;
import edu.princeton.cs.algs4.StdRandom;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class RandomizedQueue<Item> implements Iterable<Item> {

    private ArrayList<Item> items;

    // construct an empty randomized queue
    public RandomizedQueue() {
        items = new ArrayList<>(8);
    }

    private void checkNotNull(Item item) {
        if (item == null) {
            throw new IllegalArgumentException();
        }
    }

    // is the randomized queue empty?
    public boolean isEmpty() {
        return items.isEmpty();
    }

    // return the number of items on the randomized queue
    public int size() {
        return items.size();
    }

    // add the item
    public void enqueue(Item item) {
        checkNotNull(item);
        items.add(item);
    }

    // remove and return a random item
    public Item dequeue() {
        if (isEmpty()) {
            throw new NoSuchElementException();
        }
        int index = StdRandom.uniformInt(0, size());
        Item item = items.get(index);
        int lastIdx = size() - 1;
        items.set(index, items.get(lastIdx));
        items.remove(lastIdx);
        return item;
    }

    // return a random item (but do not remove it)
    public Item sample() {
        if (isEmpty()) {
            throw new NoSuchElementException();
        }
        int index = StdRandom.uniformInt(0, size());
        return items.get(index);
    }

    private class RandomizedIterator implements Iterator<Item> {

        private final RandomizedQueue<Item> queueRef;

        RandomizedIterator(RandomizedQueue<Item> queue) {
            queueRef = queue;
        }

        public boolean hasNext() {
            return !queueRef.isEmpty();
        }

        public Item next() {
            if (!hasNext()) {
                throw new NoSuchElementException();
            }
            return queueRef.dequeue();
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    // return an independent iterator over items in random order
    public Iterator<Item> iterator() {
        return new RandomizedIterator(this);
    }

    // unit testing (required)
    public static void main(String[] args) {
        RandomizedQueue<Integer> queue = new RandomizedQueue<>();

        StdOut.printf("Queue starts out empty: %b%n", queue.isEmpty());
        StdOut.printf("Empty queue has size 0: %b%n", queue.size() == 0);

        queue.enqueue(100);
        StdOut.printf("enqueue increases queue size: %b; %b%n",
                      !queue.isEmpty(),
                      queue.size() == 1);

        queue.dequeue();
        StdOut.printf("dequeue decreases queue size: %b; %b%n",
                      queue.isEmpty(),
                      queue.size() == 0);

        queue.enqueue(200);
        queue.enqueue(100);
        Integer sampled = queue.sample();
        StdOut.printf("sample peeks a random item: %b%n", sampled != null);

        queue.enqueue(500);
        Integer dequeued = queue.dequeue();
        StdOut.printf("dequeue removes a random item: %d, %b%n",
                      dequeued,
                      queue.size() == 2);

        queue = new RandomizedQueue<>();
        int[] numbers = new int[10];
        for (int i = 0; i < numbers.length; i++) {
            numbers[i] = i;
            queue.enqueue(i);
        }

        int queueIteratorLength = 0;
        for (int ignored : queue) {
            queueIteratorLength++;
        }
        StdOut.printf("iterator length is same as size: %b%n",
                      queueIteratorLength == numbers.length);

    }
}
