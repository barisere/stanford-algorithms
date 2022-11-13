import edu.princeton.cs.algs4.StdOut;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.NoSuchElementException;

public class Deque<Item> implements Iterable<Item> {

    private ArrayList<Item> fore = new ArrayList<>(4);
    private ArrayList<Item> rear = new ArrayList<>(4);

    private void checkNotNull(Item item) {
        if (item == null) {
            throw new IllegalArgumentException();
        }
    }

    // is the deque empty?
    public boolean isEmpty() {
        return fore.isEmpty() && rear.isEmpty();
    }

    // return the number of items on the deque
    public int size() {
        return fore.size() + rear.size();
    }

    // add the item to the front
    public void addFirst(Item item) {
        checkNotNull(item);
        fore.add(item);
    }

    // add the item to the back
    public void addLast(Item item) {
        checkNotNull(item);
        rear.add(item);
    }

    // remove and return the item from the front
    public Item removeFirst() {
        ArrayList<Item> source = fore.isEmpty() ? rear : fore;
        if (source.isEmpty()) {
            throw new NoSuchElementException();
        }
        int idx = source == fore ? source.size() - 1 : 0;
        return removeAt(source, idx);
    }

    // remove and return the item from the back
    public Item removeLast() {
        ArrayList<Item> source = rear.isEmpty() ? fore : rear;
        if (source.isEmpty()) {
            throw new NoSuchElementException();
        }
        int idx = source == rear ? source.size() - 1 : 0;
        return removeAt(source, idx);
    }

    private Item removeAt(ArrayList<Item> source, int idx) {
        Item item = source.get(idx);
        source.remove(idx);
        return item;
    }

    // return an iterator over items in order from front to back
    public Iterator<Item> iterator() {
        return new DequeIterator<>(this);
    }

    private class DequeIterator<Item> implements Iterator<Item> {
        private final Deque<Item> deque;

        public DequeIterator(Deque<Item> queue) {
            deque = queue;
        }

        public boolean hasNext() {
            return !deque.isEmpty();
        }

        public Item next() {
            return deque.removeFirst();
        }

        public void remove() {
            throw new UnsupportedOperationException();
        }
    }

    // unit testing (required)
    public static void main(String[] args) {
        Deque<Integer> queue = new Deque<>();

        StdOut.printf("Queue starts out empty: %b%n", queue.isEmpty());
        StdOut.printf("Empty queue has size 0: %b%n", queue.size() == 0);

        queue.addFirst(100);
        StdOut.printf("addFirst increases queue size: %b%n", queue.size() == 1);

        queue.removeFirst();
        StdOut.printf("removeFirst decreases queue size: %b%n", queue.size() == 0);

        queue.addLast(100);
        StdOut.printf("addLast increases queue size: %b%n", queue.size() == 1);

        queue.removeLast();
        StdOut.printf("removeLast decreases queue size: %b%n", queue.size() == 0);

        queue.addLast(200);
        queue.addLast(100);
        StdOut.printf("removeLast returns the last item added using addLast: %b%n",
                      queue.removeLast() == 100);

        queue.addFirst(200);
        queue.addFirst(300);
        StdOut.printf("removeFirst returns the last item added using addFirst: %b%n",
                      queue.removeFirst() == 300);

        queue = new Deque<>();
        queue.addFirst(200);
        queue.addFirst(300);
        queue.addFirst(400);
        queue.addFirst(500);
        queue.removeLast();
        queue.removeLast();
        queue.removeLast();
        queue.removeLast();
        StdOut.printf("queue is empty after enqueuing in front and dequeuing from behind: %b%n",
                      queue.isEmpty());
        try {
            Integer i = queue.removeLast();
            StdOut.printf("removeLast from empty queue did not throw exception, returned: %d%n", i);
        }
        catch (NoSuchElementException exception) {
            StdOut.println("removeLast from empty queue throws NoSucnElementException exception");
        }
        try {
            Integer i = queue.removeFirst();
            StdOut.printf("removeFirst from empty queue did not throw exception, returned: %d%n",
                          i);
        }
        catch (NoSuchElementException exception) {
            StdOut.println("removeFirst from empty queue throws NoSucnElementException exception");
        }

        queue = new Deque<>();
        int[] numbers = new int[8];
        for (int i = 0; i < numbers.length; i++) {
            numbers[i] = i;
            queue.addFirst(i);
            queue.addLast(i);
        }
        for (int i = numbers.length - 1; i >= 0; i--) {
            int n = numbers[i];
            StdOut.printf("draining queue from both sides preserves items order: %b%n",
                          queue.removeLast() == n && queue.removeFirst() == n);
        }

        Deque<Integer> finalQueue = new Deque<>();
        for (int n : numbers) {
            finalQueue.addLast(n);
        }
        int[] finalQueueConsumed = new int[numbers.length];
        int idx = 0;
        for (int i : finalQueue) {
            finalQueueConsumed[idx++] = i;
        }
        boolean arraysEqual = true;
        for (int i = 0; i < numbers.length; i++) {
            arraysEqual = arraysEqual && numbers[i] == finalQueueConsumed[i];
        }
        StdOut.printf("iterator returned elements in order: %b%n", arraysEqual);
    }

}
