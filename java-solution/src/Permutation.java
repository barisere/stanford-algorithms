/* *****************************************************************************
 *  Name:              Alan Turing
 *  Coursera User ID:  123456
 *  Last modified:     1/1/2019
 **************************************************************************** */

import edu.princeton.cs.algs4.StdIn;
import edu.princeton.cs.algs4.StdOut;

import java.util.NoSuchElementException;

public class Permutation {
    public static void main(String[] args) {
        int k = Integer.parseInt(args[0]);
        RandomizedQueue<String> queue = new RandomizedQueue<>();
        while (true) {
            try {
                queue.enqueue(StdIn.readString());
                if (queue.size() == k) {
                    while (!queue.isEmpty()) {
                        StdOut.println(queue.dequeue());
                    }
                }
            }
            catch (NoSuchElementException ex) {
                for (String s : queue) {
                    StdOut.println(s);
                }
                break;
            }
        }
    }
}
