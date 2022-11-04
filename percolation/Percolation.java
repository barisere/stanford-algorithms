import edu.princeton.cs.algs4.WeightedQuickUnionUF;

public class Percolation {

    private static final int TOP = 0;
    private final int size;
    private final int bottom;
    private final boolean[][] opened;
    private final WeightedQuickUnionUF wqu;
    private int nOpen = 0;

    // creates n-by-n grid, with all sites initially blocked
    public Percolation(int n) {
        if (n < 1) {
            throw new IllegalArgumentException();
        }
        size = n;
        opened = new boolean[n][n];
        bottom = n * n + 1;
        wqu = new WeightedQuickUnionUF(n * n + 2);
    }

    private void assertValidIndex(int row, int col) {
        if (row < 1 || row > size || col < 1 || col > size) {
            throw new IllegalArgumentException();
        }
    }

    // opens the site (row, col) if it is not open already
    public void open(int row, int col) {
        assertValidIndex(row, col);
        opened[row - 1][col - 1] = true;
        nOpen++;
        int effectiveIndex = indexInWqu(row, col);
        if (row == 1) {
            wqu.union(effectiveIndex, TOP);
        }
        if (row == size) {
            wqu.union(effectiveIndex, bottom);
        }
        if (row > 1 && isOpen(row - 1, col)) {
            wqu.union(effectiveIndex, indexInWqu(row - 1, col));
        }
        if (row < size && isOpen(row + 1, col)) {
            wqu.union(effectiveIndex, indexInWqu(row + 1, col));
        }
        if (col > 1 && isOpen(row, col - 1)) {
            wqu.union(effectiveIndex, indexInWqu(row, col - 1));
        }
        if (col < size && isOpen(row, col + 1)) {
            wqu.union(effectiveIndex, indexInWqu(row, col + 1));
        }
    }

    // is the site (row, col) open?
    public boolean isOpen(int row, int col) {
        assertValidIndex(row, col);
        return opened[row - 1][col - 1];
    }

    // is the site (row, col) full?
    public boolean isFull(int row, int col) {
        assertValidIndex(row, col);
        return wqu.find(indexInWqu(row, col)) == wqu.find(TOP);
    }

    private int indexInWqu(int row, int col) {
        return size * (row - 1) + col;
    }

    // returns the number of open sites
    public int numberOfOpenSites() {
        return nOpen;
    }

    // does the system percolate?
    public boolean percolates() {
        return wqu.find(TOP) == wqu.find(bottom);
    }

    // test client (optional)
    public static void main(String[] args) {

    }
}
