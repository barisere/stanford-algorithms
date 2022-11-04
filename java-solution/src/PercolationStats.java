/* *****************************************************************************
 *  Name:              Alan Turing
 *  Coursera User ID:  123456
 *  Last modified:     1/1/2019
 **************************************************************************** */

import edu.princeton.cs.algs4.StdOut;
import edu.princeton.cs.algs4.StdRandom;
import edu.princeton.cs.algs4.StdStats;

public class PercolationStats {

    private static final double CONFIDENCE_95 = 1.96;
    private final int trials;
    private final int gridSize;
    private final double[] thresholds;
    private int numberOfSites;

    // perform independent trials on an n-by-n grid
    public PercolationStats(int n, int trials) {
        if (n < 1 || trials < 1) {
            throw new IllegalArgumentException();
        }
        gridSize = n;
        numberOfSites = gridSize * gridSize;
        this.trials = trials;
        this.thresholds = new double[trials];
    }

    // sample mean of percolation threshold
    public double mean() {
        return StdStats.mean(thresholds);
    }

    // sample standard deviation of percolation threshold
    public double stddev() {
        return StdStats.stddev(thresholds);
    }

    // low endpoint of 95% confidence interval
    public double confidenceLo() {
        return mean() - (CONFIDENCE_95 * stddev() / Math.sqrt(trials));
    }

    // high endpoint of 95% confidence interval
    public double confidenceHi() {
        return mean() + (CONFIDENCE_95 * stddev() / Math.sqrt(trials));
    }

    private void run() {
        for (int i = 0; i < trials; i++) {
            Percolation p = new Percolation(gridSize);
            while (!p.percolates()) {
                int row = StdRandom.uniformInt(1, gridSize + 1);
                int col = StdRandom.uniformInt(1, gridSize + 1);
                p.open(row, col);
            }
            thresholds[i] = (double) p.numberOfOpenSites() / numberOfSites;
        }
    }

    // test client (see below)
    public static void main(String[] args) {
        int gridSize = Integer.parseInt(args[0]);
        int trials = Integer.parseInt(args[1]);

        PercolationStats stats = new PercolationStats(gridSize, trials);
        StdOut.printf("mean                     = %f%n", stats.mean());
        StdOut.printf("stddev                   = %f%n", stats.stddev());
        StdOut.printf("95%% confidence interval = [%f, %f]%n",
                      stats.confidenceLo(),
                      stats.confidenceHi());
    }

}
