import java.util.Random;

/** Object for sampling indices given a probability distribution.
 * This is best explained using an example. If you call
 *
 * double[] probDist = {0.2, 0.5, 0.05};
 * mySampler gen = new mySampler(probDist);
 *
 * then gen.nextIndex() will generate a random index from 0-3 with the
 * following distribution:
 * index | P(index)
 * ------+---------
 *  0    | 20%
 *  1    | 50%
 *  2    |  5%
 *  3    | 25%
 *
 */
public class mySampler {
    /** Cumulative probability distribution from which
     * samples are drawn */
    private double[] probDist;
    /** Internal random number generator */
    private Random rand;

    /** Constructs a new mySampler using the pdf vector probs
     * @param probs vector of probabilities
     */
    public mySampler(double[] probs) {
        // build cumulative probability distribution
        probDist = new double[probs.length];
        probDist[0] = probs[0];
        for(int i = 1; i < probs.length; i++) {
            probDist[i] = probDist[i-1] + probs[i];
        }
        // initialize random number generator
        rand = new Random();
    }

    /** Returns an index given by this sampler's cdf */
    public int nextIndex() {
        double num = rand.nextDouble();
        // return the index...
        for (int i = 0; i < probDist.length; i++) {
            if(num < probDist[i])
                return i;
        }
        // or the length of the vector
        return probDist.length;
    }
}
