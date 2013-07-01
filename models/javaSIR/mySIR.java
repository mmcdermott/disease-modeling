// required for HashSet, HashMap, ArrayList, FileWriter, and IOException
import java.util.*;
import java.io.*;

/** A simple stochastic agent-based SIR model.
 * You might get a warning when you compile this because I make a
 * cast from clone() on lines 49 and 84. The warning can be
 * safely ignored.
 */
public class mySIR {
    /** size of population */
    public static final int N = 1000;
    /** number of time steps to run */
    public static final int T = 100;
    /** "infection" paramter */
    public static final double a = 0.5;
    /** "recovery" parameter */
    public static final double b = 0.3;
    /** number of initially infected people */
    public static final int INITIAL_INFECTED = 1;

    /** Main method */
    public static void main(String[] args) {
        // Create initial population of people
        // Use a set data structure since we don't care about order
        // and we want each person to be unique
        HashSet<myPerson> people = new HashSet<myPerson>();
        for(int i = 0; i < N-INITIAL_INFECTED; i++) {
            people.add(new myPerson(myPerson.stateS));
        }
        for(int i = 0; i < INITIAL_INFECTED; i++) {
            people.add(new myPerson(myPerson.stateI));
        }

        // initialize population vars
        HashMap<String,Integer> currPop = new HashMap<String,Integer>();
        currPop.put("S", N-INITIAL_INFECTED);
        currPop.put("I", INITIAL_INFECTED);
        currPop.put("R", 0);

        // initialize probability vectors
        // here it's an array of length 1 since there's only one state
        // you can transition to, for both S and I
        double[] Sprob = new double[1];
        double[] Iprob = new double[1];

        // initialize results
        ArrayList<HashMap<String,Integer>> popData = new ArrayList<HashMap<String,Integer>>();
        popData.add((HashMap<String,Integer>)currPop.clone());

        // run the following T times...
        for(int i = 0; i < T; i++) {
            // Update transition probabilities
            Sprob[0] = a*currPop.get("I")/N;
            Iprob[0] = b;
            // initialize sampler given Sprob and Iprob
            mySampler sampS = new mySampler(Sprob);
            mySampler sampI = new mySampler(Iprob);

            // Iterate over all people
            Iterator<myPerson> pitr = people.iterator();
            while( pitr.hasNext() ) {
                myPerson agent = pitr.next();
                // If agent is in S...
                if (agent.isSusceptible()) {
                    // infect if sampler-S gives us index 0
                    if (sampS.nextIndex() == 0) {
                        agent.infect();
                        currPop.put("S", currPop.get("S")-1);
                        currPop.put("I", currPop.get("I")+1);
                    }
                // If agent is in I...
                } else if (agent.isInfected()) {
                    // infect if sampler-I gives us index 0
                    if (sampI.nextIndex() == 0) {
                        agent.recover();
                        currPop.put("I", currPop.get("I")-1);
                        currPop.put("R", currPop.get("R")+1);
                    }
                }
            }

            // Record populations
            popData.add((HashMap<String,Integer>)currPop.clone());
        }

        // Write data to csv file
        try {
            FileWriter output = new FileWriter("SIRdata.csv");
            output.write("\"S\",\"I\",\"R\"\n");
            for(HashMap<String,Integer> vec : popData) {
                output.write(vec.get("S") + "," + vec.get("I") + "," + vec.get("R") + "\n");
            }
            output.close();
        // handle errors gracefully
        } catch (IOException ioe) {
            System.out.println(ioe);
            System.exit(1);
        }
    }
}
