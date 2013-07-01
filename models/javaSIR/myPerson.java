/** Person representing a single agent in the SIR model. */
public class myPerson {
    // class constants
    /** Susceptible state */
    public static final byte stateS = 0;
    /** Infected state */
    public static final byte stateI = 1;
    /** Recovered state */
    public static final byte stateR = 2;

    // Instance Variables
    /** Current state of this person */
    private byte state;

    /** Construct new person with initial state.
     */
    public myPerson(byte initial) {
        state = initial;
    }

    /** Checks if person is susceptible.
     * @return true if person is susceptible */
    public boolean isSusceptible() { return state == stateS; }

    /** Checks if person is infected.
     * @return true if person is infected */
    public boolean isInfected() { return state == stateI; }

    /** Become infected. */
    public void infect() { state = stateI; }

    /** Recover from infection. */
    public void recover() { state = stateR; }
}
