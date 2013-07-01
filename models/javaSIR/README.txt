A quick and dirty stochastic agent-based SIR model using Java.
Author: Ryo Kimura, Mount Holyoke REU 2013

# Files #
README.txt     - this file that you're reading right now

# Programs #
mySIR.java     - main program (you can ignore the compilation warnings)
myPerson.java  - deffines a class for individual agents in model
mySampler.java - defines a class for sampling from user-specified
                 probability distributions
plotCSV.R      - plots data from SIRdata.csv

# Sample Output #
SIRdata.csv    - sample output from mySIR.java
Rplots.pdf     - sample output form plotCSV.R

# Basic usage
java mySIR.java
java mySIR
R
> source('plotCSV.R')
