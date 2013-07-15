disease-modeling
================

A repository for the disease modeling code the Mathematical Modeling group at the Mt. Holyoke 2013 REU. All our finished products can be found in the 'deliverables' folder. This folder currently contains:

#### Delivarables:
1. agentBased/
    * hillModel_basic.nlogo

        This is a netlogo, agent-based implementation of the Hill Model [1].
Probabilities of infection are determined based on an Eulerian approximation to
the Differential Equation System and a binomial approximation is used to compute
the number of next infections. It matches the deterministic system.


Agent Based C++ Hill Model
--------------------------

NOT YET WORKING. 

This model is a c++ implementation of an agent based, population level model of
US TB levels that also analyzes cost. To run, download the files, navigate to 
models/agentHillModel, run make, then run ./agentModel. To plot the results, 
run R < plotData.R --save --args true [deltaT], where [deltaT] is your timestep. 

Alternatively, you can run 'make plots DELTA\_T=[deltaT]' to compile, run, and 
plot the system. 
