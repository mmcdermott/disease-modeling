disease-modeling
================

A repository for the disease modeling code the Mathematical Modeling group at the Mt. Holyoke 2013 REU. All our finished products can be found in the 'deliverables' folder. This folder currently contains:

#### Deliverables:
1. agentBased/
    * hillModel_basic.nlogo

        This is a netlogo, agent-based implementation of the Hill Model [1].
Probabilities of infection are determined based on an Eulerian approximation to
the Differential Equation System and a binomial approximation is used to compute
the number of next infections. It matches the deterministic system well. It
currently does not support cost estimations.
    * agentbasedStrictHill
        This is a c++ implementation of the agent based netlogo implementation
described above. It is configured to run 54 runs upon execution, and store the
results in the data directory for future viewing. The plotData.R script then
reads in this data and plots it. To run this version, run 
'make clean' then 'make' then ./agentModelStrictHill i, where i is the index at
which you want your file names to begin (times 54). This setup allows you to
easily run multiple runs. Lowering popConst, the number of people each agent
represents in agentbasedStrictHill.cpp will provide better results, but at the
cost of greater computational load. The graph 'finalRun.pdf' was compiled using
nearly 2000 runs of the model with each agent representing 1 true individual,
and a time constant of 0.05
    * Upcoming: 
        + agentBased.cpp
            An agent based implementation of the Hill Model [1] in c++, analyzing TB on the population level while assessing intervention cost effectiveness.
2. costBenefitAnalysis/
    * costBenefitAnalysis.R
        An R script to estimate the cost effectiveness of various intervention
strategies for controlling US TB level. The model is a re-implementation of the
Hill Model [1], extended to provide conservative estimates for total costs
incurred with a variety of intervention styles. Of note are the three plots as
well, which detail the cost effectiveness of intervening via reducing incoming
LTBI among immigrating populations. These are especially interesting given that
the results of the basic model show us that the most effective intervention
strategy is reducing the incoming LTBI population.

3. basicHillModel
    * lsodaHillModel.R
        An R implementation of the Hill Model [1] using R, with DE approximation routine lsoda.

Upcoming:
=========

Agent Based C++ Hill Model
--------------------------

NOT YET WORKING. 

This model is a c++ implementation of an agent based, population level model of
US TB levels that also analyzes cost. To run, download the files, navigate to 
models/agentHillModel, run make, then run ./agentModel. To plot the results, 
run R < plotData.R --save --args true [deltaT], where [deltaT] is your timestep. 

Alternatively, you can run 'make plots DELTA\_T=[deltaT]' to compile, run, and 
plot the system. 


Citations:
==========

[1] Hill, A. N., Becerra, J. E., & Castro, K. G. (2012). Modelling tuberculosis
trends in the USA. Epidemiology and infection, 140(10), 1862.
