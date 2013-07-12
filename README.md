disease-modeling
================

A repository for the disease modeling code the Mathematical Modeling group at the Mt. Holyoke 2013 REU

Agent Based C++ Hill Model
--------------------------

NOT YET WORKING. 

This model is a c++ implementation of an agent based, population level model of
US TB levels that also analyzes cost. To run, download the files, navigate to 
models/agentHillModel, run make, then run ./agentModel. To plot the results, 
run R < plotData.R --save --args true [deltaT], where [deltaT] is your timestep. 

Alternatively, you can run 'make plots DELTA\_T=[deltaT]' to compile, run, and 
plot the system. 
