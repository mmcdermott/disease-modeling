//  agentbased.cpp

#include <iostream>
#include <fstream>
#include <list>
#include <random>
#include <chrono>
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include <sstream>
#include "turtle_plusTreatment.hpp"
using namespace std;

const bool debug = false;

typedef list<turtle> turtleList;

double lambda0;
double lambda1;

const double discRate = 1.03;
const double popConst = 100; //For now
const int    finalYr  = 100;
const int    totT     = (int) (finalYr/DELTA_T);

//2010 New Cases in Population
//source: http://www.cdc.gov/mmwr/preview/mmwrhtml/mm5105a3.htm
//TODO upon moving over constants, fix them here.
const int newCases0 = 8714; //US-born
const int newCases1 = 7554; //Foreign-born
//Initial Populations--source: Hill Model.
const int initUSP   = (1./popConst)*250000000;                            
const int initFBP   = (1./popConst)*31400000;                             
const int initF0    = (1./popConst)*((1-r0)*(newCases0)/vF);              
const int initF1    = (1./popConst)*((1-r1)*(newCases1)/vF);              
const int initL0    = (1./popConst)*(r0*(newCases0)/vL0);                 
const int initL1    = (1./popConst)*(r1*(newCases1)/vL1);                 
const int initI0    = (1./popConst)*(q*newCases0/(mu0 + mud + phi0));     
const int initI1    = (1./popConst)*(q*newCases1/(mu0 + mud + phi1));     
const int initJ0    = (1./popConst)*((1-q)*newCases0/(mu0 + mud + phi0)); 
const int initJ1    = (1./popConst)*((1-q)*newCases1/(mu1 + mud + phi1)); 

turtleList population;

int N0[totT];
int S0[totT];
int L0[totT];
int F0[totT];
int J0[totT];
int I0[totT];
int N1[totT];
int S1[totT];
int L1[totT];
int F1[totT];
int I1[totT];
int J1[totT];
double activeCost[totT];  //cost of active TB treatments
double latentCost[totT];  //cost of latent TB treatments
double totalCost[totT];   //total cost of all TB treatments

int numNaturalDeaths[totT];          //number of natural deaths
int numTBDeaths[totT];               //number of TB deaths
int numAcuteProgressions[totT];      //number of active TB progressions from acute LTBI
int numChronicProgressions[totT];    //number of active TB progressions from chronic LTBI
//int numExogeneousReinfections[totT]; //number of exogeneous reinfections

/* 
 * void updatePop(...)
 *
 * This functions updates the population lists given the number of turtles of 
 * the given states. It returns nothing, as it is a state updating function.
 */
void updatePop(const turtle::State &turtState, const turtle::COB &cob, int timeStep, 
               int numTurtles = 1)
{
  switch(cob) {
    case turtle::USA:
      switch(turtState) {
        case turtle::CHRONIC_LTBI:
          L0[timeStep] += numTurtles;
          break;
        case turtle::ACUTE_LTBI:
          F0[timeStep] += numTurtles;
          break;
        case turtle::INFECTIOUS_TB:
          I0[timeStep] += numTurtles;
          break;
        case turtle::NONINFECTIOUS_TB:
          J0[timeStep] += numTurtles;
          break;
        case turtle::SUSCEPTIBLE:
          S0[timeStep] += numTurtles;
          break;
        case turtle::TB_DEATH:
          break;
        case turtle::NATURAL_DEATH:
          break;
      }
      break;
    case turtle::OTHER:
      switch(turtState) {
        case turtle::CHRONIC_LTBI:
          L1[timeStep] += numTurtles;
          break;
        case turtle::ACUTE_LTBI:
          F1[timeStep] += numTurtles;
          break;
        case turtle::INFECTIOUS_TB:
          I1[timeStep] += numTurtles;
          break;
        case turtle::NONINFECTIOUS_TB:
          J1[timeStep] += numTurtles;
          break;
        case turtle::SUSCEPTIBLE:
          S1[timeStep] += numTurtles;
          break;
        case turtle::TB_DEATH:
          break;
        case turtle::NATURAL_DEATH:
          break;
      }
      break;
  }
}

void createTurtles(const turtle::State &turtState, const turtle::COB &cob, int timeStep, int numTurtles)
{
  for (int i = 0; i < numTurtles; ++i) 
  {
    population.push_front(turtle(cob, turtState)); 
  }
  updatePop(turtState, cob, timeStep, numTurtles);
}

/** Export sizes of N,S,L,F,I,J to file fname (csv file) */
void exportData(string fname) {
  ofstream output;
  output.open(fname);
  output << "\"N0\",\"S0\",\"L0\",\"F0\",\"I0\",\"J0\",";
  output << "\"N1\",\"S1\",\"L1\",\"F1\",\"I1\",\"J1\",";
  output << "\"latCost\",\"actCost\",\"totCost\",";
  output << "\"numNatDeath\",\"numTBDeath\",\"numAcuteProg\",\"numChronProg\"" << endl;
  
  for (int i=0; i < totT; ++i) {
    output << N0[i] << "," << S0[i] << "," << L0[i] << "," << F0[i] << "," << I0[i] << "," << J0[i] << ",";
    output << N1[i] << "," << S1[i] << "," << L1[i] << "," << F1[i] << "," << I1[i] << "," << J1[i] << ",";
    output << activeCost[i] << "," << latentCost[i] << "," << totalCost[i] << ",";
    output << numNaturalDeaths[i] << "," << numTBDeaths[i] << "," << numAcuteProgressions[i] << "," << numChronicProgressions[i] << endl;
  }
  output.close();
}

int run(string rfname)
{
  unsigned seed = chrono::system_clock::now().time_since_epoch().count();
  default_random_engine generator (seed);
  srand(time(NULL));
  double newCost = 0;
  turtle::State prevState, nextState;

  N0[0] = initUSP;
  N1[0] = initFBP;
  //Acute (Fast) LTBI, new cases
  createTurtles(turtle::ACUTE_LTBI, turtle::USA, 0, initF0);
  createTurtles(turtle::ACUTE_LTBI, turtle::OTHER, 0, initF1);
  //Chronic (Long) LTBI
  createTurtles(turtle::CHRONIC_LTBI, turtle::USA, 0, initL0);
  createTurtles(turtle::CHRONIC_LTBI, turtle::OTHER, 0, initL1);
  //Infectious TB
  createTurtles(turtle::INFECTIOUS_TB, turtle::USA, 0, initI0);
  createTurtles(turtle::INFECTIOUS_TB, turtle::OTHER, 0, initI1);
  //Non-Infectious TB
  createTurtles(turtle::NONINFECTIOUS_TB, turtle::USA, 0, initJ0);
  createTurtles(turtle::NONINFECTIOUS_TB, turtle::OTHER, 0, initJ1);
  //Susceptible
  S0[0] = (N0[0] - F0[0] - L0[0] - I0[0] - J0[0]);
  S1[0] = (N1[0] - F1[0] - L1[0] - I1[0] - J1[0]);
  if (debug) {
    cout << "Number of Iterations: " << totT << endl;
  }
  for (int i = 1; i < totT; ++i){
    //Generating Preferred contact rate based on previous time step
    double c01 = (1-e0)*((1-e1)*N1[i-1])/((1-e0)*N0[i-1]+(1-e1)*N1[i-1]);
    double c00 = 1-c01;
    double c10 = (1-e1)*((1-e0)*N0[i-1])/((1-e0)*N0[i-1]+(1-e1)*N1[i-1]); 
    double c11 = 1-c10;
    //Generating lambda0 and lambda1 based on previous time step
    double lambda0 = beta * (c00*I0[i-1]/N0[i-1] + c01*I1[i-1]/N1[i-1]);
    double lambda1 = beta * (c10*I0[i-1]/N0[i-1] + c11*I1[i-1]/N1[i-1]);

    //TODO: Maybe use some kind of exponential, or reed frost, or something?
    double probOfReinfectionUSB = x * p * lambda0 * DELTA_T;
    double probOfReinfectionFB  = x * p * lambda1 * DELTA_T;
    //Debugging Info:
    if (debug)
    {
      cout << "iteration " << i << endl;
      cout << "number of Turtles " << population.size() << endl;
      cout << "Population Size " << N0[i-1] + N1[i-1] << endl;
    }

    for (turtleList::iterator turtleIter = population.begin(); 
         turtleIter != population.end(); ++turtleIter){
      turtle &t = *turtleIter;

      prevState = t.getState();
      //Update the turtle's state
      t.updateState();
      nextState = t.getState();
      if(nextState != prevState) {
        // Natural deaths, TB deaths, Progressions
        if(nextState == turtle::NATURAL_DEATH) {
          numNaturalDeaths[i]++;
        } else if (nextState == turtle::TB_DEATH) {
          numTBDeaths[i]++;
        } else if ((prevState == turtle::ACUTE_LTBI) &&
                  ((nextState == turtle::INFECTIOUS_TB) || (nextState == turtle::NONINFECTIOUS_TB))) {
          numAcuteProgressions[i]++;
        } else if ((prevState == turtle::CHRONIC_LTBI) &&
                  ((nextState == turtle::INFECTIOUS_TB) || (nextState == turtle::NONINFECTIOUS_TB))) {
          numChronicProgressions[i]++;
        }
      }

      //Update cost arrays
      newCost  = t.getCost();  //returns and resets turtle cost
      if(newCost >= ACTIVE_TREATMENT_COST_NOHOSPITAL)
        activeCost[i] += popConst*(newCost/pow(discRate,(i*DELTA_T)));
      else
        latentCost[i] += popConst*(newCost/pow(discRate,(i*DELTA_T))); 
            
      if (t.dead())
      {
        turtleList::iterator newIter = population.erase(turtleIter);
        turtleIter = --newIter;//TODO: store previous state in case of dead turtle for sake of updating pops
      } else {
        //Check for exogenous re-infection TODO: abstract this to a function
        double infParam = (double)rand()/(double)RAND_MAX;
        if ((t.getCountry() == turtle::USA) && (infParam <= probOfReinfectionUSB))
        {
          t.infect(); 
        } else if ((t.getCountry() == turtle::OTHER) 
                   && (infParam <= probOfReinfectionFB)) {
          t.infect();  
        }

        //Update our population lists
        updatePop(t.getState(), t.getCountry(), i);

        if (t.getState() == turtle::SUSCEPTIBLE) {
          turtleList::iterator newIter = population.erase(turtleIter);
          turtleIter = --newIter;
        }
      }
    }
    // Agent independent Population Changes during this time step: 
    // Note that these did not affect infection likelihood during
    // this time step.

    // US birth and death (-> S0)
    S0[i] += S0[i-1] + (int) floor(ro*(N0[i-1]+N1[i-1])*DELTA_T);
    S0[i] -= (int) floor(mu0*S0[i-1])*DELTA_T;
    // susceptible arrival (-> S1)
    S1[i] += S1[i-1] + (int) floor((1 - f) * alpha * (N0[i-1]+N1[i-1]) * DELTA_T);         
    S1[i] -= (int) floor(mu1*S1[i-1])*DELTA_T;
 	 	
    //Creating Binomial Distributions for generating new infections from S0/S1
    binomial_distribution<int> usInfec(S0[i-1], lambda0 * DELTA_T);
 	binomial_distribution<int> fbInfec(S1[i-1], lambda1 * DELTA_T);

    int numUSInfections = usInfec(generator);
    int newf0           = floor(p * numUSInfections);
    int newl0           = numUSInfections - newf0;
    S0[i]              -= numUSInfections;
		
    int numFBInfections = fbInfec(generator);
    int newf1           = floor(p * numFBInfections);
    int newl1           = numFBInfections - newf1;
    int LTBIArrivals    = floor(f * alpha * (N0[i-1] + N1[i-1]) * DELTA_T);
    int AcuteArrivals   = floor(g * p * LTBIArrivals);
    newf1              += AcuteArrivals;
    newl1              += LTBIArrivals - AcuteArrivals;
    S1[i]              -= numFBInfections;

    //Creating the turtles
    createTurtles(turtle::ACUTE_LTBI,   turtle::USA,   i, newf0);
    createTurtles(turtle::CHRONIC_LTBI, turtle::USA,   i, newl0);
    createTurtles(turtle::ACUTE_LTBI,   turtle::OTHER, i, newf1);
    createTurtles(turtle::CHRONIC_LTBI, turtle::OTHER, i, newl1);

    if (debug) {
      cout << "numUsInfections: " << numUSInfections << endl;
      cout << "numFBInfections: " << numFBInfections << endl;
      cout << "LTBIArrivals: " << LTBIArrivals << endl;
    }
    //Resetting the populations
    N0[i]    = S0[i] + F0[i] + L0[i] + I0[i] + J0[i];
    N1[i]    = S1[i] + F1[i] + L1[i] + I1[i] + J1[i];
    activeCost[i] += activeCost[i-1];  //all costs are cumulative
    latentCost[i] += latentCost[i-1];
    totalCost[i] = activeCost[i] + latentCost[i];
  }
  // write data to file
  //exportData("modelData.csv");
  exportData(rfname);
  return 0;
}

int main(int argc, char const *argv[])
{
  int numruns = 1;//54; //number of runs performed
  int runnumber = ((argc > 1) ? atoi(argv[1]) : 1); // takes in the number of the run happening
  int adjustedi;
  string filename;

  for (int i = 1 ; i <= numruns; ++i) {
    stringstream sstm;
    filename = "";
    adjustedi = i + (numruns*(runnumber-1));
    sstm << "modelDataRun" << adjustedi << ".csv";
    filename = sstm.str();
    run(filename);
    population.clear();
    for (int j = 0; j < totT; ++j) {
      N0[j] = 0;
      S0[j] = 0;
      L0[j] = 0;
      F0[j] = 0;
      J0[j] = 0;
      I0[j] = 0;
      N1[j] = 0;
      S1[j] = 0;
      L1[j] = 0;
      F1[j] = 0;
      I1[j] = 0;
      J1[j] = 0;
      activeCost[j] = 0;
      latentCost[j] = 0;
      totalCost[j] = 0;
      numNaturalDeaths[j] = 0;
      numTBDeaths[j] = 0;
      numAcuteProgressions[j] = 0;
      numChronicProgressions[j] = 0;
    }
  }
  return 0;
}
