//
//  agentbased.cpp
//  
//
//  Created by mhcuser on 6/28/13.
//
//

#include <iostream>
#include <list>
#include <random>
#include <chrono>
#include <math.h>
#include "turtle.hpp"
using namespace std;

const bool debug = false;

typedef list<turtle> turtleList; //TODO: Make this turtles, once the turtle class is defined. 

const double ro      = 0.018;     // USB birth rate per year
const double alpha   = 0.005;     // FB birth rate per year
const double p       = 0.103;     // Fraction of new infectionsn which are acute
                                  //  (fast progressors)
const double vF      = 1.5;       // Progression of acute infection per year
const double l0      = 0.015;     // Prevalence of LTBI in USB in 2000
const double l1      = 0.211;     // Prevalence of LTBI in FB  in 2000
const double r0      = 0.667;     // Fraction of cases due to reactivation in the USB population
const double r1      = 0.780;     // Fraction of cases due to reactivation in the FB population
const double vL0     = 0.0014;    // Progression rate for reactivation (chronic LTBI) in the USB population per year
const double vL1     = 0.0010;    // Progression rate for reactivation (chronic LTBI) in the FB population per year
const double q       = 0.708;     // Fraction of infections progressing to infectious disease
const double x       = 0.111;     // Fraction of re-infected chronic LTBI moving to acute infection
const double f       = 0.187;     // Fraction of FB arrivals with LTBI
const double ARI0    = 0.030/100; // Annual risk of infection for USB in 2000
const double beta    = 10.39;     // Effective contact rate per year
const double e0      = 0.965;     // Fraction of preferred contacts with own population for USB
const double e1      = 0.985;     // Fraction of preferred contacts with own population for FB
const double g       = 0.0047;    // Fraction of FB arrivals with LTBI who are fast progressors
const double phi0    = 1.114;     // Cumulative fraction self-cure and treatment of active disease for both populations per year RATES (USB)
const double phi1    = 1.167;     // Cumulative fraction self-cure and treatment of active disease for both populations per year RATES (FB)
const double sigmaF0 = 1.296;     // Cumulative fraction of treatment for acute infection for both populations per year RATES (USB)
const double sigmaF1 = 1.301;     // Cumulative fraction of treatment for acute infection for both populations per year RATES (FB)

double sigmaL        = 0.057;     // Treatment rate for chronic LTBI per year
double lambda0;
double lambda1;

const double popConst = 1000; //For now
const int    finalYr  = 100;
const double deltaT   = .1;
const int    totT     = (int) (finalYr/deltaT);

//2010 New Cases in Population
//source: http://www.cdc.gov/mmwr/preview/mmwrhtml/mm5105a3.htm
const int newCases0 = 8714; //US-born
const int newCases1 = 7554; //Foreign-born
//Initial Populations--source: Hill Model.
const int initUSP   = (1./popConst)*250000000;                            
const int initFBP   = (1./popConst)*31400000;                             
const int initF0    = (1./popConst)*((1-r0)*(newCases0)/vF);              
const int initF1    = (1./popConst)*((1-r1)*(newCases1)/vF);              
const int initL0    = (1./popConst)*(r0*(newCases0)/vL0);                 
const int initL1    = (1./popConst)*(r1*(newCases1)/vL1);                 
const int initI0    = (1./popConst)*(q*newCases0/(MU0 + MUD + phi0));     
const int initI1    = (1./popConst)*(q*newCases1/(MU0 + MUD + phi1));     
const int initJ0    = (1./popConst)*((1-q)*newCases0/(MU0 + MUD + phi0)); 
const int initJ1    = (1./popConst)*((1-q)*newCases1/(MU1 + MUD + phi1)); 

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
double cost[totT];

int turtlepopsize = 0;

unsigned seed = chrono::system_clock::now().time_since_epoch().count();
default_random_engine generator (seed);

/* 
 * void updatePop(...)
 *
 * This functions updates the population lists given the number of turtles of 
 * the given states. It returns nothing, as it is a state updating function.
 */
void updatePop(turtle::State turtState, turtle::COB cob, int timeStep, 
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
      }
      break;
  }
}

void createTurtles(turtle::State turtState, turtle::COB cob, int timeStep, int numTurtles)
{
  for (int i = 0; i < numTurtles; ++i) 
  {
    population.push_front(turtle(cob, turtState)); 
  }
  updatePop(turtState, cob, timeStep, numTurtles);
}

int main()
{  
  N0[0] = initUSP;
  N1[0] = initFBP;
  //Acute (Fast) LTBI, new cases
  F0[0] = initF0;
  createTurtles(turtle::ACUTE_LTBI, turtle::USA, 0, initF0);
  F1[0] = initF1;
  createTurtles(turtle::ACUTE_LTBI, turtle::OTHER, 0, initF1);
  //Chronic (Long) LTBI
  L0[0] = initL0;
  createTurtles(turtle::CHRONIC_LTBI, turtle::USA, 0, initL0);
  L1[0] = initL1;
  createTurtles(turtle::CHRONIC_LTBI, turtle::OTHER, 0, initL1);
  //Infectious TB
  I0[0] = initI0;
  createTurtles(turtle::INFECTIOUS_TB, turtle::USA, 0, initI0);
  I1[0] = initI1;
  createTurtles(turtle::INFECTIOUS_TB, turtle::OTHER, 0, initI1);
  //Non-Infectious TB
  J0[0] = initJ0;
  createTurtles(turtle::NONINFECTIOUS_TB, turtle::USA, 0, initJ0);
  J1[0] = initJ1;
  createTurtles(turtle::NONINFECTIOUS_TB, turtle::OTHER, 0, initJ1);
  //Susceptible
  S0[0] = (N0[0] - F0[0] - L0[0] - I0[0] - J0[0]);
  S1[0] = (N1[0] - F1[0] - L1[0] - I1[0] - J1[0]);
  if (debug) {
    cout << "Number of Iterations: " << totT << endl;
  }
	for (int i = 1; i < totT; ++i)
	{
    //Generating Preferred contact rate based on previous time step
    double c00 = (1-e0)*((1-e1)*N1[i-1])/((1-e0)*N0[i-1]+(1-e1)*N1[i-1]);
    double c01 = 1-c00;
    double c10 = (1-e1)*((1-e0)*N0[i-1])/((1-e0)*N0[i-1]+(1-e1)*N1[i-1]); 
    double c11 = 1-c10;
    //Generating lambda0 and lambda1 based on previous time step
    double lambda0 = beta * (c00*(I0[i-1]/N0[i-1]) + c01*(I1[i-1]/N1[i-1]));
    double lambda1 = beta * (c10*(I0[i-1]/N0[i-1]) + c11*(I1[i-1]/N1[i-1]));

    double probOfReinfectionUSB = lambda0 * deltaT;
    double probOfReinfectionFB  = lambda1 * deltaT;

    //Debugging Info:
    if (debug)
    {
      cout << "iteration " << i << endl;
      cout << "number of Turtles " << population.size() << endl;
      cout << "Population Size " << N0[i-1] + N1[i-1] << endl;
    }

		for (turtleList::iterator turtleIter = population.begin(); 
			turtleIter != population.end(); ++turtleIter)
		{
      //TODO: Make updatePop respect dead states and appropriately subtract from total Pop
      // then reorganize so this calls updatePop as soon as possible. 
		  turtle t = *turtleIter;
      //Update the turtle's state
      t.updateState();
      if ((t.getState() == turtle::TB_DEATH) || (t.getState() == turtle::NATURAL_DEATH))
      {
        turtleList::iterator newIter = population.erase(turtleIter);
        turtleIter = --newIter;
      } else {
        //Check for exogenous re-infection TODO: abstract this to a function
        double infParam = (double)rand()/(double)RAND_MAX;
        if ((t.getCountry() == turtle::USA) && (infParam <= probOfReinfectionUSB))
        {
          t.infect(false); //TODO: Decide if J vs I really belongs in this fn
        } else if ((t.getCountry() == turtle::OTHER) 
                   && (infParam <= probOfReinfectionFB)) {
          t.infect(false); //TODO: See above 
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
    //
    // US birth and death (-> S0)
		S0[i]  = S0[i-1] + (int) floor(ro*(N0[i-1]+N1[i-1])*deltaT);                      
		S0[i] -= (int) floor(MU0*S0[i-1])*deltaT;
    // susceptible arrival (-> S1)
		S1[i]  = S1[i-1] + (int) floor((1 - f) * alpha * (N0[i-1]+N1[i-1]) * deltaT);         
		S1[i] -= (int) floor(MU1*S1[i-1])*deltaT;
 	 	
    //Creating Binomial Distributions for generating new infections from S0/S1
 	 	binomial_distribution<int> usInfec(S0[i-1], lambda0 * deltaT);
 	 	binomial_distribution<int> fbInfec(S1[i-1], lambda1 * deltaT);

    int numUSInfections = usInfec(generator);
    int newf0           = floor(p * numUSInfections);
    int newl0           = numUSInfections - newf0;
    S0[i]              -= numUSInfections;
		
    int numFBInfections = fbInfec(generator);
    int newf1           = floor(p * numFBInfections);
    int newl1           = numFBInfections - newf1;
    int LTBIArrivals    = floor(f * alpha * (N0[i-1] + N1[i-1]));
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
		N0[i] = S0[i] + F0[i] + L0[i] + I0[i] + J0[i];
		N1[i] = S1[i] + F1[i] + L1[i] + I1[i] + J1[i];
	}
  return 0;
}
