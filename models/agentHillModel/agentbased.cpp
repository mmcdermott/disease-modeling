//
//  agentbased.cpp
//  
//
//  Created by mhcuser on 6/28/13.
//
//

#include <iostream>
#include <vector>
#include <list>
#include "agentbased.h"
#include "turtle.h"
using namespace std;

typedef list<double> turtleList;

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
const double mud     = 0.115;     // Mortality rate due to TB per year
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
double deltaS0;
double deltaS1;
double totpop;
double lambda0;
double lambda1;

//2010 New Cases in Population
//source: http://www.cdc.gov/mmwr/preview/mmwrhtml/mm5105a3.htm
const int newCases0 = 8714  //US-born
const int newCases1 = 7554  //Foreign-born

turtleList population; //TODO: Make this turtles, once the turtle class is defined. 


const double popConst = 1000; //For now
int finalYr = 100;
double deltaT = .1;
double totT = finalYr/deltaT;

int N0[totT];
N0.fill(0);
int L0[totT];
L0.fill(0);
int F0[totT];
F0.fill(0); //TODO: deleted later
int J0[totT];
J0.fill(0);
int I0[totT];
I0.fill(0);
int N1[totT];
N1.fill(0);
int L1[totT];
L1.fill(0);
int F1[totT];
F1.fill(0); //TODO: deleted later
int I1[totT];
I1.fill(0);
int J1[totT];
J1.fill(0);
double[totT] cost;

int main()
{  
	N0[0] = (250000000); //now in millions because agents
	N1[0] = (31400000);
	//Acute (Fast) LTBI, new cases
	F0[0] = ((1-r0)*(newCases0)/vF);
	F1[0] = ((1-r1)*(newCases1)/vF);
	//Chronic (Long) LTBI
	L0[0] = (r0*(newCases0)/vL0);
	L1[0] = (r1*(newCases1)/vL1);
	//Infectious TB
	I0[0] = (q*newCases0/(mu0 + mud + phi0));
	I1[0] = (q*newCases1/(mu1 + mud + phi1));
	//Non-Infectious TB
	J0[0] = ((1-q)*newCases0/(mu0 + mud + phi0));
	J1[0] = ((1-q)*newCases1/(mu1 + mud + phi1));
	//Susceptible
	S0[0] = (N0[0] - F0[0] - L0[0] - I0[0] - J0[0]);
	S1[0] = (N1[0] - F1[0] - L1[0] - I1[0] - J1[0]);
	int turtlepopsize = 0;

	for (int i = 0; i < totT; ++i)
	{

		//turtlepopsize = F0[i]+L0[i]+I0[i]+J0[i]+F1[i]+L1[i]+I1[i]+J1[i];
		for (turtleList::iterator turtleIter = population.begin(); 
			turtleIter != population.end(); ++turtleIter)
		{
			/* code */
		}
		S0[i+1] = S0[i] + (int) floor(ro * (N0[i]+N1[i]) * deltaT);                       // US birth (-> S0)
		S0[i+1] -= (int) floor(mu0*S0[i]);
		S1[i+1] = S1[i] + (int) floor((1 - f) * alpha * (N0[i]+N1[i]) * deltaT);         // susceptible arrival (-> S1)
		S1[i+1] -= (int) floor(mu1*S1[i]);
  		F1[i+1] = F1[i] + (int) floor(g * p * f * alpha * (N0[i]+N1[i]) * deltaT);       // acute LTBI arrival (-> F1)
 	 	L1[i+1] = L1[i] + (int) floor((1 - g * p) * f * alpha * (N0[i]+N1[i]) * deltaT); // latent LTBI arrival (-> L1)

 	 	let usinfec random-poisson (lambda0 * S0pop * deltaT)
  let newf0 round (p * usinfec)
  let newl0 round ((1 - p) * usinfec)
  set deltaS0 deltaS0 - newf0 - newl0

		N0[i+1] = S0[i] + F0[i] + L0[i] + I0[i] + J0[i];
		N1[i+1] = S1[i] + F1[i] + L1[i] + I1[i] + J1[i];


	}
    
    return 0;
}
