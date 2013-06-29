//
//  agentbased.cpp
//  
//
//  Created by mhcuser on 6/28/13.
//
//
using namespace std;
#include <iostream>
#include "agentbased.h"

double mu0;     //Natural mortality rate USB per year
double mu1;     // Natural mortality rate FB per year
double ro;      // USB birth rate per year
double alpha;   // FB birth rate per year
double p;       // Fraction of new infectionsn which are acute (fast progressors)
double vF;      // Progression rate of acute infection per year
double l0;      // Prevalence of LTBI in the USB population in 2000
double l1;      // Prevalence of LTBI in the FB population in 2000
double r0;      // Fraction of cases due to reactivation in the USB population
double r1;      // Fraction of cases due to reactivation in the FB population
double vL0;     // Progression rate for reactivation (chronic LTBI) in the USB population per year
double vL1;     // Progression rate for reactivation (chronic LTBI) in the FB population per year
double q;       // Fraction of infections progressing to infectious disease
double mud;     // Mortality rate due to TB per year
double x;       // Fraction of re-infected chronic LTBI moving to acute infection
double f;       // Fraction of FB arrivals with LTBI
double ARI0;    // Annual risk of infection for USB in 2000
double beta;    // Effective contact rate per year
double e0;      // Fraction of preferred contacts with own population for USB
double e1;      // Fraction of preferred contacts with own population for FB
double g;       // Fraction of FB arrivals with LTBI who are fast progressors
double phi0;    // Cumulative fraction self-cure and treatment of active disease for both populations per year RATES (USB)
double phi1;    // Cumulative fraction self-cure and treatment of active disease for both populations per year RATES (FB)
double sigmaF0; // Cumulative fraction of treatment for acute infection for both populations per year RATES (USB)
double sigmaF1; // Cumulative fraction of treatment for acute infection for both populations per year RATES (FB)
double sigmaL;  // Treatment rate for chronic LTBI per year
         
                // We handle susceptibles only by population
double S0pop;   // population of USB Susceptibles (S0)
double S1pop;   // population of FB Susceptibles (S1)
                // "Snapshot" variables which need to be global
double deltaS0;
double deltaS1;
double totpop;
double lambda0;
double lambda1;

turtles-own [
             dstate        // state of TB disease (latentLTBI=0, acuteLTBI=1, infectiousATBI=2, non-infectiousATBI=3)
             // Note: dstate = 0 by default
             nstate        // state of turtle in the next time step, may or may not be the same as dstate
             ]

to setup
clear-all
setup-globals
setup-turtles
reset-ticks
end

to setup-globals
mu0 =1 / 78;
mu1 =1 / 53;
ro =0.018;
alpha =0.005;
p =0.103;
vF =1.5;
l0 = 0.015;
l1 =0.211;
r0 =0.667;
r1 =0.780;
vL0 =0.0014;
vL1 =0.0010;
q =0.708;
mud =0.115;
x =0.111;
ARI0 =0.030 / 100;
beta =10.39;
e0 =0.965;
e1 =0.985;
g =0.0047;
phi0 =1.114;
phi1 =1.167;
sigmaF0 =1.296;
sigmaF1 =1.301;

sigmaL 0.057;
f 0.187;
if popConst < 1 [popConst 1000]
// parameter to relate population to actual number of turtles in model
// the reason it's large is to ensure each compartment contains at least one initial person
end

