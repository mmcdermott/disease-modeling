//
//  agentbased.h
//  
//
//  Created by mhcuser on 6/28/13.
//
//

#ifndef ____agentbased__
#define ____agentbased__

#include <iostream>

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

list<turtle> population;

vector<int> L0;
vector<int> F0;
vector<int> J0;
vector<int> I0;
vector<int> L1;
vector<int> F1;
vector<int> I1;
vector<int> J1;
vector<double> cost;



#endif /* defined(____agentbased__) */
