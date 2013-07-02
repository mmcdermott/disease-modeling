//
//  agentbased.cpp
//  
//
//  Created by mhcuser on 6/28/13.
//
//

#include <iostream>
using namespace std;
#include "agentbased.h"
//#include <array>

const double ro      = 0.018;     // USB birth rate per year
const double alpha   = 0.005;     // FB birth rate per year
const double p       = 0.103;     // Fraction of new infectionsn which are acute (fast progressors)
const double vF      = 1.5;       // Progression rate of acute infection per year
const double l0      = 0.015;     // Prevalence of LTBI in the USB population in 2000
const double l1      = 0.211;     // Prevalence of LTBI in the FB population in 2000
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

                            // We handle susceptibles only by population
double S0pop;               // population of USB Susceptibles (S0)
double S1pop;               // population of FB Susceptibles (S1)

double sigmaL        = 0.057;     // Treatment rate for chronic LTBI per year
double deltaS0;
double deltaS1;
double totpop;
double lambda0;
double lambda1;

int main()
{
    
    if (popConst < 1)
        popConst = 1000;
        // parameter to relate population to actual number of turtles in model
        // the reason it's large is to ensure each compartment contains at least one initial person
    return 0;
}
