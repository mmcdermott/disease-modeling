//
// turtle.h
//
//
// Created by mhcuser on 7/1/13.
//
//

#ifndef ____turtle__
#define ____turtle__

#include <iostream>

const double DELTA_T = 1;  //1 month

const double MU0 = 1./78; //Natural mortality rate USB per year
const double MU1 = 1./53; // Natural mortality rate FB per year

const double LATENT_TREATMENT_COST = 500;  //9 months of isoniazid
const double ACTIVE_TREATMENT_COST = 6000; //9 month medications + hospitalizations

const double LATENT_TREATMENT_LENGTH = 9;  //9 months
const double ACTIVE_TREATMENT_LENGTH = 9;  //9 months

const double PROB_ACTIVE_TREATMENT = 1.;   //probability of someone with active TB starting treatment every timestep
const double PROB_LATENT_TREATMENT = .5; //probability of someone with latent TB starting treatment every timestep

#endif /* defined(____turtle__) */
