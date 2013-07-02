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

const double PROB_ACTIVE_TREATMENT = .1;   //probability of someone with active TB starting treatment every timestep
const double PROB_LATENT_TREATMENT = .005; //probability of someone with latent TB starting treatment every timestep

const double PROB_ACUTE_PROGRESSION = 0.005; //Probability of disease progression from acute latent to active TB every timestep
const double PROB_CHRONIC_PROGRESSION = 0.005; //Probability of disease progression from chronic latent to active TB every timestep

const double PERCENT_INFECTIOUS_TB = 0.8;  //Proportion of TB cases that are infectious

enum COB {  //country of birth
	USA = 0,
	OTHER = 1
};
enum State {  //health state
	ACUTE_LTBI = 0,
	CHRONIC_LTBI = 1,
	INFECTIOUS_TB = 2,
	NONINFECTIOUS_TB = 3,
	SUSCEPTIBLE = 4,
	TB_DEATH = 5,
	NATURAL_DEATH = 6
};

const char* countryNames[] = {"USA", "Other"};
const char* stateNames[] = {"Acute Latent (F)", "Chronic Latent (L)", "Infectious TB (I)",
							"Non-Infectious TB (J)", "Susceptible (S)", "TB Death", "Natural Death"};

#endif /* defined(____turtle__) */
