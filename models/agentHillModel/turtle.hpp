//
// turtle.hpp
//
//
// Created by mhcuser on 7/1/13.
//
//

#ifndef ____turtle__
#define ____turtle__

#include <iostream>
#include <math.h>

const double DELTA_T = .05;  //measured in years

const double MU0   = 1./78;                 //Natural mortality rate USB per year, source: Hill Model
const double MU1   = 1./53;                 // Natural mortality rate FB per year, source: Hill Model
const double MUD   = 0.115;                 // Mortality rate due to TB per year, source: Hill Model
const double MU_TB = 1-pow(1-MUD, DELTA_T); //Mortality rate due to TB per timestep, calculated

const double LATENT_TREATMENT_COST = 500;  //9 months of isoniazid
const double ACTIVE_TREATMENT_COST = 6000; //9 month medications + hospitalizations

const double LATENT_TREATMENT_LENGTH = (0.75/DELTA_T);  //9 months, in time steps
const double ACTIVE_TREATMENT_LENGTH = (0.75/DELTA_T);  //9 months, in time steps

const double PROB_ACTIVE_TREATMENT         = .1;   //probability of someone with active TB starting treatment every timestep
const double PROB_LATENT_TREATMENT         = .005; //probability of someone with latent TB starting treatment every timestep
const double PROB_ACTIVE_TREATMENT_SUCCESS = 1;    // probability that treatment of active TB is successful
const double PROB_LTBI_TREATMENT_SUCCESS   = 1;    // probability that treatment of latent TB is successful

const double PROB_ACUTE_PROGRESSION   = 0.005; //Probability of disease progression from acute latent to active TB every timestep
const double PROB_CHRONIC_PROGRESSION = 0.005; //Probability of disease progression from chronic latent to active TB every timestep

const double PERCENT_INFECTIOUS_TB = 0.8;   //Proportion of TB cases that are infectious
const double PROB_SELF_CURE        = 0.005; // Probability of someone with active TB self-curing



//Declaration
class turtle{
public:
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
  turtle(COB c, State s);
  State updateState();
  void display();
  COB getCountry();
  State getState();
  int getTreatmentTimeLeft();
  int getNewCost();
  int getTimeSinceInfection();
  void infect();

private:
  COB country;
  State state;
  int treatmentTimeLeft;
  int newCost;
  double mu;  //natural mortality rate
  int x; // time since infection
};

extern const char* countryNames[2];
extern const char* stateNames[7];

#endif /* defined(____turtle__) */
