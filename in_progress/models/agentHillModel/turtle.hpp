// turtle.hpp

#ifndef ____turtle__
#define ____turtle__

#include <iostream>
#include <math.h>

const double DELTA_T = .05;  //measured in years

const double mu0   = 1./78;                   //Natural mortality rate USB per year, source: Hill Model
const double mu1   = 1./53;                   //Natural mortality rate FB per year, source: Hill Model
const double mud   = 0.115;                   //Mortality rate due to TB per year, source: Hill Model
const double MU0   = 1 - exp(-mu0 * DELTA_T); //Natural mortality prob USB, source: Hill Model
const double MU1   = 1 - exp(-mu1 * DELTA_T); //Natural mortality prob FB, source: Hill Model
const double MUD   = 1 - exp(-mud * DELTA_T);   //TB Mortality prob, source: Hill Model

//Hill model constants
const double ro      = 0.018;     // USB birth rate per year
const double alpha   = 0.005;     // FB birth rate per year
const double p       = 0.103;     // Fraction of new infectionsn which are acute(fast progressors)
const double r0      = 0.667;     // Fraction of cases due to reactivation in the USB population
const double r1      = 0.780;     // Fraction of cases due to reactivation in the FB population
const double x       = 0.111;     // Fraction of re-infected chronic LTBI moving to acute infection
const double f       = 0.187;     // Fraction of FB arrivals with LTBI
const double beta    = 10.39;     // Effective contact rate per year
const double e0      = 0.965;     // Fraction of preferred contacts with own population for USB
const double e1      = 0.985;     // Fraction of preferred contacts with own population for FB
const double g       = 0.0047;    // Fraction of FB arrivals with LTBI who are fast progressors

const double vF = 1.5; // Progression of acute infection per year
const double PROB_ACUTE_PROGRESSION   = 1 - exp(-vF*DELTA_T); //Probability of disease progression from acute latent to active TB every timestep

const double vL0 = 0.0014;    // Progression rate for reactivation (chronic LTBI) in the USB population per year
const double vL1 = 0.0010;    // Progression rate for reactivation (chronic LTBI) in the FB population per year
const double PROB_CHRONIC_PROGRESSION = 1 - exp(-((vL0 + vL1)/2.)*DELTA_T); //Probability of disease progression from chronic latent to active TB every timestep

const double PERCENT_INFECTIOUS_TB = 0.708;   //Proportion of TB cases that are infectious, source: Hill model (q)

const double phi0    = 1.114; // Cumulative fraction self-cure and treatment of active disease for both populations per year RATES (USB)
const double phi1    = 1.167; // Cumulative fraction self-cure and treatment of active disease for both populations per year RATES (FB)
const double sigmaL  = 0.057; // Treatment rate for chronic LTBI per year
const double sigmaF0 = 1.296; // Treatment for acute LTBI
const double sigmaF1 = 1.301;
const double PROB_ACTIVE_SELF_CURE = 1 - exp(-((phi0 + phi1)/2.)*.1*DELTA_T); // Probability of active TB self-curing per time step

const double LATENT_TREATMENT_COST = 500;  //9 months of isoniazid
const double ACTIVE_TREATMENT_COST = 6000; //9 month medications + hospitalizations

const double LATENT_TREATMENT_LENGTH = (0.75/DELTA_T);  //9 months, in time steps
const double ACTIVE_TREATMENT_LENGTH = (0.75/DELTA_T);  //9 months, in time steps

const double PERCENT_INITIAL_LATENT_TREATMENT = 0;  // proportion of initial latent TB population who start in treatment
const double PERCENT_INITIAL_ACTIVE_TREATMENT = 0.90;  // proportion of initial active TB population who start in treatment
const double PROB_ACTIVE_TREATMENT         = 1 - exp(-((phi0 + phi1)/2.)*.9*DELTA_T);//.01;   //probability of someone with active TB starting treatment every timestep
const double PROB_CHRONIC_LATENT_TREATMENT = 1 - exp(-sigmaL * DELTA_T); //probability of someone with latent TB starting treatment every timestep
const double PROB_ACUTE_LATENT_TREATMENT   = 1 - exp(-((sigmaF0 + sigmaF1)/2.)*DELTA_T);//
const double PROB_ACTIVE_TREATMENT_SUCCESS = 1;    // probability that treatment of active TB is successful
const double PROB_LTBI_TREATMENT_SUCCESS   = 1;    // probability that treatment of latent TB is successful

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
  turtle(const COB &c, const State &s, const int initTreat);
  void updateState();
  void display();
  bool handleTreatment(const double &probSuccess, const double &treatCost);
  bool dead();
  COB getCountry();
  State getState();
  int getTreatmentTimeLeft();
  double getNewCost();
  double getresetNewCost();
  int getTimeSinceInfection();
  void infect();

private:
  COB country;
  State state;
  int treatmentTimeLeft;
  double newCost;
  double mu;  //natural mortality rate
  int x; // time since infection
};

extern const char* countryNames[2];
extern const char* stateNames[7];

#endif /* defined(____turtle__) */
