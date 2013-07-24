// turtleStrictHill.hpp

// get round to work
//check exogenous reinfection

#ifndef ____turtle__
#define ____turtle__

#include <iostream>
#include <math.h>

const double DELTA_T = .02;  // measured in years

//Hill model constants
const double mu0     = 1./78;     // Natural mortality rate USB per year
const double mu1     = 1./53;     // Natural mortality rate FB per year
const double mud     = 0.115;     // Mortality rate due to TB per year
const double ro      = 0.018;     // USB birth rate per year
const double alpha   = 0.005;     // FB birth rate per year
const double p       = 0.103;     // Fraction of new infections which are acute (fast progressors)
const double q       = 0.708;     // Fraction of infections progressing to infectious disease
const double r0      = 0.667;     // Fraction of cases due to reactivation in the USB population
const double r1      = 0.780;     // Fraction of cases due to reactivation in the FB population
const double x       = 0.111;     // Fraction of re-infected chronic LTBI moving to acute infection
const double f       = 0.187;     // Fraction of FB arrivals with LTBI
const double beta    = 10.39;     // Effective contact rate per year
const double e0      = 0.965;     // Fraction of preferred contacts with own population for USB
const double e1      = 0.985;     // Fraction of preferred contacts with own population for FB
const double g       = 0.0047;    // Fraction of FB arrivals with LTBI who are fast progressors
const double vF      = 1.5;       // Progression of acute infection per year
const double vL0     = 0.0014;    // Progression rate for reactivation (chronic LTBI) in the USB population per year
const double vL1     = 0.0010;    // Progression rate for reactivation (chronic LTBI) in the FB population per year
const double phi0    = 1.114;     // Cumulative fraction self-cure and treatment of active disease for both populations per year RATES (USB)
const double phi1    = 1.167;     // Cumulative fraction self-cure and treatment of active disease for both populations per year RATES (FB)
const double sigmaL  = 0.057;     // Treatment rate for chronic LTBI per year
const double sigmaF0 = 1.296;     // Treatment rate for acute LTBI (USB)
const double sigmaF1 = 1.301;     // Treatment rate for acute LTBI (FB)

//Additional constants
const double MU0                        = 1 - exp(-mu0 * DELTA_T);     // Natural mortality prob per timestep (USB)
const double MU1                        = 1 - exp(-mu1 * DELTA_T);     // Natural mortality prob per timestep (FB)
const double MUD                        = 1 - exp(-mud * DELTA_T);     // TB Mortality prob per timestep
const double PROB_CHRONIC_PROGRESSION_0 = 1 - exp(-vL0 * DELTA_T);     // Probability of disease progression from chronic latent to active TB every timestep (USB)
const double PROB_CHRONIC_PROGRESSION_1 = 1 - exp(-vL1 * DELTA_T);     // Probability of disease progression from chronic latent to active TB every timestep (FB)
const double PROB_ACUTE_PROGRESSION     = 1 - exp(-vF * DELTA_T);      // Probability of disease progression from acute latent to active TB every timestep
const double PROB_ACTIVE_CURE_0         = 1 - exp(-phi0 * DELTA_T);    // Probability of active TB cure per time step (USB)
const double PROB_ACTIVE_CURE_1         = 1 - exp(-phi1 * DELTA_T);    // Probability of active TB cure per time step (FB)
const double PROB_CHRONIC_LATENT_CURE   = 1 - exp(-sigmaL * DELTA_T);  // Probability of chronic latent TB cure per time step
const double PROB_ACUTE_LATENT_CURE_0   = 1 - exp(-sigmaF0 * DELTA_T); // Probability of acute latent TB cure per time step (USB)
const double PROB_ACUTE_LATENT_CURE_1   = 1 - exp(-sigmaF1 * DELTA_T); // Probability of acute latent TB cure per time step (FB)

//Declaration
class turtle{
public:
  enum COB {  // Country of birth
    USA   = 0,
    OTHER = 1
  };
  enum State {  // Health state
    ACUTE_LTBI       = 0,
    CHRONIC_LTBI     = 1,
    INFECTIOUS_TB    = 2,
    NONINFECTIOUS_TB = 3,
    SUSCEPTIBLE      = 4,
    TB_DEATH         = 5,
    NATURAL_DEATH    = 6
  };
  turtle(const COB &c, const State &s);
  bool newinfect();
  bool dead();
  void updateState();
  void display();
  COB getCountry();
  State getState();
  void reinfect();

private:
  COB country;
  State state;
  bool newinfection;
};

extern const char* countryNames[2];
extern const char* stateNames[7];

#endif /* defined(____turtle__) */
