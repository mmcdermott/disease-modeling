//  turtle.cpp

#include <iostream>
#include <math.h>
#include <stdlib.h>
#include <random>
#include <functional>
using namespace std;
#include "turtle.hpp"


const char* countryNames[2] = {"USA", "Other"};
const char* stateNames[7] = {"Acute Latent (F)", "Chronic Latent (L)", "Infectious TB (I)",
							"Non-Infectious TB (J)", "Susceptible (S)", "TB Death", "Natural Death"};

//Implementation
//Constructor
//TODO: Add age stratification, such that we can eliminate the unnatural death
//rate specificity (mu0 mu1)
turtle::turtle(const COB &c, const State &s)
  : country(c), state(s), treatmentTimeLeft(0), newCost(0), x(0)
{
	if(country == USA) 
    mu = MU0;
	else mu = MU1;
}

bool turtle::handleTreatment(const double &probSuccess, const double &treatCost){
  newCost = treatCost;
  treatmentTimeLeft--;
  //Effect of treatment
  if (treatmentTimeLeft == 0 && (double)rand()/RAND_MAX < probSuccess) {
    state = SUSCEPTIBLE;
    return true;
  }
  return false;
}

bool turtle::dead() {
  return (state == NATURAL_DEATH || state == TB_DEATH);
}


//Updates turtle state, treatmentTimeLeft, and newCost for each iteration
void turtle::updateState(){
  //Initializations
  double r  = (double)rand()/RAND_MAX; //random numbers from (0,1]

  if (dead()) {
    cout << "gotcha! I'm a dead turtle getitng updated via updateState!" << endl;
    return;
  }

  if (r < mu) {  //Natural death rate
    state = NATURAL_DEATH;
    return;
  }
  r -= mu;  //Natural death rate probability accounted for

  //Disease progression and self-cure
  if (state == CHRONIC_LTBI) {
    if (treatmentTimeLeft > 0) {
      if (handleTreatment(PROB_LTBI_TREATMENT_SUCCESS, LATENT_TREATMENT_COST/LATENT_TREATMENT_LENGTH)) {
        return;
      }
    } else if (r < PROB_CHRONIC_LATENT_TREATMENT) {
      treatmentTimeLeft = LATENT_TREATMENT_LENGTH;
      return;
    } else {
      r -= PROB_CHRONIC_LATENT_TREATMENT;
    }

    if (r < PROB_CHRONIC_PROGRESSION) {  //Disease progression from Chronic Latent to Active TB
      if (r < PERCENT_INFECTIOUS_TB*PROB_CHRONIC_PROGRESSION) {
        state = INFECTIOUS_TB;
      } else {
        state = NONINFECTIOUS_TB;
      }
      return;
    }
  } else if(state == ACUTE_LTBI){
    if (treatmentTimeLeft > 0) {
      if (handleTreatment(PROB_LTBI_TREATMENT_SUCCESS, LATENT_TREATMENT_COST/LATENT_TREATMENT_LENGTH)) {
        return;
      }
    } else if (r < PROB_ACUTE_LATENT_TREATMENT) {
      treatmentTimeLeft = LATENT_TREATMENT_LENGTH;
      return;
    } else {
      r -= PROB_ACUTE_LATENT_TREATMENT;
    }

    if(r < PROB_ACUTE_PROGRESSION){  //Disease progression from Acute Latent to Active TB
      if(r < PERCENT_INFECTIOUS_TB*PROB_ACUTE_PROGRESSION) {
        state = INFECTIOUS_TB;
      } else {
        state = NONINFECTIOUS_TB;
      }
      return;
    }
  } else if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB){
    if (treatmentTimeLeft > 0) {
       if (handleTreatment(PROB_ACTIVE_TREATMENT_SUCCESS, ACTIVE_TREATMENT_COST/ACTIVE_TREATMENT_LENGTH)) {
         return;
      }
    } else if (r < PROB_ACTIVE_TREATMENT) {
      treatmentTimeLeft = LATENT_TREATMENT_LENGTH;
      return;
    } else {
      r -= PROB_ACTIVE_TREATMENT;
    }

    //TB deaths and self-cures
    if (r < MUD) {
      state = TB_DEATH;  //Mortality rate for TB
    } else if (r - MUD < PROB_ACTIVE_SELF_CURE) {
      state = SUSCEPTIBLE;  //Self-cure rate
    }
    return;
  }
}


void turtle::display(){
  cout << "Country of Birth: " << countryNames[country] << "\n";
  cout << "Health State: " << stateNames[state] << "\n";
  cout << "Treatment Time Left: " << treatmentTimeLeft << "\n";
  cout << "New cost: $" << newCost << "\n";
  cout << "Natural death rate: " << mu << "\n\n";
}

turtle::COB turtle::getCountry() {
  return country;
}

turtle::State turtle::getState() {
  return state;
}

int turtle::getTreatmentTimeLeft() {
  return treatmentTimeLeft;
}

double turtle::getNewCost() {
  return newCost;
}

double turtle::getresetNewCost() {
  double oldCost = newCost;
  newCost = 0;
  return oldCost;
}

int turtle::getTimeSinceInfection() {
  return x;
}

void turtle::infect(){
  double r = (double)rand()/RAND_MAX;
  if (r < PERCENT_INFECTIOUS_TB) 
    state = INFECTIOUS_TB;
  else 
    state = NONINFECTIOUS_TB;
}
/*
//Commented out to test compilation of agentbased.cpp
int main()
{
    srand(time(NULL));
    turtle t = turtle(turtle::USA, turtle::ACUTE_LTBI);
    turtle t2 = turtle(turtle::USA, turtle::NATURAL_DEATH);
    turtle t3 = turtle(turtle::USA, turtle::ACUTE_LTBI);
    turtle t4 = turtle(turtle::USA, turtle::ACUTE_LTBI);
    turtle t5 = turtle(turtle::USA, turtle::INFECTIOUS_TB);

    int i;
    for(i=1; i<5; i++){
		  cout << "\nnew day " << i << "\n";
      turtle tb = turtle(turtle::OTHER, turtle::ACUTE_LTBI);
      t.display();
		  t.updateState();
      t2.display();
      t2.updateState();
      t3.display();
      t3.updateState();
      t4.display();
      t4.updateState();
      t5.display();
      t5.updateState();
      tb.display();
	}
}*/

