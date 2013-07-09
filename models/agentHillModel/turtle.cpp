//
//  turtle.cpp
//  
//
//  Created by mhcuser on 7/1/13.
//
//
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
//rate specificty (mu0 mu1)
turtle::turtle(COB c, State s)
  : country(c), state(s), treatmentTimeLeft(0), newCost(0), x(0)
{
	if(country == USA) 
    mu = MU0;
	else mu = MU1;
}

//Updates turtle state, treatmentTimeLeft, and newCost for each iteration
turtle::State turtle::updateState(){
  //srand(time(NULL));
  
  //Initializations
  double r; //random number from (0,1];
  turtle::State result = state;  //result = next state
  
  //Disease progression from latent to active TB
  if (state == CHRONIC_LTBI){
    r = (double)rand()/RAND_MAX;
    if (r < PROB_CHRONIC_PROGRESSION){
      r = (double)rand()/RAND_MAX; //random number from (0,1]
      if (r < PERCENT_INFECTIOUS_TB) 
        result = INFECTIOUS_TB;
      else 
        result = NONINFECTIOUS_TB;
    }			
  }
  else if (state == ACUTE_LTBI){
    r = (double)rand()/RAND_MAX;
    if (r < PROB_ACUTE_PROGRESSION){
      r = (double)rand()/RAND_MAX; //random number from (0,1]
  		if (r < PERCENT_INFECTIOUS_TB) 
        result = INFECTIOUS_TB;
  		else 
        result = NONINFECTIOUS_TB;
  	}
  }
  
  //Mortality rate for TB
  if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB){
    r = (double)rand()/RAND_MAX; //random number from (0,1]
    if(r < MU_TB*DELTA_T) result = TB_DEATH;
  }
  
  //Self-cure rate
  // CAUTION: NOT SURE IF THIS WILL RESULT IN INCORRECT PROBS
  if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB) {
    r = (double)rand()/RAND_MAX; //random number from (0,1]
    if(r < PROB_ACTIVE_SELF_CURE)
      result = SUSCEPTIBLE;
  }
  else if(state == ACUTE_LTBI || state == CHRONIC_LTBI) {
    r = (double)rand()/RAND_MAX; //random number from (0,1]
    if(r < PROB_LATENT_SELF_CURE)
      result = SUSCEPTIBLE;
  }
  
  //Calculate new costs of treatments
  if(treatmentTimeLeft > 0){
  	if(state == ACUTE_LTBI || state == CHRONIC_LTBI)
  		newCost += LATENT_TREATMENT_COST / LATENT_TREATMENT_LENGTH;
  	if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB)
  		newCost += ACTIVE_TREATMENT_COST / ACTIVE_TREATMENT_LENGTH;
  	
  	treatmentTimeLeft--;
  	//Effect of treatment
    if(treatmentTimeLeft == 0){
      r = (double)rand()/RAND_MAX; //random number from (0,1]
      if(r < PROB_LTBI_TREATMENT_SUCCESS && (state == ACUTE_LTBI || state == CHRONIC_LTBI) )
        result = SUSCEPTIBLE;  //individual is cured of LTBI				
      else if(r < PROB_ACTIVE_TREATMENT_SUCCESS && (state == INFECTIOUS_TB || state == NONINFECTIOUS_TB) )
        result = SUSCEPTIBLE;  //individual is cured of active TB
    }
  }
  else { //probability of entering treatment (all turtles have latent or active TB)
    r = (double)rand()/RAND_MAX; //random number from (0,1]
    if(r < PROB_ACTIVE_TREATMENT){
      if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB)
        treatmentTimeLeft = ACTIVE_TREATMENT_LENGTH;
      else if(r < PROB_LATENT_TREATMENT && (state == ACUTE_LTBI || state == CHRONIC_LTBI) )
        treatmentTimeLeft = LATENT_TREATMENT_LENGTH;
    }
  }
  
  //Natural Death Rate
  r = (double)rand()/RAND_MAX; //random number from (0,1]
  if(r < mu * DELTA_T){
    result = NATURAL_DEATH;
  }
  //cout << "\nresult = " << result << "\n\n";
  state = result;
  return result;
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

int turtle::getNewCost() {
  return newCost;
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
	turtle t = turtle(turtle::USA, turtle::CHRONIC_LTBI);
	t.display();
		
	t.updateState();
	t.display();
	
	t.updateState();
	t.display();
}
*/
