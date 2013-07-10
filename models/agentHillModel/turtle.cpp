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
//rate specificity (mu0 mu1)
turtle::turtle(COB c, State s)
  : country(c), state(s), treatmentTimeLeft(0), newCost(0), x(0)
{
	if(country == USA) 
	//TODO: Add DELTA_T to definition of mu's
    mu = MU0;
	else mu = MU1;
}

//Updates turtle state, treatmentTimeLeft, and newCost for each iteration
void turtle::updateState(){
  //srand(time(NULL)); TODO: Should this be seeded somewhere?
  //Initializations
  double r  = (double)rand()/RAND_MAX, //random numbers from (0,1]
         rT = (double)rand()/RAND_MAX;
  double pfill = 0;  //sum of all probability states previously considered
  turtle::State result = state; //result = next state, default is that the state doesn't change
  
  if(r < mu*DELTA_T)  //Natural death rate
    result = NATURAL_DEATH;
  else if(state == ACUTE_LTBI || state == CHRONIC_LTBI){
    pfill += mu*DELTA_T;  //Natural death rate probability accounted for
    
    //Treatment costs and effects (use rT as random number)
    if(treatmentTimeLeft > 0){  //Turtle is currently receiving treatment
      newCost += LATENT_TREATMENT_COST / LATENT_TREATMENT_LENGTH;  //Add new costs of treatments
      treatmentTimeLeft--;
      //Effect of treatment
      if(treatmentTimeLeft == 0 && (rT < PROB_LTBI_TREATMENT_SUCCESS) ){
        state = SUSCEPTIBLE;
        return;
      }
    }else{  //probability of entering treatment
      if(rT < PROB_LATENT_TREATMENT)
        treatmentTimeLeft = LATENT_TREATMENT_LENGTH;
    }
    
    //Disease progression and self-cure
    if(r < pfill + PROB_LATENT_SELF_CURE)  //Self-cure rate
      result = SUSCEPTIBLE;
    else if(state == CHRONIC_LTBI){
      pfill += PROB_LATENT_SELF_CURE;  //Latent self-cure probability accounted for
	  if(r < pfill + PROB_CHRONIC_PROGRESSION){  //Disease progression from Chronic Latent to Active TB
        if(r < pfill + PERCENT_INFECTIOUS_TB*PROB_CHRONIC_PROGRESSION) 
          result = INFECTIOUS_TB;
        else
          result = NONINFECTIOUS_TB;
      }
    }else if(state == ACUTE_LTBI){
      pfill += PROB_LATENT_SELF_CURE;  //Latent self-cure probability accounted for
      if(r < pfill + PROB_ACUTE_PROGRESSION){  //Disease progression from Acute Latent to Active TB
        if(r < pfill + PERCENT_INFECTIOUS_TB*PROB_ACUTE_PROGRESSION) 
          result = INFECTIOUS_TB;
	    else
          result = NONINFECTIOUS_TB;
      }
  	}
  }else if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB){
	pfill += mu*DELTA_T;  //Natural death rate probability accounted for
    
    //Treatment costs and effects (use rT as random number)
    if(treatmentTimeLeft > 0){  //Turtle is currently receiving treatment
      newCost += ACTIVE_TREATMENT_COST / ACTIVE_TREATMENT_LENGTH;  //Add new costs of treatments
      treatmentTimeLeft--;
      //Effect of treatment
      if(treatmentTimeLeft == 0 && (rT < PROB_ACTIVE_TREATMENT_SUCCESS) ){
          state = SUSCEPTIBLE;  //individual is cured of Active TB
          return;
      }
    }
    else{  //probability of entering treatment
      if(rT < PROB_ACTIVE_TREATMENT)
        treatmentTimeLeft = ACTIVE_TREATMENT_LENGTH;
    }
    
    //TB deaths and self-cures
    if(r < pfill + MUD*DELTA_T)
	  result = TB_DEATH;  //Mortality rate for TB
	else if(r < pfill + MUD*DELTA_T + PROB_ACTIVE_SELF_CURE)
      result = SUSCEPTIBLE;  //Self-cure rate
  }
  
  state = result;
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

//Commented out to test compilation of agentbased.cpp
int main()
{
    turtle t = turtle(turtle::USA, turtle::CHRONIC_LTBI);
    
    int i;
    for(i=1; i<50; i++){
		t.display();
		t.updateState();
	}
}

