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
  //srand(time(NULL)); TODO: Should this be seeded somewhere?
  //TODO: I think we can make through with just one random number throughout this whole function. Specifically, like, we pick one random number if state == CHRONIC_LATENT. Then, if its less than prob_chronic_progression, we can use it again by appriorately scaling PERCENT_INFECTIOUS_TB as r is still a random number between 0 and PROB_CHRONIC_PROGRESSION
  
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
  
  //TODO: First, we can move this if up and make it an else if
  if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB){
    //Mortality rate for TB
    r = (double)rand()/RAND_MAX; //random number from (0,1]
    if(r < MU_TB*DELTA_T) result = TB_DEATH;
  }
  
  //Self-cure rate
  // CAUTION: NOT SURE IF THIS WILL RESULT IN INCORRECT PROBS
  // TODO: This can be moved into the previous if statement. This is using an incorrect probability method, as we should not be giving preference to TB_Death over TB Curing. It alters the total probability of events. 
  if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB) {
    r = (double)rand()/RAND_MAX; //random number from (0,1]
    if(r < PROB_ACTIVE_SELF_CURE)
      result = SUSCEPTIBLE;
  }
  //TODO: This can be moved into another if statement earlier as well. Again, this causes preference to be awarded. 
  else if(state == ACUTE_LTBI || state == CHRONIC_LTBI) {
    r = (double)rand()/RAND_MAX; //random number from (0,1]
    if(r < PROB_LATENT_SELF_CURE)
      result = SUSCEPTIBLE;
  }
  
  //Calculate new costs of treatments
  if(treatmentTimeLeft > 0){
  	if(state == ACUTE_LTBI || state == CHRONIC_LTBI)
      //TODO: Is this a double or an int? Maybe its int division, and thus always 0
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
    //TODO: This has some chance of having dead turtles enter treatment, which is fine as we delete them if they die anyways. But its something to keep in mind. As this isn't in an else if, I think this is accurate probabilistically. However, wherever we use r above, if we add another variable, like randRange or something, which tells the current range in which we know r sits, and then we subtract the bottom of that range from r, so r is uniformly random between 0 and randRange, we can still use the same r we used before for this. This only makes sense if generating a uniform random is very expensive. If its not as expensive as the extra variable, then skip it. 
    r = (double)rand()/RAND_MAX; //random number from (0,1]
    //TODO: reorganize this. 
    if(r < PROB_ACTIVE_TREATMENT){
      if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB)
        treatmentTimeLeft = ACTIVE_TREATMENT_LENGTH;
      else if(r < PROB_LATENT_TREATMENT && (state == ACUTE_LTBI || state == CHRONIC_LTBI) )
        treatmentTimeLeft = LATENT_TREATMENT_LENGTH;
    }
  }
  
  //Natural Death Rate
  r = (double)rand()/RAND_MAX; //random number from (0,1]
  //TODO: See comments about about r and if this is probabilistically correct. I think it is, but I don't know if we need to generate r again. 
  if(r < mu * DELTA_T){
    result = NATURAL_DEATH;
  }
  //cout << "\nresult = " << result << "\n\n";
  state = result;
  //TODO: we don't actually need to return result here, as we always use the turtle.getState() function later. We need to ask whether the return copying is more intensive then the calling of turtle.getState(). If it is, eliminate the return. If its not, return the state, and then store it in a local variable in agentbased.cpp for all future comparisons. 
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
