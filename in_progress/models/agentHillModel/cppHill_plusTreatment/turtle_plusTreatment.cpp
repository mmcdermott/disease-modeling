//  turtle.cpp

#include <iostream>
#include <math.h>
#include <stdlib.h>
#include <random>
#include <functional>
using namespace std;
#include "turtle_plusTreatment.hpp"


const char* countryNames[2] = {"USA", "Other"};
const char* stateNames[7] = {"Acute Latent (F)", "Chronic Latent (L)", "Infectious TB (I)",
							"Non-Infectious TB (J)", "Susceptible (S)", "TB Death", "Natural Death"};

//Implementation
//Constructor
turtle::turtle(const COB &c, const State &s)
  : country(c), state(s), cost(0) //, treatmentTimeLeft(0)
{
  if (state == INFECTIOUS_TB || state == NONINFECTIOUS_TB) {
    double r = (double)rand()/RAND_MAX;
    if (r < PROB_HOSPITAL) {
      cost = ACTIVE_TREATMENT_COST_HOSPITAL;
    } else {
      cost = ACTIVE_TREATMENT_COST_NOHOSPITAL;
    }
  }
}

bool turtle::dead() {
  return (state == NATURAL_DEATH || state == TB_DEATH);
}

//Updates turtle state
void turtle::updateState(){
  //Initializations
  double r  = (double)rand()/RAND_MAX; //random number from (0,1]
  
  if(country == USA){
    if (r < MU0) {  //Natural death rate (USB)
      state = NATURAL_DEATH;
      return;
    }
    r -= MU0;  //Natural death rate probability accounted for

    //Disease progression and self-cure
    if (state == CHRONIC_LTBI) {
      if (r < PROB_CHRONIC_PROGRESSION_0) {  //Disease progression from Chronic Latent to Active TB (USB)
        if(r < PROB_HOSPITAL*PROB_CHRONIC_PROGRESSION_0) {
          cost += COST_PER_ACTIVE_CURE_HOSPITAL;
        } else {
          cost += COST_PER_ACTIVE_CURE_NOHOSPITAL;
        }
        if (r < q*PROB_CHRONIC_PROGRESSION_0) {
          state = INFECTIOUS_TB;
        } else {
          state = NONINFECTIOUS_TB;
        }
        return;
      } else if (r - PROB_CHRONIC_PROGRESSION_0 < PROB_CHRONIC_LATENT_CURE) {
        state = SUSCEPTIBLE;
        cost += COST_PER_LATENT_CURE;
        return;
      }
    } else if(state == ACUTE_LTBI){
      if(r < PROB_ACUTE_PROGRESSION){  //Disease progression from Acute Latent to Active TB
        if(r < PROB_HOSPITAL*PROB_ACUTE_PROGRESSION) {
          cost += COST_PER_ACTIVE_CURE_HOSPITAL;
        } else {
          cost += COST_PER_ACTIVE_CURE_NOHOSPITAL;
        }
        if(r < q*PROB_ACUTE_PROGRESSION) {
          state = INFECTIOUS_TB;
        } else {
          state = NONINFECTIOUS_TB;
        }
        return;
      } else if (r - PROB_ACUTE_PROGRESSION < PROB_ACUTE_LATENT_CURE_0) {
        state = SUSCEPTIBLE;
        cost += COST_PER_LATENT_CURE;
        return;
      }
    } else if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB){
    
      //TB deaths and self-cures
      if (r < MUD) {
        state = TB_DEATH;  //Mortality rate for TB
      } else if (r - MUD < PROB_ACTIVE_CURE_0) {
        state = SUSCEPTIBLE;  //Self-cure rate
      }
      return;
    }
  }
  else{ //country == OTHER
    if (r < MU1) {  //Natural death rate (FB)
      state = NATURAL_DEATH;
      return;
    }
    r -= MU1;  //Natural death rate probability accounted for

    //Disease progression and self-cure
    if (state == CHRONIC_LTBI) {
      if (r < PROB_CHRONIC_PROGRESSION_1) {  //Disease progression from Chronic Latent to Active TB (FB)
        if (r < q*PROB_CHRONIC_PROGRESSION_1) {
          state = INFECTIOUS_TB;
        } else {
          state = NONINFECTIOUS_TB;
        }
        return;
      } else if (r - PROB_CHRONIC_PROGRESSION_1 < PROB_CHRONIC_LATENT_CURE) {
        state = SUSCEPTIBLE;
        cost += COST_PER_LATENT_CURE;
        return;
      }
    } else if(state == ACUTE_LTBI){
      if(r < PROB_ACUTE_PROGRESSION){  //Disease progression from Acute Latent to Active TB
        if(r < q*PROB_ACUTE_PROGRESSION) {
          state = INFECTIOUS_TB;
        } else {
          state = NONINFECTIOUS_TB;
        }
        return;
      } else if (r - PROB_ACUTE_PROGRESSION < PROB_ACUTE_LATENT_CURE_1) {
        state = SUSCEPTIBLE;
        cost += COST_PER_LATENT_CURE;
        return;
      }
    } else if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB){
    
      //TB deaths and self-cures
      if (r < MUD) {
        state = TB_DEATH;  //Mortality rate for TB
      } else if (r - MUD < PROB_ACTIVE_CURE_1) {
        state = SUSCEPTIBLE;  //Self-cure rate
      }
      return;
    }
  }
}

void turtle::display(){
  cout << "\nCountry of Birth: " << countryNames[country] << "\n";
  cout << "Health State: " << stateNames[state] << "\n";
  cout << "Cost: $" << cost << "\n";
}

turtle::COB turtle::getCountry() {
  return country;
}

turtle::State turtle::getState() {
  return state;
}

void turtle::infect(){
  double r = (double)rand()/RAND_MAX;
  if (r < q) 
    state = INFECTIOUS_TB;
  else 
    state = NONINFECTIOUS_TB;
}

double turtle::getCost(){
    double c = cost;
    cost = 0;
    return c;
}

/*
//Commented out to test compilation of agentbasedStrictHill.cpp
int main()
{
    srand(time(NULL));
    turtle t = turtle(turtle::USA, turtle::ACUTE_LTBI);
    turtle t2 = turtle(turtle::USA, turtle::NATURAL_DEATH);
    turtle t3 = turtle(turtle::USA, turtle::CHRONIC_LTBI);
    turtle t4 = turtle(turtle::USA, turtle::ACUTE_LTBI);
    turtle t5 = turtle(turtle::USA, turtle::INFECTIOUS_TB);
    
    cout << "\n--------------\nStarting values\n--------------" << endl;

    t.display();
    t2.display();
    t3.display();
    t4.display();
    t5.display();
        
    int i;
    for(i=1; i<500; i++){
      t.updateState();
      t2.updateState();
      t3.updateState();
      t4.updateState();
      t5.updateState();
	}
    
    cout << "\n------------\nEnding values\n------------" << endl;
    
    t.display();
    t2.display();
    t3.display();
    t4.display();
    t5.display();
}*/
