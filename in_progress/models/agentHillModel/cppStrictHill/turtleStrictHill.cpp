//  turtleStrictHill.cpp

#include <iostream>
#include <math.h>
#include <stdlib.h>
#include <random>
#include <functional>
using namespace std;
#include "turtleStrictHill.hpp"


const char* countryNames[2] = {"USA", "Other"};
const char* stateNames[6] = {"Infectious TB (I)",
							"Non-Infectious TB (J)", "Susceptible (S)", "TB Death", "Natural Death", "Latent (L)"};

//Implementation
//Constructor
turtle::turtle(const COB &c, const State &s, int timeinfec)
  : country(c), state(s), timeinfec(timeinfec)
{}

bool turtle::dead() {
  return (state == NATURAL_DEATH || state == TB_DEATH);
}

//Updates turtle state
void turtle::updateState(){
  //Initializations
  changeState = false;
  double r  = (double)rand()/RAND_MAX; //random number from (0,1]
  if(country == USA){
    if (r < MU0) {  //Natural death rate (USB)
      state = NATURAL_DEATH;
      return;
    }
    r -= MU0;  //Natural death rate probability accounted for

    //Disease progression and self-cure
    if (state == LATENT) {
      timeinfec++;
      if (timeinfec <= 2/DELTA_T) {
        if(r < PROB_ACUTE_PROGRESSION){  //Disease progression from Acute Latent to Active TB
          changeState = true;
          if(r < q*PROB_ACUTE_PROGRESSION) {
            state = INFECTIOUS_TB;
          } else {
            state = NONINFECTIOUS_TB;
          }
          return;
        } else if (r - PROB_ACUTE_PROGRESSION < PROB_ACUTE_LATENT_CURE_0) {
          state = SUSCEPTIBLE;
          return;
        }
      } else {
        if (r < PROB_CHRONIC_PROGRESSION_0) {  //Disease progression from Chronic Latent to Active TB (USB)
          changeState = true;
          if (r < q*PROB_CHRONIC_PROGRESSION_0) {
            state = INFECTIOUS_TB;
          } else {
            state = NONINFECTIOUS_TB;
          }
          return;
        } else if (r - PROB_CHRONIC_PROGRESSION_0 < PROB_CHRONIC_LATENT_CURE) {
          state = SUSCEPTIBLE;
          return;
        }
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
  else { //country == OTHER
    if (r < MU1) {  //Natural death rate (FB)
      state = NATURAL_DEATH;
      return;
    }
    r -= MU1;  //Natural death rate probability accounted for

    //Disease progression and self-cure
    if (state == LATENT) {
      timeinfec++;
      if (timeinfec <= 2/DELTA_T) {
        if(r < PROB_ACUTE_PROGRESSION){  //Disease progression from Acute Latent to Active TB
          changeState = true;
          if(r < q*PROB_ACUTE_PROGRESSION) {
            state = INFECTIOUS_TB;
          } else {
            state = NONINFECTIOUS_TB;
          }
          return;
        } else if (r - PROB_ACUTE_PROGRESSION < PROB_ACUTE_LATENT_CURE_1) {
          state = SUSCEPTIBLE;
          return;
        }
      } else {  
        if (r < PROB_CHRONIC_PROGRESSION_1) {  //Disease progression from Chronic Latent to Active TB (USB)
          changeState = true;
          if (r < q*PROB_CHRONIC_PROGRESSION_1) {
            state = INFECTIOUS_TB;
          } else {
            state = NONINFECTIOUS_TB;
          }
          return;
        } else if (r - PROB_CHRONIC_PROGRESSION_1 < PROB_CHRONIC_LATENT_CURE) {
          state = SUSCEPTIBLE;
          return;
        }
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

bool turtle::getPrevState(){
  return changeState;
}

void turtle::display(){
  cout << "\nCountry of Birth: " << countryNames[country] << "\n";
  cout << "Health State: " << stateNames[state] << "\n";
}

turtle::COB turtle::getCountry() {
  return country;
}

turtle::State turtle::getState() {
  return state;
}

int turtle::InfectionTime(){
  return timeinfec;
}

void turtle::reinfect(){
  timeinfec = 0;
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
}
*/
