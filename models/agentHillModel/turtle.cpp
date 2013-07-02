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
#include "turtle.h"

//Declaration
class turtle{
private:
    COB country;
	State state;
	int treatmentTimeLeft;
    int newCost;
	double mu;  //natural mortality rate
    int x; // time since infection

public:
    turtle(COB c, State s);
    int updateState();
    void display();
    COB getCountry();
    State getState();
    int getTreatmentTimeLeft();
    int getNewCost();
    int getTimeSinceInfection();
	void infect(bool pulmonary_TB);
};

//Implementation
//Constructor
turtle::turtle(COB c, State s){
	country = c;
	state = s;
	treatmentTimeLeft = 0;
	newCost = 0;
	x = 0;
	if(country == USA) mu = MU0;
	else mu = MU1;
}

//Updates turtle state, treatmentTimeLeft, and newCost for each iteration
int turtle::updateState(){
	srand(time(NULL));
	
	//Initializations
	double r; //random number from (0,1]
	int i, result = 0;  //result = next state
	
	//Disease progression from latent to active TB
	if(state == CHRONIC_LTBI){
		r = (double)rand()/RAND_MAX; //random number from (0,1]
		if(r < PROB_CHRONIC_PROGRESSION){
			r = (double)rand()/RAND_MAX; //random number from (0,1]
			if(r < PERCENT_INFECTIOUS_TB) result = INFECTIOUS_TB;
			else result = NONINFECTIOUS_TB;
		}			
	}
	else if(state == ACUTE_LTBI){
		r = (double)rand()/RAND_MAX; //random number from (0,1]
		if(r < PROB_ACUTE_PROGRESSION){
			r = (double)rand()/RAND_MAX; //random number from (0,1]
			if(r < PERCENT_INFECTIOUS_TB) result = INFECTIOUS_TB;
			else result = NONINFECTIOUS_TB;
		}
	}
	
	//Mortality rate for TB
	//Self-cure rate
	
	//Calculate new costs of treatments
	if(treatmentTimeLeft > 0){
		if(state == ACUTE_LTBI || state == CHRONIC_LTBI)
			newCost += LATENT_TREATMENT_COST / LATENT_TREATMENT_LENGTH;
		if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB)
			newCost += ACTIVE_TREATMENT_COST / ACTIVE_TREATMENT_LENGTH;
		
		treatmentTimeLeft--;
		//Effect of treatment
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
	if(r < mu){
		result = NATURAL_DEATH;
	}
	
	cout << "\nresult = " << result << "\n\n";
	
	state = static_cast<State>(result);
	return result;
}

void turtle::display(){
	cout << "Country of Birth: " << countryNames[country] << "\n";
	cout << "Health State: " << stateNames[state] << "\n";
	cout << "Treatment Time Left: " << treatmentTimeLeft << "\n";
	cout << "New cost: $" << newCost << "\n";
	cout << "Natural death rate: " << mu << "\n\n";
}

COB turtle::getCountry() {
	return country;
}
State turtle::getState() {
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
void turtle::infect(bool pulmonary_TB){
	if(pulmonary_TB) state = INFECTIOUS_TB;
	else state = NONINFECTIOUS_TB;
}


int main()
{
	turtle t = turtle(USA, CHRONIC_LTBI);
	t.display();
		
	t.updateState();
	t.display();
	
	t.updateState();
	t.display();
}
