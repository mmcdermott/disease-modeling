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

enum COB {
	USA = 0,
	OTHER = 1
};
enum State {
	ACUTE_LATENT = 0,
	CHRONIC_LATENT = 1,
	INFECTIOUS_TB = 2,
	NONINFECTIOUS_TB = 3,
	SUSCEPTIBLE = 4,
	TB_DEATH = 5,
	NATURAL_DEATH = 6
};

const char* countryNames[] = {"USA", "Other"};
const char* stateNames[] = {"Acute Latent (F)", "Chronic Latent (L)", "Infectious TB (I)",
							"Non-Infectious TB (J)", "Susceptible (S)", "TB Death", "Natural Death"};

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
    int updateState(double probVector[]);
    void display();
    COB getCountry();
    State getState();
    int getTreatmentTimeLeft();
    int getNewCost();
    int getTimeSinceInfection();
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
int turtle::updateState(double probVector[]){
	srand(time(NULL));
	
	//Initializations
	double r; //random number from (0,1]
	int startingIndex = country*24 + state*6;
	double probArray[6];
	int i, result = 0;  //result = next state
	
	//Find turtle-specific discrete probability distribution
	for(i=0; i<6; i++)
		probArray[i] = probVector[startingIndex + i];
	
	//Sample a random state "result"
	r = (double)rand()/RAND_MAX; //random number from (0,1]
		i = 0;
	while(r > 0){
		r -= probArray[i];
		if(r < 0) result = i;
		i++;
	}
	cout << "\nresult = " << result << "\n\n";
	
	//Natural Death Rate
	r = (double)rand()/RAND_MAX; //random number from (0,1]
	if(r < mu){
		result = NATURAL_DEATH;
	}
	
	//Calculate new costs of treatments
	if(treatmentTimeLeft > 0){
		if(state == ACUTE_LATENT || state == CHRONIC_LATENT)
			newCost += LATENT_TREATMENT_COST / LATENT_TREATMENT_LENGTH;
		if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB)
			newCost += ACTIVE_TREATMENT_COST / ACTIVE_TREATMENT_LENGTH;
		
		treatmentTimeLeft--;
	}
	else { //probability of entering treatment
		r = (double)rand()/RAND_MAX; //random number from (0,1]
		if(r < PROB_ACTIVE_TREATMENT){
			if(state == INFECTIOUS_TB || state == NONINFECTIOUS_TB)
				treatmentTimeLeft = ACTIVE_TREATMENT_LENGTH;
			else if(r < PROB_LATENT_TREATMENT && (state == ACUTE_LATENT || state == CHRONIC_LATENT) )
				treatmentTimeLeft = LATENT_TREATMENT_LENGTH;
		}
	}
	
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


int main()
{
	turtle t = turtle(USA, CHRONIC_LATENT);
	t.display();
	
	//Initialize a simple probability vector to test
	double probVector[48];
	int i;
	for(i=0;i<48;i++)
		probVector[i] = 0.2;
	for(i=0;i<48;i+=6)
		probVector[i] = 0.0;
		
	t.updateState(probVector);
	t.display();
	
	t.updateState(probVector);
	t.display();
}
