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
#include <chrono>
#include <random>
#include <functional>
#include <array>
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
	SUSCEPTIBLE = 4
};
enum DeathType {
	STILL_ALIVE = 0,
	NATURAL_DEATH = 1,
	TB_DEATH = 2
};

const char* countryNames[] = {"USA", "Other"};
const char* stateNames[] = {"Acute Latent (F)", "Chronic Latent (L)", "Infectious TB (I)", "Non-Infectious TB (J)", "Susceptible (S)", "Dead"};
const char* deathTypeNames[] = {"Still Alive", "Natural Death", "TB Death"};

//Declaration
class turtle{
private:
    COB country;
	State state;
    DeathType deathType;
	int treatmentTimeLeft;
    int newCost;
	double mu;  //natural mortality rate

public:
    turtle(COB c, State s);
    int updateState(double probVector[]);
    void display();
};

//Implementation
//Constructor
turtle::turtle(COB c, State s){
	country = c;
	state = s;
	treatmentTimeLeft = 0;
	newCost = 0;
	deathType = STILL_ALIVE;
	if(country == USA) mu = MU0;
	else mu = MU1;
}
//Updates turtle state, treatmentTimeLeft, and newCost for each iteration
int turtle::updateState(double probVector[]){
	srand(time(NULL));
	double r; //random number from (0,1]
	int startingIndex = country*24 + state*6;
	double probArray[6];
	int i, result = 0;
	for(i=0; i<6; i++)
		probArray[i] = probVector[startingIndex + i];
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
		deathType = NATURAL_DEATH;
		return 5;
	}
	if(result <= 4) state = static_cast<State>(result);
	else deathType = TB_DEATH;
	return result;
}

void turtle::display(){
	cout << "Country of Birth: " << countryNames[country] << "\n";
	cout << "Health State: " << stateNames[state] << "\n";
	cout << "Treatment Time Left: " << treatmentTimeLeft << "\n";
	cout << "New cost: $" << newCost << "\n";
	cout << "Natural death rate: " << mu << "\n";
	cout << "Natural death: " << deathTypeNames[deathType] << "\n\n";
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
}
