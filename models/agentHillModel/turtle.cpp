//
//  turtle.cpp
//  
//
//  Created by mhcuser on 7/1/13.
//
//
#include <iostream>
using namespace std;
#include "turtle.h"

enum State {
    Acute LATENT = 0,
    Chronic LATENT = 1;
};

int main()
{
	mu0 = 1/78;
    mu1 = 1/53;
}

class turtle{
private:
    char COB;
    int treatmentTimeLeft;
    int newCost;
public:
    turtle(char COB, State);
    char updateState(double[] probVector,int size);
    char updateCost();
};

