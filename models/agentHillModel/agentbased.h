//
//  agentbased.h
//  
//
//  Created by mhcuser on 6/28/13.
//
//

#ifndef ____agentbased__
#define ____agentbased__

#include <iostream>
#include "turtle.h"

void createTurtles(turtle::State turtState, turtle::COB cob, int timeStep, int numTurtles);
void updatePop(turtle::State turtState, turtle::COB cob, int timeStep, int numTurtles = 1);

#endif /* defined(____agentbased__) */
