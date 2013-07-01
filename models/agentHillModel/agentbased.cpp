//
//  agentbased.cpp
//  
//
//  Created by mhcuser on 6/28/13.
//
//

#include <iostream>
using namespace std;
#include "agentbased.h"
//#include <array>


int main()
{

    turtles-own [
             int dstate        // state of TB disease (latentLTBI=0, acuteLTBI=1, infectiousATBI=2, non-infectiousATBI=3)
             // Note: dstate = 0 by default
             int nstate        // state of turtle in the next time step, may or may not be the same as dstate
             ]


    mu0 = 1/78;
    mu1 = 1/53;
    ro  =0.018;
    alpha =0.005;
    p =0.103;
    vF =1.5;
    l0 = 0.015;
    l1 =0.211;
    r0 =0.667;
    r1 =0.780;
    vL0 =0.0014;
    vL1 =0.0010;
    q =0.708;
    mud =0.115;
    x =0.111;
    ARI0 =0.030 / 100;
    beta =10.39;
    e0 =0.965;
    e1 =0.985;
    g =0.0047;
    phi0 =1.114;
    phi1 =1.167;
    sigmaF0 =1.296;
    sigmaF1 =1.301;

    sigmaL = 0.057;
    f = 0.187;
    
    if (popConst < 1)
        popConst = 1000;
        // parameter to relate population to actual number of turtles in model
        // the reason it's large is to ensure each compartment contains at least one initial person
    return 0;
}

