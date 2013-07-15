breed [USBs USB]
breed [FBs FB]

globals [
  mu0     ;; Natural mortality rate USB per year
  mu1     ;; Natural mortality rate FB per year
  ro      ;; USB birth rate per year
  alpha   ;; FB birth rate per year
  p       ;; Fraction of new infectionsn which are acute (fast progressors)
  vF      ;; Progression rate of acute infection per year
  l0      ;; Prevalence of LTBI in the USB population in 2000
  l1      ;; Prevalence of LTBI in the FB population in 2000
  r0      ;; Fraction of cases due to reactivation in the USB population
  r1      ;; Fraction of cases due to reactivation in the FB population
  vL0     ;; Progression rate for reactivation (chronic LTBI) in the USB population per year
  vL1     ;; Progression rate for reactivation (chronic LTBI) in the FB population per year
  q       ;; Fraction of infections progressing to infectious disease
  mud     ;; Mortality rate due to TB per year
  x       ;; Fraction of re-infected chronic LTBI moving to acute infection
  f       ;; Fraction of FB arrivals with LTBI
  ARI0    ;; Annual risk of infection for USB in 2000
  beta    ;; Effective contact rate per year
  e0      ;; Fraction of preferred contacts with own population for USB
  e1      ;; Fraction of preferred contacts with own population for FB
  g       ;; Fraction of FB arrivals with LTBI who are fast progressors
  phi0    ;; Cumulative fraction self-cure and treatment of active disease for both populations per year RATES (USB)
  phi1    ;; Cumulative fraction self-cure and treatment of active disease for both populations per year RATES (FB)
  sigmaF0 ;; Cumulative fraction of treatment for acute infection for both populations per year RATES (USB)
  sigmaF1 ;; Cumulative fraction of treatment for acute infection for both populations per year RATES (FB)
  sigmaL  ;; Treatment rate for chronic LTBI per year
  initial_N0 ; USB population in 2000
  initial_N1 ; FB population in 2000
  
  ;; We handle susceptibles only by population
  S0pop   ;; population of USB Susceptibles (S0)
  S1pop   ;; population of FB Susceptibles (S1)
  ;; "Snapshot" variables which need to be global
  deltaS0
  deltaS1
  totpop
  lambda0
  lambda1
]

turtles-own [
  dstate        ;; state of TB disease (latentLTBI=0, acuteLTBI=1, infectiousATBI=2, non-infectiousATBI=3)
                ;; Note: dstate = 0 by default
  nstate        ;; state of turtle in the next time step, may or may not be the same as dstate
]

to setup
  clear-all
  setup-globals
  setup-turtles
  reset-ticks
end

to setup-globals
  set mu0 1 / 78
  set mu1 1 / 53
  set ro 0.018
  set alpha 0.005
  set p 0.103
  set vF 1.5
  set l0 0.015
  set l1 0.211
  set r0 0.667
  set r1 0.780
  set vL0 0.0014
  set vL1 0.0010
  set q 0.708
  set mud 0.115
  set x 0.111
  set ARI0 0.030 / 100
  set beta 10.39
  set e0 0.965
  set e1 0.985
  set g 0.0047
  set phi0 1.114
  set phi1 1.167
  set sigmaF0 1.296
  set sigmaF1 1.301
  set initial_N0 250
  set initial_N1 31.4
  
  set sigmaL 0.057
  set f 0.187
  if popConst < 1000 [set popConst 1000]
  ;; parameter to relate population to actual number of turtles in model
  ;; the reason it's large is to ensure each compartment contains at least one initial person
end

to setup-turtles
  ;; let creates a local variable
  ;; New Cases in Population
  let newCases0 0.008714
  let newCases1 0.007554
  
  ;; WARNING: THE METHOD OF CALCULATING THE SIZE OF EACH COMPARTMENT IS IFFY
  ;; Acute (Fast) LTBI
  ;; NOTE: spaces between operators are necessary
  create-USBs round ((1 - r0) * newCases0 * popConst / vF) [ new-usb 1 ] ; (F0)
  create-FBs round ((1 - r1) * newCases1 * popConst / vF) [ new-fb 1 ]  ; (F1)
  ;; Chronic (Slow) LTBI ;; BIG
  create-USBs round (r0 * newCases0 * popConst / vL0) [ new-usb 0 ] ; (L0)
  create-FBs round (r1 * newCases1 * popConst / vL1) [ new-fb 0]  ; (L1)
  ;; Infectious TB
  create-USBs round (q * newCases0 * popConst / (mu0 + mud + phi0)) [ new-usb 2 ] ; (I0)
  create-FBs round (q * newCases1 * popConst / (mu1 + mud + phi1)) [ new-fb 2 ]  ; (I1)
  ;; Non-infectious TB
  create-USBs round ((1 - q) * newCases0 * popConst / (mu0 + mud + phi0)) [ new-usb 3 ] ; (J0)
  create-FBs round ((1 - q) * newCases1 * popConst / (mu1 + mud + phi1)) [ new-fb 3 ]  ; (J1)
  ;; Susceptibles
  set S0pop initial_N0 * popConst - count usbs
  set S1pop initial_N1 * popConst - count fbs
end

to go
  ifelse ticks < totT / deltat [
    find-next-state
    ;; do births, arrivals, and new infections second since they create turtles
    US-births
    FB-arrivals
    new-infections
    ;; finally, move turtles in compartments
    move-all
    tick]
  [stop]
end

;; put everything in one variable to avoid unnecessary global variables
to find-next-state
  ;; temporary variables which capture the current state of the system
  let L0pop count usbs with [dstate = 0]   ; current L0 population
  let F0pop count usbs with [dstate = 1]   ; current F0 population
  let I0pop count usbs with [dstate = 2]   ; current I0 population
  let J0pop count usbs with [dstate = 3]   ; current J0 population
  let L1pop count fbs with [dstate = 0]    ; current L1 population
  let F1pop count fbs with [dstate = 1]    ; current F1 population
  let I1pop count fbs with [dstate = 2]    ; current I1 population
  let J1pop count fbs with [dstate = 3]    ; current J1 population
  let N0pop L0pop + F0pop + I0pop + J0pop + S0pop
  let N1pop L1pop + F1pop + I1pop + J1pop + S1pop
  set totpop N0pop + N1pop
  ;; reset deltaS0 and deltaS1
  set deltaS0 0
  set deltaS1 0
  
  ;; Determine next state via cumulative distributions
  ;; For now, naively assume risk = rate
  let c01 (1 - e0) * ((1 - e1) * N1pop) / ((1 - e0) * N0pop + (1 - e1) * N1pop)
  let c00 1 - c01
  let c10 (1 - e1) * ((1 - e0) * N0pop) / ((1 - e0) * N0pop + (1 - e1) * N1pop)
  let c11 1 - c10
  set lambda0 beta * (c00 * (I0pop / N0pop) + c01 * (I1pop / N1pop))
  set lambda1 beta * (c10 * (I0pop / N0pop) + c11 * (I1pop / N1pop))
  
  ;; Chronic LTBI (L)
  let vec0 cvec (list torisk(sigmaL) torisk(mu0) torisk(q * vL0) torisk((1 - q) * vL0) torisk(x * p * lambda0))
  let vec1 cvec (list torisk(sigmaL) torisk(mu1) torisk(q * vL1) torisk((1 - q) * vL1) torisk(x * p * lambda1))
  ; US-borns
  ask usbs with [dstate = 0] [
    let rand random-float 1
    ifelse rand < item 0 vec0 [ ; latent TB treatment (=> S0)
      set nstate 4
      set deltaS0 deltaS0 + 1]
    [ifelse rand < item 1 vec0 [ ; death due to natural causes
      set nstate 4]
    [ifelse rand < item 2 vec0 [ ; progression to infectious TB (=> I0)
      set nstate 2]
    [ifelse rand < item 3 vec0 [ ; progression to infectious TB (=> J0)
      set nstate 3]
    [ifelse rand < item 4 vec0 [ ; re-infection (=> F0)
      set nstate 1] []]]]] ; otherwise, stay
  ]
  ; Foreign-borns
  ask fbs with [dstate = 0] [
    let rand random-float 1
    ifelse rand < item 0 vec1 [ ; latent TB treatment (=> S0)
      set nstate 4
      set deltaS1 deltaS1 + 1]
    [ifelse rand < item 1 vec1 [ ; death due to natural causes
      set nstate 4]
    [ifelse rand < item 2 vec1 [ ; progression to infectious TB (=> I1)
      set nstate 2]
    [ifelse rand < item 3 vec1 [ ; progression to non-infectious (=> J1)
      set nstate 3]
    [ifelse rand < item 4 vec1 [ ; re-infection (=> F1)
      set nstate 1] []]]]]
  ]
  
  ;; Acute LTBI (F)
  set vec0 cvec (list torisk(sigmaF0) torisk(mu0) torisk(q * vF) torisk((1 - q) * vF))
  set vec1 cvec (list torisk(sigmaF1) torisk(mu1) torisk(q * vF) torisk((1 - q) * vF))
  ; US-borns
  ask usbs with [dstate = 1] [
    let rand random-float 1
    ifelse rand < item 0 vec0 [ ; acute LTBI treatment (=> S0)
      set nstate 4
      set deltaS0 deltaS0 + 1]
    [ifelse rand < item 1 vec0 [ ; death due to natural causes
      set nstate 4]
    [ifelse rand < item 2 vec0 [ ; progression to infectious TB (=> I0)
      set nstate 2]
    [ifelse rand < item 3 vec0 [ ; progression to non-infectious TB (=> J0)
      set nstate 3] []]]] ; otherwise, stay
  ]
  ; Foreign-borns
  ask fbs with [dstate = 1] [
    let rand random-float 1
    ifelse rand < item 0 vec1 [ ; acute LTBI treatment (=> S1)
      set nstate 4
      set deltaS1 deltaS1 + 1]
    [ifelse rand < item 1 vec1 [ ; death due to natural causes
      set nstate 4]
    [ifelse rand < item 2 vec1 [ ; progression to infectious TB (=> I1)
      set nstate 2]
    [ifelse rand < item 3 vec1 [ ; progression to non-infectious TB (=> J1)
      set nstate 3] []]]] ; otherwise, stay
  ]
  
  ;; Infectious/Non-infectious TB (I + J)
  set vec0 cvec (list torisk(phi0) torisk(mud) torisk(mu0))
  set vec1 cvec (list torisk(phi1) torisk(mud) torisk(mu1))
  ; US-borns
  ask usbs with [member? dstate [2 3]] [
    let rand random-float 1
    ifelse rand < item 0 vec0 [ ; self-cure/active disease treatment (=> S0)
      set nstate 4
      set deltaS0 deltaS0 + 1]
    [ifelse rand < item 1 vec0 [ ; death due to TB
      set nstate 4]
    [ifelse rand < item 2 vec0 [ ; death due to natural causes
      set nstate 4] []]]
  ]
  ; Foreign-borns
  ask fbs with [member? dstate [2 3]] [
    let rand random-float 1
    ifelse rand < item 0 vec1 [ ; self-cure/active disease treatment (=> S1)
      set nstate 4
      set deltaS1 deltaS1 + 1]
    [ifelse rand < item 1 vec1 [ ; death due to TB
      set nstate 4]
    [ifelse rand < item 2 vec1 [ ; death due to natural causes
      set nstate 4] []]]
  ]
end

;; avoid using popConst below since it leads to exponential growth
to US-births
  set deltaS0 deltaS0 + round(ro * totpop * deltaT) ; US birth (-> S0)
end

to FB-arrivals
  set deltaS1 deltaS1 + round((1 - f) * alpha * totpop * deltaT)                ; susceptible arrival (-> S1)
  create-fbs round (g * p * f * alpha * totpop * deltaT) [new-fb 1]       ; acute LTBI arrival (-> F1)
  create-fbs round ((1 - g * p) * f * alpha * totpop * deltaT) [new-fb 0] ; latent LTBI arrival (-> L1)
end

;; technically these should be binomial(S0pop, lambda0)
;; but since S0pop is big and lambda0 is small
;; we can use the Poisson approximation
to new-infections
  ; new USB infections
  let usinfec random-poisson (lambda0 * S0pop * deltaT)
  let newf0 round (p * usinfec)
  let newl0 round ((1 - p) * usinfec)
  set deltaS0 deltaS0 - newf0 - newl0
  create-usbs round newf0 [new-usb 1]       ; new USB acute LTBI infection (=> F0)
  create-usbs round newl0 [new-usb 0] ; new USB latent LTBI infection (=> L0)
  ; new FB infections
  let fbinfec random-poisson (lambda1 * S1pop * deltaT)
  let newf1 round (p * fbinfec)
  let newl1 round ((1 - p) * fbinfec)
  set deltaS1 deltaS1 - newf1 - newl1
  create-fbs round newf1 [new-fb 1]       ; new FB acute LTBI infection (=> F1)
  create-fbs round newl1 [new-fb 0] ; new FB latent LTBI infection (=> L1)
  ; death by natural causes
  set deltaS0 deltaS0 - round ( random-poisson(mu0 * S0pop * deltaT) )
  set deltaS1 deltaS1 - round ( random-poisson(mu1 * S1pop * deltaT) )
end

to move-all
  ; move turtles
  ask turtles [
    if dstate != nstate [
      ifelse nstate = 4 [die]
      [set dstate nstate update-color]
  ]]
  ; update S0pop/S1pop according to deltaS0/deltaS1
  set S0pop S0pop + deltaS0
  set S1pop S1pop + deltaS1
end

;; subroutines
to-report torisk [rate] ;; global procedure
  report rate * deltaT
end

; return cumulative probability distribution of pvec
to-report cvec [pvec]
  ;; basically, this builds cvec by incrementally adding the elements of pvec
  report butfirst reduce [lput (?2 + last ?1) ?1] fput [0] pvec
end

to new-usb [state]
  set dstate state
  set nstate state
  ; these drawing commands can be commented out,
  ; but it doesn't really improve performance
  set shape "square"
  setxy random-xcor random-ycor
  update-color
end

to new-fb [state]
  set dstate state
  set nstate state
  ; these drawing commands can be commented out,
  ; but it doesn't really improve performance
  set shape "triangle"
  setxy random-xcor random-ycor
  update-color
end

to update-color ;; turtle procedure
  ifelse dstate = 0 [set color 2]    ;(L) gray 
  [ifelse dstate = 1 [set color 45]   ;(F) yellow
    [ifelse dstate = 2 [set color 15] ;(I) red
      [set color 25]]]                ;(J) orange
end
@#$#@#$#@
GRAPHICS-WINDOW
484
10
1262
809
64
64
5.9535
1
10
1
1
1
0
1
1
1
-64
64
-64
64
1
1
1
ticks
30.0

SLIDER
21
17
193
50
deltaT
deltaT
0
1
0.1
0.001
1
NIL
HORIZONTAL

SLIDER
18
56
190
89
totT
totT
100
500
100
1
1
NIL
HORIZONTAL

BUTTON
10
161
76
194
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
173
161
236
194
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

BUTTON
84
161
165
194
go once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

MONITOR
15
201
119
246
NIL
S0pop
17
1
11

MONITOR
126
202
230
247
NIL
S1pop
17
1
11

MONITOR
16
250
87
295
non-S pop
count turtles
17
1
11

PLOT
268
14
468
164
US borns
time
people
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"F0" 1.0 0 -4079321 true "" "plot count usbs with [dstate = 1]"
"I0" 1.0 0 -2674135 true "" "plot count usbs with [dstate = 2]"
"J0" 1.0 0 -955883 true "" "plot count usbs with [dstate = 3]"

PLOT
269
171
469
321
Foreign borns
time
people
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"F1" 1.0 0 -4079321 true "" "plot count fbs with [dstate = 1]"
"I1" 1.0 0 -2674135 true "" "plot count fbs with [dstate = 2]"
"J1" 1.0 0 -955883 true "" "plot count fbs with [dstate = 3]"

PLOT
270
329
470
479
Chronic LTBIs
time
people
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13345367 true "" "plot count usbs with [dstate = 0]"
"pen-1" 1.0 0 -5825686 true "" "plot count fbs with [dstate = 0]"

PLOT
23
326
223
476
Incidence
NIL
NIL
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"default" 1.0 0 -13345367 true "" "plot 1e6 * (vF * count usbs with [dstate = 1] + vL0 * count usbs with [dstate = 0]) / (S0pop + count usbs)"
"pen-1" 1.0 0 -5825686 true "" "plot 1e6 * (vF * count fbs with [dstate = 1] + vL1 * count fbs with [dstate = 0]) / (S1pop + count fbs)"
"pen-2" 1.0 0 -13840069 true "" "plot 1e6 * (vF * count turtles with [dstate = 1] + vL0 * count usbs with [dstate = 0] + vL1 * count fbs with [dstate = 0]) / (S0pop + S1pop + count turtles)"

INPUTBOX
19
94
174
154
popConst
5000
1
0
Number

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="final size experiment" repetitions="12" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>count usbs with [dstate = 0]</metric>
    <metric>count fbs with [dstate = 0]</metric>
    <enumeratedValueSet variable="initial_N0">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="totT">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deltaT">
      <value value="0.3"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial_N1">
      <value value="31.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="popConst">
      <value value="10000"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="incidence experiment" repetitions="12" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>1e6 * (vF * count usbs with [dstate = 1] + vL0 * count usbs with [dstate = 0]) / (S0pop + count usbs)</metric>
    <metric>1e6 * (vF * count fbs with [dstate = 1] + vL1 * count fbs with [dstate = 0]) / (S1pop + count fbs)</metric>
    <metric>1e6 * (vf * count turtles with [dstate = 1] + vL0 * count usbs with [dstate = 0] + vL1 * count fbs with [dstate = 0]) / (S0pop + S1pop + count turtles)</metric>
    <enumeratedValueSet variable="initial_N0">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial_N1">
      <value value="31.4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deltaT">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="popConst">
      <value value="10000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="totT">
      <value value="100"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="fb final incidence" repetitions="8" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <metric>1e6 * (vF * count fbs with [dstate = 1] + vL1 * count fbs with [dstate = 0]) / (S1pop + count fbs)</metric>
    <enumeratedValueSet variable="popConst">
      <value value="5000"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial_N0">
      <value value="250"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="deltaT">
      <value value="0.1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="totT">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial_N1">
      <value value="31.4"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
