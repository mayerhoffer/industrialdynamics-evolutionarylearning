;; ====================================================== DECLARATIONS ======================================================
;extensions [ Rnd ];profiler] ;; for roulette wheel selection / for network generation

globals [
  used-random-seed                   ;; stores the used random seed for replication
  randomlist                         ;; initializes an externally generated list of random numbers (e.g. to use a Beta distribution)
  randomlistlength                   ;; number of entries in the random number list
  mean-global-growth                 ;; average growth of all turtles (for statistical purposes)
  max-age                            ;; maximum age since simulation start (for statistical purposes)
  max-globalShare                    ;; maximum firnm size since simulation start (for statistical purposes)
  max-global-localShare              ;; maximum localized firm size since simulation start (for statistical purposes)
  globalHiHi                         ;; HiHi index
]

turtles-own [
  learning                           ;; sets the learning regime to which the turtle adheres
  fitness                            ;; current fitness/productivity of the turtle
  fitness-old                        ;; fitness/productivity of the turtle from last period
  globalShare                        ;; current market share of the turtle
  globalShare-old                    ;; market share of the turtle from last period
  myGlobalGrowth                     ;; share growth beteen last and current period (NA if firm 'died')
  myGlobalGrowthList                 ;; list of all growths since simulation start
  localShare                         ;; current localized market power
  localShare-old                     ;; localized market power from last period
  myLocalGrowth                      ;; growth in localized market powerbeteen last and current period (NA if firm 'died')
  num-deaths                         ;; number of deaths since simulation start
  died?                              ;; indicate whether the turtle "died" this period (for internal calculations)
  age                                ;; periods since last "death"/simulation start
]


;; ====================================================== SETUP PROCEDURES ======================================================
to SETUP
  clear-all
  SETUP-SEED
  SETUP-NETWORK
  INITIALISE-RANDOM
  ASSIGN-VARIABLES
  ASSIGN-LEARNING
  reset-ticks
end ;; SETUP


to SETUP-SEED  ;; Initializes predefined random seed or saves and shows one determined by NetLogo.
  set used-random-seed ifelse-value (randomSeed?) [ randomSeed ] [ new-seed ]
  random-seed used-random-seed
  output-print used-random-seed
end ;; SETUP-SEED


to INITIALISE-RANDOM  ;; Reads an externally defined list of random numbers.
  set randomlist (list)
  file-open listName
  while [not file-at-end?] [
    set randomlist lput file-read randomlist
  ]
  set randomlistlength (length randomlist)
  file-close
end ;; INITIALISE-RANDOM


to SETUP-NETWORK  ;; Can offer different network structures.
  SETUP-NETWORK-RANDOM
end ;; SETUP-NETWORK



to SETUP-NETWORK-RANDOM  ;; A variant of the classic Erdős-Rényi where each possible pair of nodes gets a chance to create a link between them with a specified probability.
  repeat population [
    create-turtles 1 [
      set shape "dot"
      ask other turtles [
        if random-float 1.0 < random-link-prob [
        create-link-with myself
        ]
      ]
    ]
  ]
end ;; SETUP-NETWORK-RANDOM


to ASSIGN-VARIABLES  ;; Assignes initial variables to turtles
  ask turtles [
    set fitness "NA"
    set fitness-old 1
    set globalShare "NA"
    set globalShare-old ( 1 / population )
    set myGlobalGrowth 0
    set localShare "NA"
    set localShare-old ( 1 / (count in-link-neighbors + 1) )
    set myLocalGrowth 0
    set num-deaths 0
    set died? false
    set age 0
    set myGlobalGrowthList (list)
  ]
end ;; ASSIGN-VARIABLES


to ASSIGN-LEARNING  ;; Assigns learning regimes to turtles
  ask turtles [
    set learning "baseline"
  ]
end ;; ASSIGN-LEARNING


;; ====================================================== MAIN LOOP ======================================================
to go  ;; Main procedure.
  LEARN
  CALCULATE-SHARE
  DEATH-BIRTH
  STATISTICS
  ADJUST-VARIABLES
  ask turtles [ COLORIZE ]
  tick
  ;; Before halting the simulation at a user-defined endstep, this stores core turtle-variables in csv-files
  if (ticks = endstep) [
    OUTPUT-CSV
    stop
  ]
end ;;go


;; ====================================================== SIMULATION DYNAMICS ======================================================
to LEARN  ;; Turtles carry out their learning procedures as per their pre-defined learning regime.
  ask turtles [
    set fitness fitness-old * ( 1 + max (list 0 min (list (item random randomlistlength randomlist) MUmax) ) )
  ]
end ;;LEARN


to CALCULATE-SHARE  ;; Turtles carry out their learning procedures as per their pre-defined learning regime.
  CALCULATE-GLOBAL-SHARE
  CALCULATE-LOCAL-SHARE
end ;;CALCULATE-SHARE


to CALCULATE-GLOBAL-SHARE  ;; Turtles calculate their global share as defined by Dosi et al. 2017
  let avg-globalFitness (sum [fitness * globalShare-old] of turtles)
  ask turtles [
    set globalShare (globalShare-old * ( 1 + replicatorStrength * ((fitness - avg-globalFitness ) / avg-globalFitness )))
  ]
;  show min [globalShare] of turtles
;  CALCULATE-LOGS
end ;; CALCULATE-GLOBAL-SHARE


to CALCULATE-LOCAL-SHARE  ;; Turtles calculate their localized market power, following the notion of Dosi et al. 2017
;; Calculation assigning all turtles without link-neighbors a localShare of 1.
  ask turtles [
    ifelse (any? in-link-neighbors) [
      let avg-localFitness (sum [fitness * localShare-old] of in-link-neighbors  + [fitness * localShare-old] of self)
      set localShare (localShare-old * ( 1 + replicatorStrength * ((fitness - avg-localFitness) / avg-localFitness)))
    ]
    [
      set localShare 1
    ]
  ]
end ;; CALCULATE-LOCAL-SHARE

;to CALCULATE-LOCAL-SHARE
;;;Alternative CALCULATION: Slower, but potentially more versatile
;  let turtleswithneighbors (turtles with [any? in-link-neighbors])
;  ask turtleswithneighbors [
;    let avg-localFitness (sum [fitness * localShare-old] of in-link-neighbors  + [fitness * localShare-old] of self)
;    set localShare (localShare-old * ( 1 + replicatorStrength * ((fitness - avg-localFitness) / avg-localFitness)))
;  ]
;  let mean-meaningfulShares (mean [localShare] of turtleswithneighbors)
;  ask turtles with [not any? in-link-neighbors] [
;    set localShare 1 ;; OR: mean-meaningfulShares
;  ]
;end ;; CALCULATE-LOCAL-SHARE


to DEATH-BIRTH  ;; Ageing of turtles and potential death/rebirth
  let avg-globalFitness (sum [fitness-old * globalShare-old] of turtles ) ;; Used below for rebirth; intentionally different from above ("old")
     ;; Alternatives: with [globalShare > 0.001]) ;OR mean [fitness] of turtles
  ask turtles [
    ;; first: ordinary ageing for turtles
    set age (age + 1)
    set died? false
    ;; death as reset of the turtle
    ifelse globalShare < (0.15 / population) [
      if rebirth-relink? [
        ;; most variables: set as at initialization (and adjusted below)
        ask my-links [die]
        ask other turtles [
          if random-float 1.0 < random-link-prob [
            create-link-with myself
          ]
        ]
      ]
      set globalShare (1 / population)
      set localShare (1 / (count my-links + 1))
      ;; localized average productivity (following the logic from Dosi et al. 2017)
      let avg-localFitness ifelse-value (any? in-link-neighbors) [(sum [fitness-old * localShare-old] of in-link-neighbors)] [avg-globalFitness] ;; Intentionally different from above. If no neighbors, take global average
      set fitness ( avg-localFitness * ( 1 + min (list (item (random randomlistlength) randomlist) MUmax) ) )
      set num-deaths (num-deaths + 1)
      set died? true
      set age 0
      set myGlobalGrowth 100
      set myGlobalGrowthList (list)
      set myLocalGrowth ifelse-value (any? in-link-neighbors) [100] ["NA"]
    ]
    [
      set myGlobalGrowth (ln globalShare - ln globalShare-old)
      set myGlobalGrowthList (lput myGlobalGrowth myGlobalGrowthList)
      set myLocalGrowth ifelse-value (any? in-link-neighbors) [ln localShare - ln localShare-old] [ "NA"]
    ]
  ]
end ;; DEATH-BIRTH


to ADJUST-VARIABLES  ;; Adjusts variables to adequately represent the network/shares after birth-death
  let sum-globalShares (sum [globalShare] of turtles)
;  show sum-globalShares
  ask turtles [
    set globalShare-old (globalShare / sum-globalShares)
    set localShare-old ( localShare / (sum [localShare] of in-link-neighbors + localShare) )
  ]
  ask turtles [
    set globalShare "NA"
    set localShare "NA"
     ;; potentially work with lower precision
;    set globalShare-old (precision globalShare-old 10)
;    set localShare-old (precision localShare-old 10)
    set fitness-old fitness
    set fitness "NA"
  ]
end ;; ADJUST-SHARE


;; ====================================================== OUTPUT ======================================================
to STATISTICS  ;; calculates some output statistics
  set mean-global-growth mean [(globalShare - globalShare-old) / globalShare-old] of turtles with [not died?]
  set max-globalShare (max (list max-globalShare (max [globalShare] of turtles)))
  let meanglobalshare mean [globalShare] of turtles
  set globalHiHi (sum ([(globalShare / meanglobalshare) ^ 2] of turtles))
end ;; STATISTICS


to OUTPUT-CSV  ;; stores various turtles variables in a CSV File
;; FORMAT: Each line represents a single time step. Turtles are separated by tab-stops and each line ends with information on the run
  ;; globalShare
  let i 0
  file-open "globalShares.csv"
  repeat (population + 1) [
    ask turtles with [who = i] [
      ifelse (globalShare-old < (0.15 / population) ) [
        file-write "NA" ; ODER globalShare "NA"
      ]
      [
        file-write globalShare-old
      ]
    ]
    set i (i + 1)
  ]
  file-print (word ";" ticks ";" used-random-seed ";" random-link-prob)
  file-close

  ;; localShare
  set i 0
  file-open "localShares.csv"
  repeat (population + 1) [
    ask turtles with [who = i] [
      file-write localShare-old
    ]
    set i (i + 1)
  ]
  file-print (word ";" ticks ";" used-random-seed ";" random-link-prob)
  file-close

  ;; age
  set i 0
  file-open "age.csv"
  repeat (population + 1) [
    ask turtles with [who = i] [
      file-write age
    ]
    set i (i + 1)
  ]
  file-print (word ";" ticks ";" used-random-seed ";" random-link-prob)
  file-close

  ;; num-deaths
  set i 0
  file-open "num-deaths.csv"
  repeat (population + 1) [
    ask turtles with [who = i] [
      file-write num-deaths
    ]
    set i (i + 1)
  ]
  file-print (word ";" ticks ";" used-random-seed ";" random-link-prob)
  file-close

  ;; fitness
  set i 0
  file-open "fitness.csv"
  repeat (population + 1) [
    ask turtles with [who = i] [
      file-write fitness-old
    ]
    set i (i + 1)
  ]
  file-print (word ";" ticks ";" used-random-seed ";" random-link-prob)
  file-close

  ;; myGlobalGrowth
  if ticks = endstep [
  set i 0
  file-open "myGlobalGrowth.csv"
  repeat (population + 1) [
    ask turtles with [who = i] [
      file-write myGlobalGrowth
    ]
    set i (i + 1)
  ]
  file-print (word ";" ticks ";" used-random-seed ";" random-link-prob)
  file-close
  ]

  ;; myGlobalGrowth-historical mean
  if ticks = endstep [
  set i 0
  file-open "myGlobalGrowth-historical-mean.csv"
  repeat (population + 1) [
    ask turtles with [who = i] [
        file-write ifelse-value (age > 1) [mean myGlobalGrowthList] ["NA"]
    ]
    set i (i + 1)
  ]
  file-print (word ";" ticks ";" used-random-seed ";" random-link-prob)
  file-close
  ]

  ;; myGlobalGrowth-historical SD
  if ticks = endstep [
  set i 0
  file-open "myGlobalGrowth-historical-SD.csv"
  repeat (population + 1) [
    ask turtles with [who = i] [
      file-write ifelse-value (age > 1) [standard-deviation  myGlobalGrowthList] ["NA"]
    ]
    set i (i + 1)
  ]
  file-print (word ";" ticks ";" used-random-seed ";" random-link-prob)
  file-close
  ]

  ;; myLocalGrowth
  if ticks = endstep [
  set i 0
  file-open "myLocalGrowth.csv"
  repeat (population + 1) [
    ask turtles with [who = i] [
      file-write myLocalGrowth
    ]
    set i (i + 1)
  ]
  file-print (word ";" ticks ";" used-random-seed ";" random-link-prob)
  file-close
  ]


end ;; OUTPUT-CSV

;; ADJUST
to COLORIZE
;  let coloroption int ((count my-links with [interdecile?] / count my-links) * 10)
;  set color (125 - coloroption * 10)
  setxy (min (list 100 (fitness-old * 100) )) (min (list 100 (globalShare-old * 100) ))
  set size 5
end
@#$#@#$#@
GRAPHICS-WINDOW
265
155
493
384
-1
-1
2.18
1
10
1
1
1
0
1
1
1
0
100
0
100
0
0
1
ticks
30.0

SLIDER
205
45
380
78
population
population
0
10000
150.0
1
1
NIL
HORIZONTAL

SWITCH
15
45
195
78
randomSeed?
randomSeed?
1
1
-1000

INPUTBOX
15
80
195
140
randomSeed
1000.0
1
0
Number

OUTPUT
15
160
195
214
11

BUTTON
180
405
420
530
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
435
405
675
530
NIL
go
NIL
1
T
OBSERVER
NIL
G
NIL
NIL
0

SLIDER
410
45
582
78
replicatorStrength
replicatorStrength
0
1
1.0
0.01
1
NIL
HORIZONTAL

TEXTBOX
15
10
165
36
Basic Settings
21
0.0
1

TEXTBOX
410
10
655
61
Learning and Competition
21
0.0
1

SLIDER
205
80
380
113
random-link-prob
random-link-prob
0
1
0.1
0.01
1
NIL
HORIZONTAL

INPUTBOX
15
240
195
300
listName
rescaleddraw.csv
1
0
String

INPUTBOX
15
305
195
365
endstep
200.0
1
0
Number

PLOT
695
305
1155
560
Histogram growth
NIL
NIL
-2.0
2.0
0.0
50.0
false
false
"" ""
PENS
"default" 0.05 1 -16777216 true "histogram [myGlobalGrowth] of turtles" "histogram [myGlobalGrowth] of turtles"

PLOT
1160
305
1620
560
Histogram age
NIL
NIL
0.0
100.0
0.0
50.0
false
false
"" ""
PENS
"pen-0" 1.0 1 -16777216 true "" "histogram [age] of turtles"

SLIDER
410
80
582
113
MUmax
MUmax
0
1
1.0
0.05
1
NIL
HORIZONTAL

SWITCH
205
115
380
148
rebirth-relink?
rebirth-relink?
0
1
-1000

TEXTBOX
20
145
170
163
Used random seed
11
0.0
1

TEXTBOX
205
10
355
36
World structure
21
0.0
1

TEXTBOX
700
10
995
61
Key output variables
21
0.0
1

PLOT
695
45
1620
300
Histogram market share
NIL
NIL
0.0
1.0
0.0
50.0
false
false
"" ""
PENS
"default" 0.002 1 -16777216 false "histogram [globalShare-old] of turtles" "histogram [globalShare-old] of turtles"

MONITOR
1470
60
1612
105
Current maximum share
max [globalShare-old] of turtles
10
1
11

@#$#@#$#@
# ODD Description of the Model
This description follows the ODD (Overview, Design concepts, Details) protocol for describing individual- and agent-based models in its first updated version of 2010:
**V. Grimm et al** (2010). The ODD protocol: A review and first update. _Ecological Modelling. Vol. 221, No. 23 (p. 2760–2768).

## Overview

### Purpose
This model introduces a network layer to the economic model on learning and industrial dynamics: **Dosi, G., Pereira, M. C., and Virgillito, M. E.** (2017). The footprint of evolutionary processes of learning and selection upon the statistical properties of industrial dynamics. Industrial and Corporate Change, 26(2)

Its purpose is to understand the impact of localized competition represented as network proximity on the distribution of market shares, their growth rates, and firm age.

### Entities, state variables, and scales
Agents represent firms. They are characterised by the following state variables:

  * __necessary for dynamics__
    * __learning__ Sets the learning regime to which the firm adheres.
    * __fitness__ Current fitness/productivity of the firm
    * __fitness(t-1)__ Fitness/productivity of the firm from last period
    * __globalShare(t)__ Current market share of the firm
    * __globalShare(t-1)__ Market share of the firm from last period
    * __localizedPower(t)__ Current localized market power
    * __localizedPower(t-1)__ Localized market power from last period
    * __died?__ Indicate whether the agent "died" this period (for internal calculations).

  * __for statistical/output purposes__
    * __myLocalGrowth__ Growth in localized market power beteen last and current period (NA if firm 'died')
    * __num-deaths__ Number of deaths since simulation start
    * __age__ Periods since last 'death'/simulation start
    * __myGlobalGrowth__ Share growth between last and current period (NA if firm 'died')
    * __myGlobalGrowthList__ List of all growth rates since simulation start

Links in the model represent a competitive relationship between the two agents that they connect. They possess no state variables.

### Process overview and scheduling
The model proceeds in time steps that represent a business-period (e.g. a year). In each step, all firms first have the chance to improve their productivity in the stochastic learning procedure. Afterward, the global market share and localized market power of all firms are calculated based on new productivities. These productivities are then used to determine which firms leave the market and are replaced by entrants (the corresponding agents are reset) and update statistical variables for surviving incumbents. Finally, all shares are adjusted to fit in the new entrants and statistics are calculated:

  1. Stochastic learning: Firms (potentially) increase their productivity.
  2. Assessment of global shares and localized market powers.
  3. Update of individual agent variables and death-birth
  4. Adjustment of shares
  5. Calculation of output statistics

## Design concepts
**Basic principles**
The model understands rationality as bounded.
Its output mirrors empirical situations but no empirical data is explicitly used for calibration.

**Emergence**
The distribution of market shares, growth rates and firm ages emerges from actions and interaction (in form of network link formation) of individual agents.

In contrast, average productivity (growth) is more tightly imposed by the shape of the random distribution underlying the stochastic learning procedure.

**Adaption**
Firms do not adapt their behavior to their current situation.

**Objectives**
Firms do not have explicit objectives that govern their behavior.

**Learning**
Learning in this model is purely stochastic and only indirectly influenced by past experience: more successful learning in the past means a higher productivity level, and hence higher expected absolute gains (with equal expected relative gains).

**Prediction**
The model does not feature predictions by firms.

**Sensing**
Firms do perceive the productivies of their link neighbors at birth to calculate their own initial productivity. Apart from this, they possess no sensation.

**Interaction**
There is no direct interaction between firms but incumbents determine the initial productivity of entrants that link to them.

**Stochasticity**
Firms are randomly linked to each other (given a user-specified link probability) at initialisation. Entrants are linked to incumbents in the same way at rebirth.
Moreover, learning happens stochastically and is determined by a user-specified random function.
Procedures that update state variables of firms are carried out as simultaneous for all firms: The order in which firms are called for any of the procedures does not matter because all procedures work with values that were calculated before (or in the last period) for all firms.

**Collectives**
The model does not feature any collective agency.

**Observation**
Aggregate data is collected each time step. This includes:

  * current mean firm age
  * current median firm age
  * current max firm age
  * max firm age since simulation start
  * current mean market share
  * current median market share
  * current max market share
  * current max market share since simulation start
  * current Herfindahl-Hirschman-Index
Data from individual firms is collected at the last simulation step (user-defined, we advice any time after the simulation has stabilized or earlier for testing).

## Details

### Initialization
Firms are linked randomly according with a user-specified probability for each link. Their state-variables are the following:

  * fitness(t): "NA"
  * fitness(t-1): 1
  * globalShare(t): "NA"
  * globalShare(t-1): 1 / population
  * myGlobalGrowth: 0
  * myGlobalGrowthList: (empty list)
  * localizedPower(t): "NA"
  * localizedPower(t-1): 1 / (number of link neighbors + 1)
  * myLocalGrowth: 0
  * num-deaths: 0
  * died?: false
  * age: 0

### Input data
The user exogenously sets the link probability and stochastic learning fuctions.

Random calculations depend on a fixed randomization procedure with pre-set values determining the result of such calculations to guarantee reproducibility of the simulation results.

### Submodels
**main loop**
This calls all other procedures while the simulation is running, checks whether the user-specified number of total steps has been reached and updates the time counter.

**Learning**
Happens for all firms simultaneously.
A firm sets its new fitness: `fitness(t) = fitness(t-1) * ( 1 + max (list 0 min (list (random value,  MUmax)))`
Hence, the productivity gain of a firm is always at least 0 and at most a user-specified maximum value.

**Calculation of global market share**
Happens for all firms simultaneously.
A firm sets its global market share by comparing its own fitness to the global average fitness weighted by market share of the previous period: `globalShare(t) = globalShare(t-1) * ( 1 * ((fitness - sum_all-agents(fitness(t) * globalShare(t-1)) / sum_all-agents(fitness(t) * globalShare(t-1) )))`

**Calculation of localized market power**
Happens for all firms simultaneously.
A firm sets its localized market power by comparing its own fitness to the local average fitness weighted by localized market power of the previous period: `localizedPower(t) = localizedPower(t-1) * ( 1 * ((fitness - sum_link-neighbors-and-self(fitness(t) * localizedPower(t-1)) / sum_link-neighbors-and-self(fitness(t) * localizedPower(t-1)) )))`

**Firm update and death-birth process**
Happens for all firms simultaneously.
First, each firm sets its died? marker false and increases its age by 1. Moreover, it calculates myGlobalGrowth (ln globalShare(t) - ln globalShare(t-1)) and appends it to myGlobalGrowthList to its myGlobalGrowth list; myLocalGrowth is calculated the same way (ln localizedPower(t) - ln localizedPower(t-1)) or set "NA" if the firm has no link neighbors.

Afterward, firms whose global market share falls below 0.15/population die. All ther links are removed, new ones are generated with each other firm with initial link probability and all state variables are reset like at initialisation with the following exceptions: The died? marker is set true and both growth parameters are 100 to mark them for exclusion in analysis. Most importantly, the fitness of a reborn firm becomes the average weighted fitness of its new link neighbors altered by the learning procedure without a minimum: `fitness(t) = sum_link-neighbors-and-self(fitness(t) * localizedPower(t-1)) * ( 1 + min (list (random value,  MUmax))`

**Adjustment of shares**
Happens for all firms simultaneously.
To correct for changes by the death-birth procedure, firms divide their global shares by the sum of all global shares and thei localized market power by the sum of localized market powers of all link neighbors and themselves.

**Statistics**
Several statistics are trivially calculated by finding averages, maximum or minimum values or comparing them. To do this, the values in question are directly read from the agents' state variables.

**CSV output**
Happens for firmssequentially (ordered by ID).
Firms write each state variable in a separate CSV-file, such that there is a file for each variable that contains values for all firms separated.
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
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="data_for_paper" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>mean [age] of turtles</metric>
    <metric>median [age] of turtles</metric>
    <metric>max [age] of turtles</metric>
    <metric>max-age</metric>
    <metric>mean [globalShare-old] of turtles</metric>
    <metric>median [globalShare-old] of turtles</metric>
    <metric>max [globalShare-old] of turtles</metric>
    <metric>max-globalShare</metric>
    <metric>globalHiHi</metric>
    <steppedValueSet variable="random-link-prob" first="0" step="0.01" last="0.99"/>
    <enumeratedValueSet variable="rebirth-relink?">
      <value value="true"/>
    </enumeratedValueSet>
    <steppedValueSet variable="randomSeed" first="10" step="10" last="500"/>
    <enumeratedValueSet variable="randomSeed?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="listName">
      <value value="&quot;rescaleddraw.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="MUmax">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="replicatorStrength">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="endstep">
      <value value="200"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="population">
      <value value="150"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
1
@#$#@#$#@
