# Critical Power and Critical Speed Analysis Application

This repo contains the code used to build the analysis application.

Visit the app here:


image

## Background

I developed this application to make it easy for cyclists to estimate their critical power (CP) and runners to estimate their critical speed (CS).

The sustainable duration of exercise is dependent upon the intensity (i.e., power or speed) at which the exercise is performed. This fundamental concept is known as the intensity-duration relationship, and for exercise durations of ~2–40 minutes, can be modeled by a two-parameter hyperbolic equation (see methods below). Critical power (or speed) is the highest power that can be sustained primarily by aerobic metabolism while still achieving a metabolic steady state. Critical power (or speed) is also a metabolic threshold such that exercising at powers (or speeds) above CP will cause increased reliance on anaerobic metabolism and a greater accumulation of fatigue-inducing metabolites. For a given power above CP, the time until task failure is dependent on the magnitude of W', suggesting that W' can be viewed as a fatigue buffer (or fatigue constant). 


## Methods

### Cycling
The data are fit using a two parameter hyperbolic model according to the equation below:

$$
t = \frac{D'}{P - CP} \
$$

where `t` is the performance time (s), `W'` is the curvature constant (Joules), `P`is the performance power (W), and `CP` is the Critical Power (W). 

Two additional variables that are calculated in the application are:

1. **5 minute Power** - An estimate of the highest power that can be sustained for 5 minutes.
2. **20 minute Power** - An estimate of the highest power that can be sustained for 20 minutes.


### Running
The data are fit using a two parameter hyperbolic model according to the equation below:

$$
t = \frac{D'}{S - CS} \
$$

where `t` is the performance time (s), `D'` is the curvature constant (m), `S` is the average speed (m/s), and `CS` is the critical speed (m/s).

Two additional variables that are calculated in the application are:

1. **Mile** - Estimated one mile performance.
2. **5,000m** - Estimated 5K performance.


## Usage

- Enter power and duration data for at least 3 performances.
- Each performance can be either a time to exhaustion test at a constant power or a fixed distance time trial (e.g., 4 km). If using a fixed distance time trial, power may vary throughout the effort, so use the average power over the entire distance.
- For the most accurate estimation of Critical Power and W Prime, choose performances lasting between ~3 and ~20 minutes, spread across this range. For example, durations of 5, 10, and 15 minutes would provide a better estimation than durations of 4, 6, and 8 minutes.
