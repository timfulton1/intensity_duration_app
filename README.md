# Critical Power and Critical Speed Analysis Application

This repo contains the code used to build the analysis application.

Visit the app here:


image

### Background

I developed this application to make it easy for cyclists to estimate their critical power and runners to estimate their critical speed.

### Methods

##### Cycling
The data are fit using a two parameter hyperbolic model according to the equation below:

$$
t = D' / P - CP
$$

where `t` is the performance time (s), `W'` is the curvature constant (Joules), `P`is the performance power (W), and `CP` is the Critical Power (W). 

Three additional variables that are calculated are:

1. **Response Time** - The sum of the time delay and the tau.
2. **Overshoot** - The difference between the amplitude and the average HHb over the final 15 seconds of exercise.
3. **RMSE** - The root mean squared error of the fit.

The timespan of the data used will range from the first fitting point to 60 seconds.

### Usage

Upload data using the browse button (visitors can use the demo data). The first fitting point defaults to 6 seconds, but it should be adjusted to the first data point after time zero (exercise start) that is higher than the baseline. The first fitting point can be adjusted using the slider on the right.
