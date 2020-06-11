# COVID-19 Estimator

This is an interface that helps countries estimate the rate of transmission of COVID-19 using the number of reported cases on specific dates according to the R package “EpiEstim” (Cori et al., 2019, 2013; R Core Team, 2019).

This interface dynamically produces the following results:

1.  Epidemic curves (number of incidents) as a function of time $t$
2.  Estimated $R$ (Reproductive number) as a function of time $t$ with 95% confidence intervals. This is calculated using sliding weekly windows, with a parametric serial interval based on a mean of $\mu_{si} = 4.8$ and standard deviation $\sigma_{si} = 2.3$

COVID-19 Estimator is available for all countries to use. It is part of the Pan-American Health Organization (PAHO) and World Health Organization's (WHO) efforts to help countries successfully monitor transmission rates and prescribe public policies addressing the COVID-19 epidemic.

### References:

Cori, A., Ferguson, N.M., Fraser, C., Cauchemez, S., 2019. Package ‘EpiEstim.’ https://doi.org/10.1093/aje/kwt133

Cori, A., Ferguson, N.M., Fraser, C., Cauchemez, S., 2013. A New Framework and Software to Estimate Time-Varying Reproduction Numbers During Epidemics. Am. J. Epidemiol. 178, 1505–1512. https://doi.org/10.1093/aje/kwt133

R Core Team, 2019. R: A language and environment for statistical computing. R Foundation for Statistical Computing,.

### Getting Started

To begin, simply click `Browse...` and upload a CSV file (comma-seperated values) in the sidebar panel on the left. 

Note that the CSV must contain dates in the first column and number of incidents in the second column. Note that dates must be written in the order of `Day/Month/Year`. A sample CSV in a correct format can been downloaded below.
