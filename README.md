# temPAWN

`temPAWN` calculates the PAWN sensitivity index for each time step of simulation results that were generated with the `run_swat*()` functions from the `SWATplusR` package. The PAWN method is implemented according to Pianosi and Wagener (2018). `temPAWN` optionally includes the computation of the temporal sensitivity of a dummy parameter. Additionally, `temPAWN` offers a plot function to visualize the sensitivity analysis results.

## Installation

You can install the current version of `temPAWN` from the default branch of the package's GitHub repository:

```r
# If you do not have the package devtools installed
install.packages("devtools")

devtools::install_github("chrisschuerz/temPAWN")
```

## Minimum example

Although "minimum", the example requires quite some computation time. For demonstration, the demo SWAT+ model available from the `SWATdata` package will be used. In case you do not have `SWATplusR` and `SWATdata` installed yet on your computer, you can install both packages via:

```r
devtools::install_github("chrisschuerz/SWATplusR")

devtools::install_github("chrisschuerz/SWATdata")
```
