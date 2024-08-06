
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <u>interactiveeafplots</u>: Visualisations of the Empirical Attainment Function

<!-- badges: start -->

\[ [**GitHub**](https://github.com/YasiinsG) \] \[
[**LinkedIn**](https://www.linkedin.com/in/yasinsg/) \]

**Author:** [Yasin Gija](https://www.linkedin.com/in/yasinsg/)
<!-- badges: end -->

The Empirical Attainment Function characterizes the probability that a
given point in the objective space is dominated by the solutions
generated by a multi-objective optimization algorithm after a certain
number of evaluations or generations. Essentially, it provides a
probabilistic description of the outcomes produced by the algorithm over
multiple runs.

Here we seek to display visually the Empirical Attainment Function using
Plotly and ggplot2 in R. This package plots the Empirical Attainment
Function (EAF), the EAF differences, the symmetric deviation function
and 3d plots of the EAF.

## Installation

You can install the development version of interactiveeafplots like so:

``` r
install.packages("devtools")
devtools::install_github("YasiinsG/interactiveeafplots")
```

## Usage

Once the interactiveeafplots package is installed, the following R
commands will give more information:

``` r
library(interactiveeafplots)
#> Warning: replacing previous import 'ggplot2::last_plot' by 'plotly::last_plot'
#> when loading 'interactiveeafplots'
?interactiveeafplot
?interactiveeafdiffplot
?interactivesymdevplot
?interactive3dplot
```
