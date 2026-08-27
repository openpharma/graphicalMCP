# S3 print method for the class `power_report`

A printed `power_report` displays the initial graph, testing and
simulation options, power outputs, and optional detailed simulations and
test results.

## Usage

``` r
# S3 method for class 'power_report'
print(x, ..., precision = 4, indent = 2, rows = 10)
```

## Arguments

- x:

  An object of the class `power_report` to print

- ...:

  Other values passed on to other methods (currently unused)

- precision:

  An integer scalar indicating the number of decimal places to to
  display.

- indent:

  An integer scalar indicating how many spaces to indent results.

- rows:

  An integer scalar indicating how many rows of detailed test results to
  print.

## Value

An object x of the class `power_report`, after printing the report of
conducting power simulations based on a graphical multiple comparison
procedure.

## References

Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
Rohmeyer, K. (2011a). Graphical approaches for multiple comparison
procedures using weighted Bonferroni, Simes, or parametric tests.
*Biometrical Journal*, 53(6), 894-913.

Bretz, F., Maurer, W., and Hommel, G. (2011b). Test and power
considerations for multiple endpoint analyses using sequentially
rejective graphical procedures. *Statistics in Medicine*, 30(13),
1489-1501.

## Examples

``` r
# A graphical multiple comparison procedure with two primary hypotheses (H1
# and H2) and two secondary hypotheses (H3 and H4)
# See Figure 4 in Bretz et al. (2011).
alpha <- 0.025
hypotheses <- c(0.5, 0.5, 0, 0)
delta <- 0.5
transitions <- rbind(
  c(0, delta, 1 - delta, 0),
  c(delta, 0, 0, 1 - delta),
  c(0, 1, 0, 0),
  c(1, 0, 0, 0)
)
g <- graph_create(hypotheses, transitions)

marginal_power <- c(0.8, 0.8, 0.7, 0.9)
corr1 <- matrix(0.5, nrow = 2, ncol = 2)
diag(corr1) <- 1
corr <- rbind(
  cbind(corr1, 0.5 * corr1),
  cbind(0.5 * corr1, corr1)
)
success_fns <- list(
  # Probability to reject both H1 and H2
  `H1andH2` = function(x) x[1] & x[2],
  # Probability to reject both (H1 and H3) or (H2 and H4)
  `(H1andH3)or(H2andH4)` = function(x) (x[1] & x[3]) | (x[2] & x[4])
)
set.seed(1234)
# Bonferroni tests
# Reduce the number of simulations to save time for package compilation
power_output <- graph_calculate_power(
  g,
  alpha,
  sim_corr = corr,
  sim_n = 1e2,
  power_marginal = marginal_power,
  sim_success = success_fns
)
```
