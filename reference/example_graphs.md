# Example graphs of commonly used multiple comparison procedures

Built-in functions to quickly generate select graphical multiple
comparison procedures.

## Usage

``` r
bonferroni(num_hyps, hyp_names = NULL)

bonferroni_weighted(hypotheses, hyp_names = NULL)

bonferroni_holm(num_hyps, hyp_names = NULL)

bonferroni_holm_weighted(hypotheses, hyp_names = NULL)

dunnett_single_step(num_hyps, hyp_names = NULL)

dunnett_single_step_weighted(hypotheses, hyp_names = NULL)

dunnett_closure_weighted(hypotheses, hyp_names = NULL)

hochberg(num_hyps, hyp_names = NULL)

hommel(num_hyps, hyp_names = NULL)

huque_etal(hyp_names = NULL)

fallback(hypotheses, hyp_names = NULL)

fallback_improved_1(hypotheses, hyp_names = NULL)

fallback_improved_2(hypotheses, epsilon = 1e-04, hyp_names = NULL)

fixed_sequence(num_hyps, hyp_names = NULL)

sidak(num_hyps, hyp_names = NULL)

simple_successive_1(hyp_names = NULL)

simple_successive_2(hyp_names = NULL)

two_doses_two_primary_two_secondary(hyp_names = NULL)

three_doses_two_primary_two_secondary(hyp_names = NULL)

random_graph(num_hyps, hyp_names = NULL)
```

## Arguments

- num_hyps:

  (Optional) Number of hypotheses in a graphical multiple comparison
  procedure.

- hyp_names:

  (Optional) A character vector of hypothesis names. The length should
  match `num_hyps` and the length of `hypotheses`. If `hyp_names` are
  not specified, hypotheses will be named sequentially as H1, H2,
  .......

- hypotheses:

  (Optional) A numeric vector of hypothesis weights in a graphical
  multiple comparison procedure. Must be a vector of values between 0 &
  1 (inclusive). The length should match `num_hyps` and the length of
  `hyp_names`. The sum of hypothesis weights should not exceed 1.

- epsilon:

  (Optional) A numeric scalar indicating the value of the \\\epsilon\\
  edge. This should be a much smaller value than hypothesis and
  transition weights. The default is 1e-4.

## Value

An S3 object as returned by
[`graph_create()`](https://openpharma.github.io/graphicalMCP/reference/graph_create.md).

## References

Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical
approach to sequentially rejective multiple test procedures. *Statistics
in Medicine*, 28(4), 586-604.

Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W., and
Rohmeyer, K. (2011). Graphical approaches for multiple comparison
procedures using weighted Bonferroni, Simes, or parametric tests.
*Biometrical Journal*, 53(6), 894-913.

Hochberg, Y. (1988). A sharper Bonferroni procedure for multiple tests
of significance. *Biometrika*, 75(4), 800-802.

Hommel, G. (1988). A stagewise rejective multiple test procedure based
on a modified Bonferroni test. *Biometrika*, 75(2), 383-386.

Huque, M. F., Alosh, M., and Bhore, R. (2011). Addressing multiplicity
issues of a composite endpoint and its components in clinical trials.
*Journal of Biopharmaceutical Statistics*, 21(4), 610-634.

Maurer, W., Hothorn, L., and Lehmacher, W. (1995). Multiple comparisons
in drug clinical trials and preclinical assays: a-priori ordered
hypotheses. *Biometrie in der chemisch-pharmazeutischen Industrie*, 6,
3-18.

Šidák, Z. (1967). Rectangular confidence regions for the means of
multivariate normal distributions. *Journal of the American Statistical
Association*, 62(318), 626-633.

Westfall, P. H., and Krishen, A. (2001). Optimally weighted, fixed
sequence and gatekeeper multiple testing procedures. *Journal of
Statistical Planning and Inference*, 99(1), 25-40.

Wiens, B. L. (2003). A fixed sequence Bonferroni procedure for testing
multiple endpoints. *Pharmaceutical Statistics*, 2(3), 211-215.

Wiens, B. L., and Dmitrienko, A. (2005). The fallback procedure for
evaluating a single family of hypotheses. *Journal of Biopharmaceutical
Statistics*, 15(6), 929-942.

Xi, D., and Bretz, F. (2019). Symmetric graphs for equally weighted
tests, with application to the Hochberg procedure. *Statistics in
Medicine*, 38(27), 5268-5282.

## See also

[`graph_create()`](https://openpharma.github.io/graphicalMCP/reference/graph_create.md)
for a general way to create the initial graph.

## Examples

``` r
# Bretz et al. (2009)
bonferroni(num_hyps = 3)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.3333
#> H2: 0.3333
#> H3: 0.3333
#> 
#> --- Transition weights ---
#>     H1 H2 H3
#>  H1  0  0  0
#>  H2  0  0  0
#>  H3  0  0  0
# Bretz et al. (2009)
hypotheses <- c(0.5, 0.3, 0.2)
bonferroni_weighted(hypotheses)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2: 0.3
#> H3: 0.2
#> 
#> --- Transition weights ---
#>     H1 H2 H3
#>  H1  0  0  0
#>  H2  0  0  0
#>  H3  0  0  0
# Bretz et al. (2009)
bonferroni_holm(num_hyps = 3)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.3333
#> H2: 0.3333
#> H3: 0.3333
#> 
#> --- Transition weights ---
#>      H1  H2  H3
#>  H1 0.0 0.5 0.5
#>  H2 0.5 0.0 0.5
#>  H3 0.5 0.5 0.0
# Bretz et al. (2009)
hypotheses <- c(0.5, 0.3, 0.2)
bonferroni_holm_weighted(hypotheses)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2: 0.3
#> H3: 0.2
#> 
#> --- Transition weights ---
#>      H1  H2  H3
#>  H1 0.0 0.5 0.5
#>  H2 0.5 0.0 0.5
#>  H3 0.5 0.5 0.0
# Xi et al. (2017)
dunnett_single_step(num_hyps = 3)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.3333
#> H2: 0.3333
#> H3: 0.3333
#> 
#> --- Transition weights ---
#>     H1 H2 H3
#>  H1  0  0  0
#>  H2  0  0  0
#>  H3  0  0  0
# Xi et al. (2017)
hypotheses <- c(0.5, 0.3, 0.2)
dunnett_single_step_weighted(hypotheses)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2: 0.3
#> H3: 0.2
#> 
#> --- Transition weights ---
#>     H1 H2 H3
#>  H1  0  0  0
#>  H2  0  0  0
#>  H3  0  0  0
# Xi et al. (2009)
hypotheses <- c(0.5, 0.3, 0.2)
dunnett_closure_weighted(hypotheses)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2: 0.3
#> H3: 0.2
#> 
#> --- Transition weights ---
#>      H1  H2  H3
#>  H1 0.0 0.5 0.5
#>  H2 0.5 0.0 0.5
#>  H3 0.5 0.5 0.0
# Hochberg (1988)
hochberg(num_hyps = 3)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.3333
#> H2: 0.3333
#> H3: 0.3333
#> 
#> --- Transition weights ---
#>      H1  H2  H3
#>  H1 0.0 0.5 0.5
#>  H2 0.5 0.0 0.5
#>  H3 0.5 0.5 0.0
# Hommel (1988)
hommel(num_hyps = 3)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.3333
#> H2: 0.3333
#> H3: 0.3333
#> 
#> --- Transition weights ---
#>      H1  H2  H3
#>  H1 0.0 0.5 0.5
#>  H2 0.5 0.0 0.5
#>  H3 0.5 0.5 0.0
# Huque et al. (2011)
huque_etal()
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 1
#> H2: 0
#> H3: 0
#> H4: 0
#> 
#> --- Transition weights ---
#>      H1  H2  H3  H4
#>  H1 0.0 0.5 0.5 0.0
#>  H2 0.0 0.0 0.0 1.0
#>  H3 0.0 0.5 0.0 0.5
#>  H4 0.0 1.0 0.0 0.0
# Wiens (2003)
hypotheses <- c(0.5, 0.3, 0.2)
fallback(hypotheses)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2: 0.3
#> H3: 0.2
#> 
#> --- Transition weights ---
#>     H1 H2 H3
#>  H1  0  1  0
#>  H2  0  0  1
#>  H3  0  0  0
# Wiens and Dmitrienko (2005)
hypotheses <- c(0.5, 0.3, 0.2)
fallback_improved_1(hypotheses)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2: 0.3
#> H3: 0.2
#> 
#> --- Transition weights ---
#>        H1    H2    H3
#>  H1 0.000 1.000 0.000
#>  H2 0.000 0.000 1.000
#>  H3 0.625 0.375 0.000
# Bretz et al. (2009)
hypotheses <- c(0.5, 0.3, 0.2)
fallback_improved_2(hypotheses)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2: 0.3
#> H3: 0.2
#> 
#> --- Transition weights ---
#>         H1     H2     H3
#>  H1 0.0000 1.0000 0.0000
#>  H2 0.9999 0.0000 0.0001
#>  H3 1.0000 0.0000 0.0000
# Maurer et al. (1995); Westfall and Krishen (2001)
fixed_sequence(num_hyps = 3)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 1
#> H2: 0
#> H3: 0
#> 
#> --- Transition weights ---
#>     H1 H2 H3
#>  H1  0  1  0
#>  H2  0  0  1
#>  H3  0  0  0
# sidak (1967)
sidak(num_hyps = 3)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.3333
#> H2: 0.3333
#> H3: 0.3333
#> 
#> --- Transition weights ---
#>     H1 H2 H3
#>  H1  0  0  0
#>  H2  0  0  0
#>  H3  0  0  0
# Figure 1 in Bretz et al. (2011)
simple_successive_1()
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2: 0.5
#> H3: 0.0
#> H4: 0.0
#> 
#> --- Transition weights ---
#>     H1 H2 H3 H4
#>  H1  0  0  1  0
#>  H2  0  0  0  1
#>  H3  0  1  0  0
#>  H4  1  0  0  0
# Figure 4 in Bretz et al. (2011)
simple_successive_2()
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2: 0.5
#> H3: 0.0
#> H4: 0.0
#> 
#> --- Transition weights ---
#>      H1  H2  H3  H4
#>  H1 0.0 0.5 0.5 0.0
#>  H2 0.5 0.0 0.0 0.5
#>  H3 0.0 1.0 0.0 0.0
#>  H4 1.0 0.0 0.0 0.0
# Figure 6 in Xi and Bretz et al. (2019)
two_doses_two_primary_two_secondary()
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.5
#> H2: 0.0
#> H3: 0.0
#> H4: 0.5
#> H5: 0.0
#> H6: 0.0
#> 
#> --- Transition weights ---
#>         H1     H2     H3     H4     H5     H6
#>  H1 0.0000 0.5000 0.5000 0.0000 0.0000 0.0000
#>  H2 0.0000 0.0000 1.0000 0.0000 0.0000 0.0000
#>  H3 0.0000 0.9999 0.0000 0.0001 0.0000 0.0000
#>  H4 0.0000 0.0000 0.0000 0.0000 0.5000 0.5000
#>  H5 0.0000 0.0000 0.0000 0.0000 0.0000 1.0000
#>  H6 0.0001 0.0000 0.0000 0.0000 0.9999 0.0000
# Add another dose to Figure 6 in Xi and Bretz et al. (2019)
three_doses_two_primary_two_secondary()
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.3333
#> H2: 0.0000
#> H3: 0.0000
#> H4: 0.3333
#> H5: 0.0000
#> H6: 0.0000
#> H7: 0.3333
#> H8: 0.0000
#> H9: 0.0000
#> 
#> --- Transition weights ---
#>          H1      H2      H3      H4      H5      H6      H7      H8      H9
#>  H1 0.00000 0.50000 0.50000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#>  H2 0.00000 0.00000 1.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000
#>  H3 0.00000 0.99990 0.00000 0.00005 0.00000 0.00000 0.00005 0.00000 0.00000
#>  H4 0.00000 0.00000 0.00000 0.00000 0.50000 0.50000 0.00000 0.00000 0.00000
#>  H5 0.00000 0.00000 0.00000 0.00000 0.00000 1.00000 0.00000 0.00000 0.00000
#>  H6 0.00005 0.00000 0.00000 0.00000 0.99990 0.00000 0.00005 0.00000 0.00000
#>  H7 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.50000 0.50000
#>  H8 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 0.00000 1.00000
#>  H9 0.00005 0.00000 0.00000 0.00005 0.00000 0.00000 0.00000 0.99990 0.00000
# Create a random graph with three hypotheses
random_graph(num_hyps = 3)
#> Initial graph
#> 
#> --- Hypothesis weights ---
#> H1: 0.1667
#> H2: 0.5000
#> H3: 0.3333
#> 
#> --- Transition weights ---
#>         H1     H2     H3
#>  H1 0.0000 0.6667 0.3333
#>  H2 0.6667 0.0000 0.3333
#>  H3 0.7500 0.2500 0.0000
```
