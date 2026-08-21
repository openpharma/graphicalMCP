# Compute the correlation matrix for group sequential test statistics

In a group sequential design, test statistics \\Z_1, \ldots, Z_K\\ at
analyses with information fractions \\t_1, \ldots, t_K\\ follow the
canonical joint distribution with correlation \$\$\text{Cor}(Z_i, Z_j) =
\sqrt{t_i / t_j}, \quad i \le j.\$\$

This correlation structure arises from the independent increments
property of the score process. It depends only on the information
fractions, not on the specific test or endpoint.

## Usage

``` r
gs_corr(info_frac)
```

## Arguments

- info_frac:

  A numeric vector of information fractions at each analysis. Must be
  positive and monotonically non-decreasing.

## Value

A symmetric correlation matrix of dimension \\K \times K\\, where \\K\\
is the length of `info_frac`. The diagonal entries are all 1.

## See also

[`gs_boundaries()`](https://openpharma.github.io/graphicalMCP/reference/gs_boundaries.md)
which uses this correlation matrix for computing group sequential
boundaries.

## Examples

``` r
# Three equally spaced analyses
gs_corr(c(1 / 3, 2 / 3, 1))
#>           [,1]      [,2]      [,3]
#> [1,] 1.0000000 0.7071068 0.5773503
#> [2,] 0.7071068 1.0000000 0.8164966
#> [3,] 0.5773503 0.8164966 1.0000000

# Two analyses at 50% and 100%
gs_corr(c(0.5, 1))
#>           [,1]      [,2]
#> [1,] 1.0000000 0.7071068
#> [2,] 0.7071068 1.0000000
```
