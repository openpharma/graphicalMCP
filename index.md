# graphicalMCP

# Introduction

Graphical approaches for multiple comparison procedures (MCPs) are a
general framework to control the family-wise error rate strongly at a
pre-specified significance level $`0<\alpha<1`$. This approach includes
many commonly used MCPs as special cases and is transparent in
visualizing MCPs for better communications. `graphicalMCP` is designed
to design and analyze graphical MCPs in a flexible, informative and
efficient way.

# Installation

### Release

You can install the current release version from *CRAN* with:

``` r

install.packages("graphicalMCP")
```

### Development

You can install the current development version from *GitHub* with:

``` r

# install.packages("pak")
pak::pak("openpharma/graphicalMCP")
```

# Documentation

- For basic usage instructions, see
  [`vignette("graphicalMCP")`](https://openpharma.github.io/graphicalMCP/articles/graphicalMCP.md)
- To become familiar with graphical MCP terminologies, see
  [`vignette("glossary")`](https://openpharma.github.io/graphicalMCP/articles/glossary.md)
- To learn examples of how to use `graphicalMCP`,
  - see
    [`vignette("shortcut-testing")`](https://openpharma.github.io/graphicalMCP/articles/shortcut-testing.md)
    for sequentially rejective graphical multiple comparison procedures
    based on Bonferroni tests
  - see
    [`vignette("closed-testing")`](https://openpharma.github.io/graphicalMCP/articles/closed-testing.md)
    for graphical multiple comparison procedures based on the closure
    principle using Bonferroni, Hochberg, parametric and Simes tests
  - see
    [`vignette("graph-examples")`](https://openpharma.github.io/graphicalMCP/articles/graph-examples.md)
    for common multiple comparison procedures illustrated using
    `graphicalMCP`
  - see
    [`vignette("internal-validation")`](https://openpharma.github.io/graphicalMCP/articles/internal-validation.md)
    for internal validation via power simulations for methods used in
    `graphicalMCP`
  - see
    [`vignette("generate-closure")`](https://openpharma.github.io/graphicalMCP/articles/generate-closure.md)
    for rationales to generate the closure and the weighting strategy of
    a graph
  - see
    [`vignette("comparisons")`](https://openpharma.github.io/graphicalMCP/articles/comparisons.md)
    for comparisons to other R packages
- To view vignettes in R after properly installing `graphicalMCP`, we
  can build vignettes by `devtools::install(build_vignettes = TRUE)`,
  and then use `browseVignettes("graphicalMCP")` to view the full list
  of vignettes

# Related work

- Graphical MCPs - [gMCP](https://cran.r-project.org/package=gMCP)
- Lighter version of `gMCP` which removes the rJava dependency -
  [gMCPLite](https://cran.r-project.org/package=gMCPLite)
- Graphical MCPs with Simes tests -
  [lrstat](https://cran.r-project.org/package=lrstat)

Built upon these packages, we hope to implement graphical MCPs in a more
general framework, with fewer dependencies and simpler S3 classes, and
without losing computational efficiency.

# Acknowledgments

Along with the authors and contributors, thanks to the following people
for their suggestions and inspirations on the package:

Keaven Anderson, Frank Bretz, Nan Chen, Yao Chen, Spencer Childress,
Chelsea Dickens, Ekkehard Glimm, Willi Maurer, Colleen McLaughlin,
Friedrich Pahlke, Matt Roumaya, Daniel Sabanés Bové, Alex Spiers, Gernot
Wassmer, Jeremy Wildfire, Nan Xiao, Yanyao Yi, Ron Yu, and Ying Zhang

We owe a debt of gratitude to the authors of
[gMCP](https://cran.r-project.org/package=gMCP) for their pioneering
work, without which this package would not be nearly as extensive as it
is.
