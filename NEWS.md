# graphicalMCP 0.1.0

* First release

# graphicalMCP 0.1.1

* Added compilation of vignettes (#73)
* Removed duplicated columns of "*" in test values (#75)

# graphicalMCP 0.1.2

* Updated function documentation
* Updated references

# graphicalMCP 0.2.0

* Corrected typos
* Updated function documentation for CRAN release

# graphicalMCP 0.2.1

* Corrected typos
* First CRAN release

# graphicalMCP 0.2.2

* Updated citations
* Resubmission for first CRAN release

# graphicalMCP 0.2.3

* Included cran-comments.ms in .Rbuildignore
* Resubmission for first CRAN release

# graphicalMCP 0.2.4

* Updated documentation according to issue #84
* Resubmission for first CRAN release

# graphicalMCP 0.2.5

* Updated adjust_weights_parametric_util.Rd
* Resubmission for first CRAN release

# graphicalMCP 0.2.6

* Github repo transferred to openpharma
* Submission for CRAN release

# graphicalMCP 0.2.7

* Added Hochberg-based procedures
* Added internal validations
* Expanded example graphs
* Updated vignettes
* Submission for CRAN release

# graphicalMCP 0.2.8

* Corrected urls for references
* Submission for CRAN release

# graphicalMCP 0.2.9

* Closed Issue #90 by changing the precision for parametric tests
* Submission for CRAN release

# graphicalMCP 0.3.0

* Added group sequential testing for graphical MCPs via `graph_test_shortcut_gsd()`, which extends `graph_test_shortcut()` to multiple analyses with per-hypothesis spending functions, information fractions, and a `look_back` option to use repeated or sequential p-values
* Added spending functions `spending_of()`, `spending_pocock()`, `spending_hsd()`, `spending_linear()`, `spending_wt()` (Wang-Tsiatis), and `spending_with_time()` for a custom spending time
* Added `gs_boundaries()`, `gs_corr()`, `repeated_p()`, and `sequential_p()`
* Added vignettes on group sequential testing and on its validation against gsDesign and rpact
* Renamed the `Adj. P-value` column to `Adj.p` in `print.graph_report()`
* Added a warning in `graph_create()` for very small transition weights (Issue #97)
* Plotting examples, vignettes, and tests are skipped when `igraph` or `gMCP` is not installed

# graphicalMCP (development version)
