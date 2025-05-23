% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/test_values.R
\name{test_values_bonferroni}
\alias{test_values_bonferroni}
\alias{test_values_parametric}
\alias{test_values_simes}
\alias{test_values_hochberg}
\title{Organize outputs for testing an intersection hypothesis}
\usage{
test_values_bonferroni(p, hypotheses, alpha, intersection = NA)

test_values_parametric(p, hypotheses, alpha, intersection = NA, test_corr)

test_values_simes(p, hypotheses, alpha, intersection = NA)

test_values_hochberg(p, hypotheses, alpha, intersection = NA)
}
\arguments{
\item{p}{A numeric vector of p-values (unadjusted, raw), whose values should
be between 0 & 1. The length should match the number of hypotheses in
\code{graph}.}

\item{hypotheses}{A numeric vector of hypothesis weights in a graphical
multiple comparison procedure. Must be a vector of values between 0 & 1
(inclusive). The length should match the row and column lengths of
\code{transitions}. The sum of hypothesis weights should not exceed 1.}

\item{alpha}{A numeric value of the overall significance level, which should
be between 0 & 1. The default is 0.025 for one-sided hypothesis testing
problems; another common choice is 0.05 for two-sided hypothesis testing
problems. Note when parametric tests are used, only one-sided tests are
supported.}

\item{intersection}{(optional) A numeric scalar used to name the
intersection hypothesis in a weighting strategy.}

\item{test_corr}{(Optional) A list of numeric correlation matrices. Each
entry in the list should correspond to each test group. For a test group
using Bonferroni or Simes tests, its corresponding entry in \code{test_corr}
should be \code{NA}. For a test group using parametric tests, its
corresponding entry in \code{test_corr} should be a numeric correlation matrix
specifying the correlation between test statistics for hypotheses in this
test group. The length should match the number of elements in
\code{test_groups}.}
}
\value{
A data frame with rows corresponding to individual hypotheses
involved in the intersection hypothesis with hypothesis weights
\code{hypotheses}. There are following columns:
\itemize{
\item \code{Intersection} - Name of this intersection hypothesis,
\item \code{Hypothesis} - Name of an individual hypothesis,
\item \code{Test} - Test type for an individual hypothesis,
\item \code{p} - (Unadjusted or raw) p-values for a individual hypothesis,
\item \code{c_value}- C value for parametric tests,
\item \code{Weight} - Hypothesis weight for an individual hypothesis,
\item \code{Alpha} - Overall significance level \eqn{\alpha},
\item \code{Inequality_holds} - Indicator to show if the p-value is less than or
equal to its significance level.
\itemize{
\item For Bonferroni and Simes tests, the significance level is the
hypothesis weight times \eqn{\alpha}.
\item For parametric tests, the significance level is the c value times
the hypothesis weight times \eqn{\alpha}.
}
}
}
\description{
An intersection hypothesis can be tested by a mixture of test types including
Bonferroni, parametric and Simes tests. This function organize outputs of
testing and prepare them for \code{graph_report}.
}
\references{
Bretz, F., Maurer, W., Brannath, W., and Posch, M. (2009). A graphical
approach to sequentially rejective multiple test procedures.
\emph{Statistics in Medicine}, 28(4), 586-604.

Lu, K. (2016). Graphical approaches using a Bonferroni mixture of weighted
Simes tests. \emph{Statistics in Medicine}, 35(22), 4041-4055.

Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework
for weighted parametric multiple test procedures.
\emph{Biometrical Journal}, 59(5), 918-931.
}
\keyword{internal}
