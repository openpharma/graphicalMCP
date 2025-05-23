% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_calculate_power.R
\name{graph_calculate_power}
\alias{graph_calculate_power}
\title{Calculate power values for a graphical multiple comparison procedure}
\usage{
graph_calculate_power(
  graph,
  alpha = 0.025,
  power_marginal = rep(alpha, length(graph$hypotheses)),
  test_groups = list(seq_along(graph$hypotheses)),
  test_types = c("bonferroni"),
  test_corr = rep(list(NA), length(test_types)),
  sim_n = 1e+05,
  sim_corr = diag(length(graph$hypotheses)),
  sim_success = NULL,
  verbose = FALSE
)
}
\arguments{
\item{graph}{An initial graph as returned by \code{\link[=graph_create]{graph_create()}}.}

\item{alpha}{A numeric value of the one-sided overall significance level,
which should be between 0 & 1. The default is 0.025 for one-sided
hypothesis testing. Note that only one-sided tests are supported.}

\item{power_marginal}{A numeric vector of marginal power values to use when
simulating p-values. See Details for more on the simulation process.}

\item{test_groups}{A list of numeric vectors specifying hypotheses to test
together. Grouping is needed to correctly perform Simes and parametric
tests.}

\item{test_types}{A character vector of test types to apply to each test
group. This is needed to correctly perform Simes and parametric
tests. The length should match the number of elements in \code{test_groups}.}

\item{test_corr}{(Optional) A list of numeric correlation matrices. Each
entry in the list should correspond to each test group. For a test group
using Bonferroni or Simes tests, its corresponding entry in \code{test_corr}
should be \code{NA}. For a test group using parametric tests, its
corresponding entry in \code{test_corr} should be a numeric correlation matrix
specifying the correlation between test statistics for hypotheses in this
test group. The length should match the number of elements in
\code{test_groups}.}

\item{sim_n}{An integer scalar specifying the number of simulations. The
default is 1e5.}

\item{sim_corr}{A numeric matrix of correlations between test statistics for
all hypotheses. The dimensions should match the number of hypotheses in
\code{graph}.}

\item{sim_success}{A list of user-defined functions to specify the success
criteria. Functions must take one simulation's logical vector of results as
an input, and return a length-one logical vector. For instance, if
"success" means rejecting hypotheses 1 and 2, use \code{sim_success = list("1 and 2" = function(x) x[1] && x[2])}. If the list is not named, the function
body will be used as the name. Lambda functions also work starting with R
4.1, e.g. \verb{sim_success = list(\\(x) x[3] || x[4])}.}

\item{verbose}{A logical scalar specifying whether the details of power
simulations should be included in results. The default is \code{verbose = FALSE}.}
}
\value{
A \code{power_report} object with a list of 3 elements:
\itemize{
\item \code{inputs} - Input parameters, which is a list of:
\itemize{
\item \code{graph} - Initial graph,
\item \code{alpha} - Overall significance level,
\item \code{test_groups} - Groups of hypotheses for different types of tests,
\item \code{test_types} - Different types of tests,
\item \code{test_corr} - Correlation matrices for parametric tests,
\item \code{sim_n} - Number of simulations,
\item \code{power_marginal} - Marginal power of all hypotheses
\item \code{sim_corr} - Correlation matrices for simulations,
\item \code{sim_success} - User-defined success criteria.
}
\item \code{power} - A list of power values
\itemize{
\item \code{power_local} - Local power of all hypotheses, which is the proportion
of simulations in which each hypothesis is rejected,
\item \code{rejection_expected} - Expected (average) number of rejected hypotheses,
\item \code{power_at_least_1} - Power to reject at least one hypothesis,
\item \code{power_all} - Power to reject all hypotheses,
\item \code{power_success} - Power of user-defined success, which is the
proportion of simulations in which the user-defined success criterion
\item \code{sim_success} is met.
}
\item \code{details} - An optional list of datasets showing simulated p-values and
results for each simulation.
}
}
\description{
Under the alternative hypotheses, the distribution of test statistics is
assumed to be a multivariate normal distribution. Given this distribution,
this function calculates power values for a graphical multiple comparison
procedure. By default, it calculate the local power, which is the probability
to reject an individual hypothesis, the probability to reject at least one
hypothesis, the probability to reject all hypotheses, the expected number of
rejections, and the probability of user-defined success criteria.
See \code{vignette("shortcut-testing")} and \code{vignette("closed-testing")} for more
illustration of power calculation.
}
\section{Simulation details}{
 The power calculation is based on simulations.
The distribution to simulate from is determined as a multivariate normal
distribution by \code{power_marginal} and \code{sim_corr}. In particular,
\code{power_marginal} is a vector of marginal power values for all hypotheses.
The marginal power is the power to reject the null hypothesis at the
significance level \code{alpha} \emph{without multiplicity adjustment}. This value could be readily available from
standard software and other R packages. Then we can determine the mean of
the multivariate normal distribution as
\deqn{\Phi^{-1}\left(1-\alpha\right)-\Phi^{-1}\left(1-d_i\right)}, which is
often called the non-centrality parameter or the drift parameter. Here
\eqn{d_i} is the marginal power \code{power_marginal} of hypothesis \eqn{i}.
Given the correlation matrix \code{sim_corr}, we can simulate from this
multivariate normal distribution using the \code{mvtnorm} R package (Genz and
Bretz, 2009).

Each set simulated values can be used to calculate the corresponding
one-sided p-values. Then this set of p-values are plugged into the
graphical multiple comparison procedure to determine which hypotheses are
rejected. This process is repeated \code{n_sim} times to produce the power
values as the proportion of simulations in which a particular success
criterion is met.
}

\examples{
# A graphical multiple comparison procedure with two primary hypotheses (H1
# and H2) and two secondary hypotheses (H3 and H4)
# See Figure 4 in Bretz et al. (2011a).
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

# Parametric tests for H1 and H2; Simes tests for H3 and H4
# User-defined success: to reject H1 or H2; to reject H1 and H2
# Reduce the number of simulations to save time for package compilation
graph_calculate_power(
  g,
  alpha,
  test_groups = list(1:2, 3:4),
  test_types = c("parametric", "simes"),
  test_corr = list(corr1, NA),
  sim_n = 1e2,
  sim_success = list(
    function(.) .[1] || .[2],
    function(.) .[1] && .[2]
  )
)

}
\references{
Bretz, F., Posch, M., Glimm, E., Klinglmueller, F., Maurer, W.,
and Rohmeyer, K. (2011a). Graphical approaches for multiple comparison
procedures using weighted Bonferroni, Simes, or parametric tests.
\emph{Biometrical Journal}, 53(6), 894-913.

Bretz, F., Maurer, W., and Hommel, G. (2011b). Test and power
considerations for multiple endpoint analyses using sequentially rejective
graphical procedures. \emph{Statistics in Medicine}, 30(13), 1489-1501.

Genz, A., and Bretz, F. (2009). \emph{Computation of Multivariate Normal
and t Probabilities}, series Lecture Notes in Statistics. Springer-Verlag,
Heidelberg.

Lu, K. (2016). Graphical approaches using a Bonferroni mixture of weighted
Simes tests. \emph{Statistics in Medicine}, 35(22), 4041-4055.

Xi, D., Glimm, E., Maurer, W., and Bretz, F. (2017). A unified framework
for weighted parametric multiple test procedures. \emph{Biometrical
Journal}, 59(5), 918-931.
}
