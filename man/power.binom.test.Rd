% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/power.binom.test.R
\name{power.binom.test}
\alias{power.binom.test}
\title{Calculates the power for one-sample exact binomial test given null and alternative probabilities}
\usage{
power.binom.test(
  n,
  p0,
  pA,
  alpha = 0.05,
  alternative = c("two.sided", "less", "greater")
)
}
\arguments{
\item{n}{number of trials}

\item{p0}{null probability of success}

\item{pA}{probability of success in alternative group}

\item{alpha}{false positive rate or 1-confidence level}

\item{alternative}{indicates the alternative hypothesis and must be one of "two.sided", "greater" or "less". You can specify just the initial letter.}
}
\value{

}
\description{
Calculates the power for one-sample exact binomial test given null and alternative probabilities
}
\examples{

power.binom.test(10, 0.5, 0.7)
power.binom.test(10, 0.5, 0.25, alternative = "less")
power.binom.test(15, 0.5, 0.7, alpha = 0.1, alternative = "greater")

}
