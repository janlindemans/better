% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gbd_clean.R
\name{d_to_OR}
\alias{d_to_OR}
\alias{behavior_OR_to_pct}
\alias{disease}
\alias{behavior_pct_to_disease}
\title{Given a behavior effect, get the disease effect}
\usage{
d_to_OR(effect)

behavior_OR_to_pct(effect, ..., data)

disease(
  effect = nudge(nudge = "all", behavior = "health"),
  ...,
  method = "Hasselblad and Hedges"
)

behavior_pct_to_disease(effect, ..., data)
}
\arguments{
\item{effect}{The behavior effect to be translated into a disease effect, class \code{better_effect}. Either Cohen's d or percentage decrease in exposure. By default \code{nudge(nudge = "all", behavior = "health")}.}

\item{...}{Other arguments for \code{gbd_filter}.}

\item{data}{Path of the GBD folder, where the data are stored. By default, automatically read from options, namely, \code{better.gbd_path}, ideally set in .Rprofile. To learn more, call \code{better_vignette(better)}.}

\item{method}{Method to convert Cohen's d into percentage change. By default, \code{"Hasselblad and Hedges"}. Alternatively, \code{"standard deviations"}. The \code{"Hasselblad and Hedges"} method converts Cohen's d into an Odds Ratio following Hasselblad, V., & Hedges, L. V. (1995). Meta-analysis of screening and diagnostic tests. Psychological Bulletin, 117(1), 167-178. The \code{"standard deviations"} converts Cohen's d into percentage change based on the definition of Cohen's d in terms of standard deviations together with the formula for the standard deviation of proportions.}

\item{cause}{The disease, that is, the cause of mortality and/or disability}

\item{measure}{The measure for mortality/disability}

\item{metric}{The metric (rate, percentage, number) for the measure}
}
\value{
The effect on disease mortality and/or disability, class \code{better_effect}
}
\description{
Given a behavior effect, possibly Cohen's d, get the disease effect
}
\section{Functions}{
\itemize{
\item \code{d_to_OR()}: Convert Cohen's d into odds ratio.

\item \code{behavior_OR_to_pct()}: Convert odds ratio into percentage decrease.

\item \code{behavior_pct_to_disease()}: Given a behavior effect in percentage decrease, get the disease effect

}}
\section{The \code{better_effect} class}{
Check out the full calculation with \code{explanation(.)}.
}

\section{Terms of use}{
A \code{better_effect} is a guesstimate rather than a rigorous impact analysis. Use it with caution.

\code{better} functions that generate \code{better_effect}s make use of data owned by others. The authors and owners of these data retain all rights to their data. Also, they were not involved in the creation of \code{better}, and bear no responsibility for any possible issues with the representation and/or use of the data.
}

\examples{
\dontshow{
NULL
library(tidyverse)
}
nudge_to_behavior_d(nudge = "information", behavior = "health") \%>\%
  d_to_OR
\dontshow{
NULL
library(tidyverse)
}
nudge_to_behavior_d(nudge = "information", behavior = "health") \%>\%
  d_to_OR \%>\%
  behavior_OR_to_pct
\dontshow{
library(tidyverse)
NULL
}
nudge("information", behavior = "food") \%>\%
  disease("diabetes mellitus")
\dontshow{
NULL
library(tidyverse)
}
nudge_to_behavior_d(nudge = "information", behavior = "health") \%>\%
  behavior_d_to_pct \%>\%
  behavior_pct_to_disease(cause = "diabetes mellitus")
}
