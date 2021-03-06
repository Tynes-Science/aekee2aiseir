% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis.R
\name{regressMomentumPrevalence}
\alias{regressMomentumPrevalence}
\title{Run regression to asses momentum prevalence}
\usage{
regressMomentumPrevalence(
  computed_returns = computeReturns(daily_price_data = importAllYahooFinanceFiles()),
  window_width_months = 12L,
  min_observations_num = 180L
)
}
\description{
Run regression to asses momentum prevalence
}
