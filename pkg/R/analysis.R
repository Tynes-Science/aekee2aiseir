#' Compute logarithmic returns for a given window width.
#' @param daily_price_data (data frame) daily price data with `date`, `symbol`, `close` columns
#' @param window_width (integer) number of periods width of aggregation window
#' @export

computeReturns <- function(
      daily_price_data,
      window_width=1L
) {
  daily_price_data %>%
  arrange(symbol, date) %>%
  mutate(log_close=log(close)) %>%
  group_by(symbol) %>%
  # Ensuring aggregation at `window_width` resolution level:
  filter(((row_number() - 1L) %% window_width) == 0L) %>%
  # Log returns are used, which deviates from the Reference Paper:
  mutate(retn=log_close - lag(log_close, n=1L)) %>%
  # Version corresponding with the Reference Paper (uses arithmetic returns):
  #  mutate(retn=(close - lag(close, n=1L))/ lag(close, n=1L)) %>%
  ungroup
}


#' Interpret linear regression results and output a formatted data frame
#' @param lm_result (lm) result of running `lm`
#' @export

interpretRegression <- function(lm_result) {
  lm_result %>%
  summary %$%
  {
    coefficients[2,] %>% t %>% as_tibble %>%
    mutate(r_squared=r.squared)
  } %T>%
  {
    colnames(.) %<>%
    str_to_lower %>%
    str_replace_all(c(" "="_", "\\."=""))
  } %>%
  rename(prob=`pr(>|t|)`)
}


#' Run linear regressions for different lead values
#' @param reaturns_data (data frame) with periodic returns data
#' @param leads (integer) lead values for which to run analysis
#' @export

runRegressionLeads <- function(returns_data, leads=1:12L) {
  setNames(as.list(leads), leads) %>%
  ldply(
    .id="n",
    .fun=. %>%
      {transmute(returns_data, x=retn, y=lead(retn, n=.))} %>%
      lm(formula=y~x, data=.) %>%
      interpretRegression
  )
}


#' Run all daily regressions per symbol at indicated leads
#' @export

runAllDailyRegressions <- function(returns_data, leads=1:12L) {
  returns_data %>%
  ddply(
    .variables="symbol",
    .fun=. %>% runRegressionLeads(leads=leads)
  ) %>%
  as_tibble
}


#' Generate price plot
#' @param data (data frame) daily price data with `date`, `symbol`, `open`, `high`,
#'   `low`, `close` columns
#' @param smb_list (character vector) list of symbols for which generate plot
#' @param start_date (character) start date in format "%Y-%m-%d" for data in plot
#' @param end_date (character) end date in format "%Y-%m-%d" for data in plot
#' @param type (character) type of price for plot from: c("open","high","low","close")
#' @param brk (character) how often show labels on x-axis
#' @export

priceMovePlot<-function(
  data,
  smb_list,
  start_date="1900-01-01",
  end_date="2900-01-01",
  type=c("open","high","low","close"),
  brk="2 month"
) {
  # Check type parameter
  assert_that(length(type)==1, msg = "No type provided or more than one value")
  type <- match.arg(type)

  # Check type and convert start & end date to Date format
  assert_that(is.string(start_date))
  assert_that(is.string(end_date))
  start_date = as.Date(start_date)
  end_date = as.Date(end_date)

  # Check type and convert start & end date to Date format
  assert_that(is.string(brk))

  data %>%
  # Filter by symbol
  filter(symbol %in% smb_list) %>%
  # Filter by start & end date
  filter(between(date, start_date, end_date)) %>%
  # Set x = date and y = open|high|low|close
  ggplot(., aes(x = date, y = get(type))) +
  geom_line() +
  # Set breaks, tick labels format, axis labels
  scale_x_date(breaks = brk, date_labels = "%b %y") +
  xlab("") +
  ylab(paste(type, "price")) +
  facet_grid(cols = vars(symbol), scales = "free")
}

### N-tile Momentum

ntileMomentum <- function(
      returns_data_w_ntile_ranks,
      leads=1:12L
) {
  returns_data_w_ntile_ranks %>%
  crossing(lead_n=leads) %>%
  group_by(lead_n) %>%
  mutate(lead_retn=lead(retn, n=unique(lead_n))) %>%
  ungroup %>%
  na.omit %>%
  group_by(
    ntile_rank,
    lead_n
  ) %>%
  summarise(
    formation_return=mean(retn),
    lead_return=mean(lead_retn),
    t_statistic=t.test(lead_retn, alternative="two.sided")[["statistic"]],
    p_value=t.test(lead_retn, alternative="two.sided")[["p.value"]],
    std_error=sd(lead_retn),
    sharpe_ratio=mean(lead_retn) / sd(lead_retn),
  ) %>%
  ungroup
}


#' Ntile analysis with no lookahead:w

returnNtilesNoLookAhead <- function(
     returns_data,
     training_date_max="2015-01-01",
     ntile_n=5L,
     leads=1:12L
) {
  # Computing ntile breaks based on historical returns data (training data):
    returns_data %>%
    filter(date <= training_date_max) %$%
    quantile(retn, probs=seq(0, 1, by=1 / ntile_n), na.rm=TRUE) %T>%
    {.[1] <- -Inf} %T>%
    {.[length(.)] <- Inf} ->
  retn_breaks

  # Applying training data breaks to subsequent returns data (testing data):
  returns_data %>%
  filter(date > training_date_max) %>%
  mutate(ntile_rank=cut(x=retn, breaks=retn_breaks, labels=FALSE))
}


returnNtilesTotal <- function(
     returns_data,
     ntiles_n=5L
) {
  dplyr::mutate(
    .data=returns_data,
    ntile_rank=dplyr::ntile(retn, n=ntiles_n)
  )
}


### Persistence

#' Test of whether momentum patterns persist beyond a training period

momentumPersistence <- function(
  dat,
  estimation_months=6L,
  validation_months=3L,
  window_width=1L
) {
  left_join(
    by=c("symbol", "month_beginning", "n"),
    x=
      regressMomentumPrevalence(
        dat,
        window_width_months=estimation_months,
        min_observations_num=estimation_months * 15L / window_width
      ),
    y=
      regressMomentumPrevalence(
        dat,
        window_width_months=validation_months,
        min_observations=validation_months * 15L / window_width
      ) %>%
      group_by(symbol, n) %>%
      mutate_at(c("estimate", "prob"), lag, 9L) %>%
      ungroup %>%
      transmute(symbol, month_beginning, n, positive=estimate > 0)
  ) %>%
  filter(estimate > 0, prob <= 0.05) %>%
  group_by(symbol, n) %>%
  summarise(hit_rate=mean(positive, na.rm=TRUE)) %>%
  ungroup
}


### Inter-Currency Differences

#' Assess momentum pattern differences between cryptocurrencies

interCurrencyDifferences <- function(
      computed_returns=computeReturns(daily_price_data=importAllYahooFinanceFiles()),
      min_observations_num=180L
) {
  computed_returns %>%
  regressMomentumPrevalence(min_observations_num=min_observations_num) %>%
  summariseMomentumPrevalence %>%
  {filter(group_by(., month_beginning), n() == max(count(., month_beginning)$n))} %>%
  group_by(symbol, p_level) %>%
  summarise(mean_occurrence=mean(value)) %>%
  ungroup
}


### Prevalence Over Time

#' Run regression to asses momentum prevalence

regressMomentumPrevalence <- function(
      computed_returns=computeReturns(daily_price_data=importAllYahooFinanceFiles()),
      window_width_months=12L,
      min_observations_num=180L
) {
  computed_returns %>%
  mutate(dummy=TRUE) %>%
  {
    full_join(
      x=distinct(.,
        month_beginning=lubridate::floor_date(date, unit="month") %>% as.Date,
        dummy
      ),
      y=.,
      by="dummy"
    )
  } %>%
  select(-dummy) %>%
  mutate(
    months_diff=lubridate::interval(month_beginning, date) %/% months(1)
  ) %>%
  filter(months_diff >= 0L, months_diff < window_width_months) %>%
  group_by(symbol, month_beginning) %>%
  filter(
    n() >= min_observations_num,
    symbol != "USDT-USD"
  ) %>%
  ddply(
    .parallel=TRUE,
    .variables=c("symbol", "month_beginning"),
    .fun=runRegressionLeads
  ) %>%
  as_tibble()
}


#' Summarise results of momentum prevalence analysis

summariseMomentumPrevalence <- function(prevalence_results) {
  prevalence_results %>%
  crossing(p_level=c(0.05, 0.01, 0.001)) %>%
  group_by(symbol, month_beginning, p_level) %>%
  summarise(value=sum(and(prob <= p_level, estimate > 0))) %>%
  ungroup %>%
  mutate_at("p_level", . %>% str_c("p", .))
}
