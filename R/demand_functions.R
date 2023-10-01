linear_demand <- function(a, b, p) {
  a - b * p
}

linear_revenue <- function(a, b, p) {
  p * linear_demand(a, b, p)
}

logistic_demand <- function(a, b, p, beta = NULL, X = NULL) {
  assert_named(beta)
  assert_named(X)

  lin_pred <- b * p + sum(beta * X)
  a * (1 / (1 + exp(-lin_pred)))
}

logistic_revenue <- function(a, b, p, beta = NULL, X = NULL) {
  p * logistic_demand(a, b, p, beta, X)
}

regret_revenue <- function(revenue, delta = -0.2, interval = c(0, 1000), ...) {
  oracle <- optimize(
    f = revenue,
    interval = interval,
    maximum = TRUE,
    ...
  )
  p_oracle <- oracle$maximum
  rev_oracle <- oracle$objective
  # Compute regret:
  p_actual <- p_oracle * (1 + delta)
  abs_regret <- rev_oracle - revenue(p_actual, ...)
  rel_regret <- abs_regret / rev_oracle
  list(
    "actual_price" = p_actual,
    "oracle_price" = p_oracle,
    "oracle_revenue" = rev_oracle,
    "abs_regret" = abs_regret,
    "rel_regret" = rel_regret
  )
}
