linear_demand <- function(a, b, p) {
  a - b * p
}

linear_revenue <- function(a, b, p) {
  p * linear_demand(a, b, p)
}

logistic_demand <- function(a, b, p, beta, X) {
  assert_named(beta)

  lin_pred <- b * p + sum(beta * X)
  a * (1 / (1 + exp(-lin_pred)))
}

logistic_revenue <- function(a, b, p, beta, X) {
  p * logistic_demand(a, b, p, beta, X)
}

optimizand_neg <- function(fun, ...) {
  -fun(...)
}

optimize(
  f = optimizand_neg,
  interval = c(.Machine$double.eps, 1000),
  fun = logistic_revenue,
  a = 1000,
  b = -0.02,
  beta = c("beta1" = -0.2),
  X = c("x1" = 4)
)
