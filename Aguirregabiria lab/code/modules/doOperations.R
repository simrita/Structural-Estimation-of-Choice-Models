bellman_contraction <- function(V.fn, util, Q, beta = .95) {
  force(V.fn)
  force(util)
  v <-
    function(x) {
      # print(x)
      val <- c(util(x, x, 1) + beta * V.fn(Q), util(x, x, 0) + beta * V.fn(0))
      integrand <- function(s) {
        val <- c(util(x, s, 1) + beta * V.fn(Q), util(x, s, 0) + beta * V.fn(x - s))
        dlnorm(s) * (max(val) + log(sum(exp(val - max(val)))))
      }
      (max(val) + log(sum(exp(val - max(val))))) * (1 - plnorm(x)) +
        integrate( # interior part
          integrand,
          lower = 0,
          upper = x
        )$value
    }
  Vectorize(v)
}

value_fn_iteration <- function(util, Q, beta = .95) {
  epsilon <- 10^-4
  delta <- 1 + epsilon
  eval.points <- seq(0, 20, length.out = 20)
  V <- function(x) x
  while (delta > epsilon) {
    V. <- V
    V <- bellman_contraction(V, util, Q)
    V <-
      ipol(
        val = V,
        intervals = c(0, Q),
        dims = 20
      )
    delta <- max(abs(V(eval.points) - V.(eval.points)))
    print(delta)
  }
  V
}
