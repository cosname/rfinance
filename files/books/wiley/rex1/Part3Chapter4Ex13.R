## Creating LP
Lp <- function(RE, ES, Buffer, ub = 0.4){
  obj <- as.vector(RE)
  ## Initialise lhs-matrix and rhs vector
  nvar <- length(obj)
  ## wealth constraint
  a1 <- rep(1, nvar) 
  b1 <- 1
  d1 <- "<="
  ## risk constraint
  a2 <- as.vector(ES)
  b2 <- Buffer
  d2 <- "<="
  ## upper bound
  a3 <- diag(nvar)
  b3 <- rep(ub, nvar)
  d3 <- rep("<=", nvar)
  ## Combining
  A <- rbind(a1, a2, a3)
  b <- c(b1, b2, b3)
  d <- c(d1, d2, d3)
  ans <- Rglpk_solve_LP(obj, mat = A, dir = d, rhs = b, max = TRUE)
  ans
}
