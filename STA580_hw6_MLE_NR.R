data <- c(22.0, 23.9, 20.9, 23.8, 25.0, 24.0, 21.7, 
          23.8, 22.8, 23.1, 23.1, 23.5, 23.0, 23.0)

sum.ln <- function(x){
   n <- length(x)
   sum = 0
   for (i in 1:n) {
      sum = sum + log(x[i])
   }
   sum
}
c <- sum.ln(data)
d <- n*log(mean(data))
# find first derivative
f.prime <- function(alpha) n*log(alpha) - d - n*digamma(alpha) + c
# find second derivative
f.double <- function(alpha) (n/alpha) - n*trigamma(alpha)

newton.raphson.gamma <- function(x1 = 1, tol = 1e-8){
   x0 = x1 + 1
   while (abs(x1 - x0) > tol) {
      x0 <- x1
      x1 <- x0 - f.prime(x0)/f.double(x0)
   }
   alpha.hat = x1
}

# Newton-Raphson for estimating mle
mle.alpha <- newton.raphson.uni()
mle.alpha

mle.beta <- mean(data)/mle.alpha
mle.beta

###################################################################

data <- c(22.0, 23.9, 20.9, 23.8, 25.0, 24.0, 21.7, 
          23.8, 22.8, 23.1, 23.1, 23.5, 23.0, 23.0)

mle.beta <- max(data)
mle.beta

sum.ln <- function(x){
   n <- length(x)
   sum = 0
   for (i in 1:n) {
      sum = sum + log(x[i])
   }
   sum
}

sumln <- sum.ln(data)
# find first derivative
f.prime <- function(alpha) n/alpha + sumln - n*log(mle.beta)
# find second derivative
f.double <- function(alpha) -(n/alpha^2)

newton.raphson.710c <- function(x1 = 1, tol = 1e-8){
   x0 = x1 + 1
   while (abs(x1 - x0) > tol) {
      x0 <- x1
      x1 <- x0 - f.prime(x0)/f.double(x0)
   }
   alpha.hat = x1
}

# Newton-Raphson for estimating mle
mle.alpha <- newton.raphson.uni()
mle.alpha






















