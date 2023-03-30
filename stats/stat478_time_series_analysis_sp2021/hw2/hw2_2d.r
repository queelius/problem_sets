# homework #2: problem 2.c

# n is size of time series
n <- 250

w <- 0.5
beta0 <- 0
beta1 <- 0.05

# z(1),z(2) ~ N(0,1)
z <- rnorm(2, mean=0,sd=1)

# {e[t]}
e <- rnorm(n, mean=0,sd=1)

# {Y[t]} is the time series of interest
X <- vector(length=n)

for (t in 1:n)
{
    X[t] = beta0 + beta1*t + z[1]*cos(w*t) + z[2]*sin(w*t) + e[t]
}

pdf(file="plot2_d.pdf")
plot(diff(X,lag=1),type="l")
