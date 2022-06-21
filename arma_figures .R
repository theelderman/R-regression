##############################
## Preparatory steps
# Housekeeping
  rm(list=ls())

# Packages 
  library(rmarkdown)  # to generate pdf file
  library(tinytex)    # to generate pdf file

##############################
## Base parameters for simulation
T <- 100000
first <- 1 + 0.1 * T    # drop first 10% of observations
set.seed(5)
u <- rnorm(T, 0, 0.2)
y <- rep(NA, T)

##############################
## Figure 6.2: MA(1)
y[1] <- 0           # long-term average
for ( t in 2 : T )  {
  y[t] <- -1/2 * u[t-1] + u[t]
}
y <- y[first : T]   # drop first 10% of observations

par(mfrow  = c(1,2),
    pty    = "s", 
    las    = 0, 
    mgp    = c(2.5, 1, 0),
    mar    = c(0, 4, 0, 0),
    family ="serif")
acf(y,  10, main="", xlim=c(1,10), ylim=c(-0.45,0.05))
grid()
pacf(y, 10, main="", xlim=c(1,10), ylim=c(-0.45,0.05))
grid()
mtext("MA(1)", side=3, line=-4, outer=TRUE)

##############################
## Figure 6.3: MA(2)
y[1:2] <- 0         # long-term average
for ( t in 3 : T )  {
  y[t] <- 1/2 * u[t-1] - 1/4 * u[t-2] + u[t]
}
y <- y[first : T]   # drop first 10% of observations

par(mfrow  = c(1,2),
    pty    = "s", 
    las    = 0, 
    mgp    = c(2.5, 1, 0),
    mar    = c(0, 4, 0, 0),
    family ="serif")
acf(y,  10, main="", xlim=c(1,10), ylim=c(-0.4,0.4))
grid()
pacf(y, 10, main="", xlim=c(1,10), ylim=c(-0.4,0.4))
grid()
mtext("MA(2)", side=3, line=-4, outer=TRUE)

##############################
## Figure 6.4: Slowly declining AR(1)
y[1] <- 0         # long-term average
for ( t in 2 : T )  {
  y[t] <- 0.9 * y[t-1] + u[t]
}
y <- y[first : T]   # drop first 10% of observations

par(mfrow  = c(1,2),
    pty    = "s", 
    las    = 0, 
    mgp    = c(2.5, 1, 0),
    mar    = c(0, 4, 0, 0),
    family ="serif")
acf(y,  10, main="", xlim=c(1,10), ylim=c(-0.1,1.0))
grid()
pacf(y, 10, main="", xlim=c(1,10), ylim=c(-0.1,1.0))
grid()
mtext("Slowly declining AR(1)", side=3, line=-4, outer=TRUE)

##############################
## Figure 6.5: Rapidly declining AR(1)
y[1] <- 0         # long-term average
for ( t in 2 : T )  {
  y[t] <- 1/2 * y[t-1] + u[t]
}
y <- y[first : T]   # drop first 10% of observations

par(mfrow  = c(1,2),
    pty    = "s", 
    las    = 0, 
    mgp    = c(2.5, 1, 0),
    mar    = c(0, 4, 0, 0),
    family ="serif")
acf(y,  10, main="", xlim=c(1,10), ylim=c(-0.1,0.6))
grid()
pacf(y, 10, main="", xlim=c(1,10), ylim=c(-0.1,0.6))
grid()
mtext("Rapidly declining AR(1)", side=3, line=-4, outer=TRUE)

##############################
## Figure 6.6: More rapidly declining AR(1) with negative coefficient
y[1] <- 0         # long-term average
for ( t in 2 : T )  {
  y[t] <- -1/2 * y[t-1] + u[t]
}
y <- y[first : T]   # drop first 10% of observations

par(mfrow  = c(1,2),
    pty    = "s", 
    las    = 0, 
    mgp    = c(2.5, 1, 0),
    mar    = c(0, 4, 0, 0),
    family ="serif")
acf(y,  10, main="", xlim=c(1,10), ylim=c(-0.1,0.6))
grid()
pacf(y, 10, main="", xlim=c(1,10), ylim=c(-0.1,0.6))
grid()
mtext("More rapidly declining AR(1) with negative coefficient",
      side=3, line=-4, outer=TRUE)

##############################
## Figure 6.7: AR(1) with unit coefficient
y[1] <- 0         # long-term average
for ( t in 2 : T )  {
  y[t] <- 1 * y[t-1] + u[t]
}
y <- y[first : T]   # drop first 10% of observations

par(mfrow  = c(1,2),
    pty    = "s", 
    las    = 0, 
    mgp    = c(2.5, 1, 0),
    mar    = c(0, 4, 0, 0),
    family ="serif")
acf(y,  10, main="", xlim=c(1,10), ylim=c(0,1.0))
grid()
pacf(y, 10, main="", xlim=c(1,10), ylim=c(0,1.0))
grid()
mtext("AR(1) with unit coefficient (non-stationary)",
      side=3, line=-4, outer=TRUE)

##############################
## Figure 6.8: ARMA(1,1)
y[1] <- 0           # long-term average
for ( t in 2 : T )  {
  y[t] <- 1/2 * y[t-1] + 1/2 * u[t-1] + u[t]
}
y <- y[first : T]   # drop first 10% of observations

par(mfrow  = c(1,2),
    pty    = "s", 
    las    = 0, 
    mgp    = c(2.5, 1, 0),
    mar    = c(0, 4, 0, 0),
    family ="serif")
acf(y,  10, main="", xlim=c(1, 10), ylim=c(-0.4, 0.8))
grid()
pacf(y, 10, main="", xlim=c(1, 10), ylim=c(-0.4, 0.8))
grid()
mtext("ARMA(1,1)", side=3, line=-4, outer=TRUE)

##############################
## Generating a pdf file which summarizes your code and your output
# go to File\Compile report to generate a pdf file containing code and output
