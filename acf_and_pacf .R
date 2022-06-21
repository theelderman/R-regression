
##############################
## Preparatory steps
# Housekeeping
  rm(list=ls())

# Packages 
  library(rmarkdown)  # to generate pdf file
  library(tinytex)    # to generate pdf file
  library(readxl)     # to read Excel files
  library(latex2exp)  # to get formulas into plots
  library(tseries)    # to estimate ARMA model

# Working directory
  setwd("/Users/taihanrui/Desktop/工作路径")

##############################
## Import data from Excel file
  data <- read_excel("./UKHP.xlsx")
  data <- as.data.frame(data)
  colnames(data) <- c("month", "P_t")
  data$month <- as.Date(data$month)

##############################
## Calculate returns and adjust sample period
  T             <- nrow(data)
  data$P_tmin1  <- c( NA, data$P_t[ 1 : T-1 ] )
  data$R_t      <- data$P_t / data$P_tmin1 - 1
  head(data)
  tail(data)

  data <- data[ data$month >= "1991-02-01" & data$month <= "2018-03-30", ]

##############################
## Estimate ACF and PACF
  ukhp_acf  <- acf(data$R_t,  12, plot=FALSE)
  print(ukhp_acf)
  ukhp_pacf <- pacf(data$R_t, 12, plot=FALSE)
  print(ukhp_pacf)
  
  table <- matrix( c( (1:12),
                      round(ukhp_acf$acf[2:13],  4), 
                      round(ukhp_pacf$acf, 4)), 
                   ncol=3)
  colnames(table) <- c("Lag",
                       "ACF", 
                       "PACF")
  print(table)
  
##############################
## ACF and PACF plots
  par(mfrow  = c(1,2),
      pty    = "s", 
      las    = 0, 
      mgp    = c(2.5, 1, 0),
      mar    = c(0, 4, 0, 0),
      family ="serif")
  acf(data$R_t,  12, ylim=c(-0.2, 0.5), main="")
  grid()
  pacf(data$R_t, 12, ylim=c(-0.2, 0.5), main="")
  grid()
  mtext(TeX("Return, $R_t$"), side=3, line=-4, outer=TRUE)
  
##############################
## Estimate ARMA(3,1) model
  p <- 0
  q <- 1
  arma_pq <- arma(data$R_t, order = c(p, q))
  summary(arma_pq)

##############################
## Information criteria for ARMA(3,1) model
  u_hat <- arma_pq$residuals[(1+p+q):nrow(data)]
  head(u_hat)

  k <- p+q+1
  print(k)

  T <- length(u_hat)
  print(T)

  sigma_hat_sq <- 1/(T-k) * sum(u_hat^2)
  print(sigma_hat_sq)

  AIC <- log(sigma_hat_sq) + 2 * k/T
  print(AIC)

  SBIC <- log(sigma_hat_sq) + k/T * log(T)
  print(SBIC)
  
##############################
## Information criteria for alternative ARMA(p,q) models
  AIC_alt  <- matrix(data=NA,nrow=4,ncol=4)
  SBIC_alt <- matrix(data=NA,nrow=4,ncol=4)
  rownames(AIC_alt)  <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)")
  colnames(AIC_alt)  <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)")
  rownames(SBIC_alt) <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)")
  colnames(SBIC_alt) <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)")
  for ( i in 1:4 ) {
    for ( j in 1:4 ) {
      p_i <- i-1
      q_j <- j-1
      arma_pq       <- arma(data$R_t, order = c(p_i, q_j))
      u_hat         <- arma_pq$residuals[(1+p_i+q_j):nrow(data)]
      k             <- p_i+q_j+1
      sigma_hat_sq  <- 1/(T-k) * sum(u_hat^2)
      AIC_alt[i,j]  <- log(sigma_hat_sq) + 2 * k/T
      SBIC_alt[i,j] <- log(sigma_hat_sq) +     k/T * log(T)
    }
  }
  
  print(AIC_alt)
  print(SBIC_alt)
  
  min(AIC_alt)
  aux <- which( AIC_alt == min(AIC_alt), arr.ind = TRUE )
  paste0("AIC: ARMA(", aux[1]-1, ",", aux[2]-1, ")")
  
  min(SBIC_alt)
  aux <- which( SBIC_alt == min(SBIC_alt), arr.ind = TRUE )
  paste0("SBIC: ARMA(", aux[1]-1, ",", aux[2]-1, ")")

##############################
