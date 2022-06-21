

##############################
## Preparatory steps
# Housekeeping
  rm(list=ls())

# Packages 
  library(rmarkdown)  # to generate pdf file
  library(tinytex)    # to generate pdf file
  library(readxl)     # to read Excel files
  library(latex2exp)  # to get formulas into plots
  
# Set working directory
  setwd("/Users/taihanrui/Desktop/工作路径")

##############################
## Import data
  data <- read_excel("./dataset1.xlsx")
  data <- as.data.frame(data)

##weather it si a balanced dataframe question a
  aggregate(data$year, by=list(type=data$distid),length)
  aggregate(data$distid, by=list(type=data$year),length)  

##############################
## Pooled regression - no distinction
  reg_1 <- lm(math4 ~ enrol + lunch + expp, data=data)
  summary(reg_1)
  
  data$c_distid <- as.factor(data$distid)
  head(data)
  reg_2 <- lm(math4 ~ enrol + lunch + expp + c_distid, data=data)  
  summary(reg_2)
  
  data$c_year <- as.factor(data$year)
  head(data)
  reg_3 <- lm(math4 ~ enrol + lunch + expp + c_year + c_distid, data=data)  
  summary(reg_3)
  
  reg_3$df.residual
  reg_2$df.residual
  reg_1$df.residual
  
##generate dummy variables
  data$D2i <- ifelse(data$year == 1992, 1, 0)
  data$D3i <- ifelse(data$year == 1993, 1, 0)
  data$D4i <- ifelse(data$year == 1994, 1, 0)
  data$D5i <- ifelse(data$year == 1995, 1, 0)
  data$D6i <- ifelse(data$year == 1996, 1, 0)
  data$D7i <- ifelse(data$year == 1997, 1, 0)
  data$D8i <- ifelse(data$year == 1998, 1, 0)
  
  reg_4 <- lm(math4 ~ 0+enrol + lunch + expp +D8i+ D2i+D3i+D4i+D5i+D6i+D7i , data=data)  
  
  summary(reg_4)
  
  reg_5 <- lm(math4 ~ enrol + lunch + expp + D2i+D3i+D4i+D5i+D6i+D7i+D8i , data=data)  
  summary(reg_5)
  
#question2
  #' ---
  #' title: GRA6547 Research Methodology in Finance
  #' subtitle: chapter_5-2_acf_and_pacf
  #' author: ''
  #' date: ''
  #' output: pdf_document
  #' ---
  
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
  data1 <- read_excel("./dataset2.xlsx")
  data1 <- as.data.frame(data1)
  data <- data1[ data1$date < "2006-01-01" , ]
  
  ##############################
  ## Calculate returns and adjust sample period
  TT             <- nrow(data)
  data$R_t      <- data$ret
  head(data)
  tail(data)
  
  ##############################
  ## Information criteria for alternative ARMA(p,q) models
  AIC_alt  <- matrix(data=NA,nrow=4,ncol=4)
  rownames(AIC_alt)  <- c("AR(0)", "AR(1)", "AR(2)", "AR(3)")
  colnames(AIC_alt)  <- c("MA(0)", "MA(1)", "MA(2)", "MA(3)")
  for ( i in 1:4 ) {
    for ( j in 1:4 ) {
      p_i <- i-1
      q_j <- j-1
      T=TT-max(p_i,q_j)
      print(T)
      arma_pq       <- arma(data$R_t, order = c(p_i, q_j))
      u_hat         <- arma_pq$residuals[(1+p_i+q_j):nrow(data)]
      k             <- p_i+q_j+1
      sigma_hat_sq  <- 1/(T-k) * sum(u_hat^2)
      AIC_alt[i,j]  <- log(sigma_hat_sq) + 2 * k/T
    }
  }
  
  print(AIC_alt)
  
  min(AIC_alt)
  aux <- which( AIC_alt == min(AIC_alt), arr.ind = TRUE )
  paste0("AIC: ARMA(", aux[1]-1, ",", aux[2]-1, ")")
  ##############################
  #ac and pac of return 
  ukhp_acf1  <- acf(data$R_t,  20, plot=FALSE)
  print(ukhp_acf1)
  ukhp_pacf1 <- pacf(data$R_t, 20, plot=FALSE)
  print(ukhp_pacf1)
  #use bg test to determine whether the return rate is white noise
  for (i in 1:20){
    print(Box.test(data$R_t,lag=i, type="Ljung-Box"))
  }
  ##############################
  ## Estimate ARMA(0,0) model
  p <- 0
  q <- 0
  arma_pq <- arma(data$R_t, order = c(p, q))
  summary(arma_pq)
  
  ##############################
  ## Information criteria for ARMA(0,0) model
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
  
  
  ##############################
  ## Estimate ACF and PACF
  ukhp_acf  <- acf(u_hat,  20, plot=FALSE)
  print(ukhp_acf)
  ukhp_pacf <- pacf(u_hat, 20, plot=FALSE)
  print(ukhp_pacf)
  
  table <- matrix( c( (1:20),
                      round(ukhp_acf$acf[2:21],  4), 
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
  acf(u_hat,  20, ylim=c(-0.2, 0.5), main="")
  grid()
  pacf(u_hat, 20, ylim=c(-0.2, 0.5), main="")
  grid()
  mtext(TeX("Return, $u_hat$"), side=3, line=-4, outer=TRUE)
  
  arma_pq <- arima(data$R_t, order = c(p, 0, q))
  
  #l.cc
  L=length(data$date)
  data$l1_cc=c(NA,data$cc[1:L-1])
  
  #reg ret cc(t-1)
  reg_ret <- lm(ret ~ l1_cc , data=data)
  summary(reg_ret)
  
  library("lmtest")
  library("sandwich")
  
  # Robust t test
  coeftest(reg_ret, vcov = vcovHC(reg_ret))
  
  #calculate predicted error of reg
  l=length(data1$date)
  Y_predicted=rep(0,l-L)
  u_hat_reg = rep(0,l-L)
  for (i in 1:56){
    datatest=data1
    datatest$n = seq(1,l,1)
    datatest$l1_cc=c(NA,data1$cc[1:l-1])
    datatest = datatest[datatest$n <= L-1+i,]
    reg_rettest <- lm(ret ~ l1_cc , data=datatest)
    LX = data.frame(l1_cc=as.numeric(datatest$cc[L-1+i]))
    Y_predicted[i]=as.numeric(predict(reg_rettest,LX))
    u_hat_reg[i]=as.numeric(data1$ret[L+i])-Y_predicted[i]
  }
  
  #calculate predicted error of arima
  Y_arma_pre=rep(0,l-L)
  u_hat_arma = rep(0,l-L)
  for (i in 1:56){
    datatest=data1
    datatest$n = seq(1,l,1)
    datatest = datatest[datatest$n <= L-1+i,]
    arma_pqtest <- arima(datatest$ret, order = c(p, 0, q))
    Y_arma_pre[i]=predict(arma_pqtest,h=1)$pred[1]
    u_hat_arma[i]=as.numeric(data1$ret[L+i])-Y_arma_pre[i]
  }
  print(u_hat_reg)
  print(u_hat_arma)
  sqrt(mean(u_hat_reg**2))
  sqrt(mean(u_hat_arma**2))
