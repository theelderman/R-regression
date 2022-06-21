
##############################
## Preparatory steps
# Housekeeping
  rm(list=ls())

# Packages 
  library(rmarkdown)  # to generate pdf file
  library(tinytex)    # to generate pdf file
  library(readxl)     # to read Excel files
  library(lmtest)     # to estimate regression using Newey-West standard errors
  library(sandwich)   # to estimate regression using Newey-West standard errors
  
# Set working directory
  setwd("/Users/taihanrui/Desktop/工作路径")
  
##############################
## Import data
  data <- read_excel("./data/macro.xlsx")
  colnames(data) <- c("month", "MICROSOFT", "SANDP", "CPI", "INDPRO", "M1SUPPLY", "CCREDIT", 
                      "BMINUSA", "USTB3M", "USTB10Y")
  data       <- as.data.frame(data)
  data$month <- as.Date(data$month)
  
# Explanatory variables
  data$dspread    <- c(NA,       diff(data$BMINUSA))
  data$dcredit    <- c(NA,       diff(data$CCREDIT))
  data$dprod      <- c(NA,       diff(data$INDPRO))
  data$dmoney     <- c(NA,       diff(data$M1SUPPLY))
  data$rterm      <- c(NA,       diff(data$USTB10Y-data$USTB3M))
  data$inflation  <- c(NA, 100 * diff(log(data$CPI)))
  data$dinflation <- c(NA, 100 * diff(data$inflation))
  data$rsandp     <- c(NA, 100 * diff(log(data$SANDP)))
  data$rmsoft     <- c(NA, 100 * diff(log(data$MICROSOFT)))
  data$mustb3m    <- data$USTB3M/12
  data$ersandp    <- data$rsandp - data$mustb3m
  data$ermsoft    <- data$rmsoft - data$mustb3m
  
# Restrict sample period
  data <- data[ (data$month >= "1986-05-01")  &  (data$month <= "2018-03-01"), ]
  
##############################
## Multivariate regression  
  regr  <- lm(ermsoft ~   ersandp 
                        + dcredit 
                        + dprod 
                        + dinflation 
                        + dmoney 
                        + dspread 
                        + rterm, 
              data = data)
  summary(regr)
  
  T <- nrow(regr$model)
  print(T)
  
  data$u <- summary(regr)$residuals

##############################
## Breusch-Godfrey test with ten lags
# Lagged estimated residuals
  data$u1  <- c(0, data$u[1:T-1])
  data$u2  <- c(0, data$u1[1:T-1])
  data$u3  <- c(0, data$u2[1:T-1])
  data$u4  <- c(0, data$u3[1:T-1])
  data$u5  <- c(0, data$u4[1:T-1])
  data$u6  <- c(0, data$u5[1:T-1])
  data$u7  <- c(0, data$u6[1:T-1])
  data$u8  <- c(0, data$u7[1:T-1])
  data$u9  <- c(0, data$u8[1:T-1])
  data$u10 <- c(0, data$u9[1:T-1])
  
  head(data[,c(23:33)], 11)

# Auxiliary regression
  aux_regr  <- lm(u ~ ersandp + dprod + dcredit + dinflation + dmoney + dspread + rterm 
                      + u1 + u2 + u3 + u4 + u5 + u6 + u7 + u8 + u9 + u10, data=data)
  summary(aux_regr)
  
  R2_aux <- summary(aux_regr)$r.squared
  print(R2_aux)

# Chi-squared version of the test
# - Compare test statistic and critical value
    alpha <- 0.05
    
    r <- 10
    
    hyp1.test_stat <- R2_aux * (T-r)
    print(hyp1.test_stat)
    
    hyp1.crit_val <- qchisq(1-alpha, r)
    print(hyp1.crit_val)
    
    if ( hyp1.test_stat > hyp1.crit_val ) {
      hyp1.reject <- "reject H0"
    } else {
      hyp1.reject <- "do not reject H0"
    }
    print(hyp1.reject)

# - Compare p-vlaue and significance level
    hyp1.p <- 1 - pchisq(hyp1.test_stat, r)
    print(hyp1.p)
    
    print(alpha)
    
    if ( hyp1.p < alpha ) {
      hyp1.reject <- "reject H0"
    } else {
      hyp1.reject <- "do not reject H0"
    }
    print(hyp1.reject)
    
# - Summarize results
    hyp1.table <- matrix( c(round(hyp1.p,4), 
                            alpha, 
                            round(hyp1.test_stat,4), 
                            round(hyp1.crit_val,4), 
                            hyp1.reject), 
                          ncol=5)
    colnames(hyp1.table) <- c("p-value", 
                              "alpha", 
                              "test statistic", 
                              "critical value", 
                              "reject?")
    rownames(hyp1.table) <- ""
    hyp1.table <- as.table(hyp1.table)
    print(hyp1.table)

##############################
## Newey-West's heteroscedasticity and autocorrelation consistent (HAC) standard errors
  coeftest(regr, vcov. = NeweyWest(regr, lag = 10, adjust = FALSE, prewhite = FALSE) )
  summary(regr)

##############################
