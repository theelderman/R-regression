
##############################
## Preparatory steps
# Housekeeping
  rm(list=ls())

# Packages 
  library(rmarkdown)  # to generate pdf file
  library(tinytex)    # to generate pdf file
  library(readxl)     # to read Excel files
  library(car)        # to perform joint hypothesis tests
  library(lmtest)     # to estimate regression using White's standard errors
  library(sandwich)   # to estimate regression using White's standard errors
  
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
  
  data$u  <- summary(regr)$residuals
  data$u2 <- data$u^2

##############################
## White's test for heteroscedasticity
# Run auxiliary regression
  data$c11 <- data$ersandp^2
  data$c12 <- data$ersandp    * data$dprod
  data$c13 <- data$ersandp    * data$dcredit
  data$c14 <- data$ersandp    * data$dinflation
  data$c15 <- data$ersandp    * data$dmoney
  data$c16 <- data$ersandp    * data$dspread
  data$c17 <- data$ersandp    * data$rterm
  data$c22 <- data$dprod^2
  data$c23 <- data$dprod      * data$dcredit
  data$c24 <- data$dprod      * data$dinflation
  data$c25 <- data$dprod      * data$dmoney
  data$c26 <- data$dprod      * data$dspread
  data$c27 <- data$dprod      * data$rterm
  data$c33 <- data$dcredit^2
  data$c34 <- data$dcredit    * data$dinflation
  data$c35 <- data$dcredit    * data$dmoney
  data$c36 <- data$dcredit    * data$dspread
  data$c37 <- data$dcredit    * data$rterm
  data$c44 <- data$dinflation^2
  data$c45 <- data$dinflation * data$dmoney
  data$c46 <- data$dinflation * data$dspread
  data$c47 <- data$dinflation * data$rterm
  data$c55 <- data$dmoney^2
  data$c56 <- data$dmoney     * data$dspread
  data$c57 <- data$dmoney     * data$rterm
  data$c66 <- data$dspread^2
  data$c67 <- data$dspread    * data$rterm
  data$c77 <- data$rterm^2
  
  unr_regr <- lm(u2 ~   ersandp  + dprod  + dcredit  + dinflation  + dmoney  + dspread  
                      + rterm
                      + c11 + c12 + c13 + c14 + c15 + c16 + c17
                      + c22 + c23 + c24 + c25 + c26 + c27
                      + c33 + c34 + c35 + c36 + c37
                      + c44 + c45 + c46 + c47
                      + c55 + c56 + c57
                      + c66 + c67
                      + c77, 
                        data = data)
  summary(unr_regr)
  
  T <- nrow(unr_regr$model)
  print(T)
  
  k <- ncol(unr_regr$model)
  print(k)
  
  R2_aux  <- summary(unr_regr)$r.squared
  print(R2_aux)

# Approach 1: Chi-squared version of the test
# - Compare test statistic and critical value
    alpha   <- 0.05
    
    m <- 35
    
    hyp1.test_stat <- R2_aux * T
    print(hyp1.test_stat)
    
    hyp1.crit_val <- qchisq(1-alpha, m)
    print(hyp1.crit_val)
    
    if ( hyp1.test_stat > hyp1.crit_val ) {
      hyp1.reject <- "reject H0"
    } else {
      hyp1.reject <- "do not reject H0"
    }
    print(hyp1.reject)

# - Compare p-value and significance level  
    hyp1.p <- 1 - pchisq(hyp1.test_stat, m)
    print(hyp1.p)
    
    print(alpha)
    
    if ( hyp1.p < alpha ) {
      hyp1.reject <- "reject H0"
    } else {
      hyp1.reject <- "do not reject H0"
    }
    print(hyp1.reject)
    
# - Summarize results
    hyp1.table <- matrix( c(round(hyp1.p, 4), 
                            alpha, 
                            round(hyp1.test_stat, 4), 
                            round(hyp1.crit_val, 4), 
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

# Approach 2: F-version of the test
# Approach 2.a: Run unrestricted and restricted regression
# - Compare test statistic and critical value
    alpha   <- 0.05
    
    m <- 35
    
    urss      <- sum( summary(unr_regr)$residuals^2 )
    print(urss)
    
    res_regr <- lm(u2 ~ 1, data=data)
    summary(res_regr)
    
    rrss <- sum( summary(res_regr)$residuals^2 )
    print(rrss)
    
    hyp2.F_stat <- (rrss - urss)/urss * (T-k)/m
    print(hyp2.F_stat)
    
    hyp2.F_crit <- qf(1-alpha, m, T-k)
    print(hyp2.F_crit)
    
    if ( hyp2.F_stat > hyp2.F_crit ) {
      hyp2.reject <- "reject H0"
    } else {
      hyp2.reject <- "do not reject H0"
    }
    print(hyp2.reject)

# - Compare p-value and significance level    
    hyp2.p <- pf(hyp2.F_stat, m, T-k, lower.tail = FALSE)
    
    print(alpha)
    
    if ( hyp2.p < alpha ) {
      hyp2.reject <- "reject H0"
    } else {
      hyp2.reject <- "do not reject H0"
    }
    print(hyp2.reject)

# - Summarize results    
    hyp2.table <- matrix( c(round(hyp2.p,4), 
                            alpha, 
                            round(hyp2.F_stat,4), 
                            round(hyp2.F_crit,4), 
                            hyp2.reject), 
                          ncol=5)
    colnames(hyp2.table) <- c("p-value", 
                              "alpha", 
                              "F-test statistic", 
                              "F-critical value", 
                              "reject?")
    rownames(hyp2.table) <- ""
    hyp2.table <- as.table(hyp2.table)
    print(hyp2.table)
  
# Approach 2.b: linearHypothesis function
# - Compare test statistic and critical value
    hyp2  <- linearHypothesis(unr_regr,
             c("ersandp=0", "dprod=0", "dcredit=0", "dinflation=0", "dmoney=0", "dspread=0", 
               "rterm=0", 
               "c11=0", "c12=0", "c13=0", "c14=0", "c15=0", "c16=0", "c17=0",
               "c22=0", "c23=0", "c24=0", "c25=0", "c26=0", "c27=0",
               "c33=0", "c34=0", "c35=0", "c36=0", "c37=0",
               "c44=0", "c45=0", "c46=0", "c47=0",
               "c55=0", "c56=0", "c57=0",
               "c66=0", "c67=0",
               "c77=0"))
    print(hyp2)
    
    hyp2.F_stat <- hyp2$F[2]
    print(hyp2.F_stat)
    
    print(hyp2.F_crit)
    
    if ( hyp2.F_stat > hyp2.F_crit ) {
      hyp2.reject <- "reject H0"
    } else {
      hyp2.reject <- "do not reject H0"
    }
    print(hyp2.reject)

# - Compare p-value and significance level
    hyp2.p <- pf(hyp2.F_stat, m, T-k, lower.tail = FALSE)
    print(hyp2.p)
    
    print(alpha)
    
    if ( hyp2.p < alpha ) {
      hyp2.reject <- "reject H0"
    } else {
      hyp2.reject <- "do not reject H0"
    }
    print(hyp2.reject)

# - Summarize results
    hyp2.table <- matrix( c(round(hyp2.p,4), 
                            alpha, 
                            round(hyp2.F_stat,4), 
                            round(hyp2.F_crit,4), 
                            hyp2.reject), 
                          ncol=5)
    colnames(hyp2.table) <- c("p-value", 
                              "alpha", 
                              "F-test statistic", 
                              "F-critical value", 
                              "reject?")
    rownames(hyp2.table) <- ""
    hyp2.table <- as.table(hyp2.table)
    print(hyp2.table)

##############################
## White's heteroscedasticity consistent standard errors    
  coeftest(regr, vcov = vcovHC(regr, type="HC0"))
  summary(regr)

##############################
