
##############################
## Preparatory steps
# Housekeeping
  rm(list=ls())

# Packages 
  library(rmarkdown)  # to generate pdf file
  library(tinytex)    # to generate pdf file
  library(readxl)     # to read Excel files
  library(moments)    # to perform Jarque-Bera test
  
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
## Jarque-Bera test
# Approach 1: Chi-squared version of the test
# - Compare test statistic and critical value
    alpha <- 0.05
    
    b1 <- mean(data$u^3) / mean(data$u^2)^(3/2)
    print(b1)
    
    b2 <- mean(data$u^4) / mean(data$u^2)^2
    print(b2)
    
    hyp1.test_stat <- T * ( b1^2 / 6 + (b2 - 3)^2 / 24 )
    print(hyp1.test_stat)
    
    hyp1.crit_val <- qchisq(1-alpha, 2)
    print(hyp1.crit_val)
    
    if ( hyp1.test_stat > hyp1.crit_val ) {
      hyp1.reject <- "reject H0"
    } else {
      hyp1.reject <- "do not reject H0"
    }
    print(hyp1.reject)

# - Compare p-value and significance level
    hyp1.p <- 1 - pchisq(hyp1.test_stat, 2)
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

# Approach 2: jarque.test function
# - Compare test statistic and critical value
    hyp2 <- jarque.test(data$u)
    print(hyp2)
    
    hyp2.test_stat  <- hyp2$statistic
    print(hyp1.test_stat)
    
    hyp2.crit_val <- qchisq(1-alpha, 2)
    print(hyp2.crit_val)
    
    if ( hyp2.test_stat > hyp2.crit_val ) {
      hyp2.reject <- "reject H0"
    } else {
      hyp2.reject <- "do not reject H0"
    }
    print(hyp2.reject)

# - Compare p-value and significance level
    hyp2.p <- 1 - pchisq(hyp2.test_stat, 2)
    print(hyp2.p)
    
    print(alpha)
    
    if ( hyp2.p < alpha ) {
      hyp2.reject <- "reject H0"
    } else {
      hyp2.reject <- "do not reject H0"
    }
    print(hyp2.reject)

# - Summarize results
    hyp2.table <- matrix( c(round(hyp2$p.value,4), 
                            alpha, 
                            round(hyp2.test_stat,4), 
                            round(hyp2.crit_val,4), 
                            hyp2.reject), 
                          ncol=5)
    colnames(hyp2.table) <- c("p-value", 
                              "alpha", 
                              "test statistic", 
                              "critical value", 
                              "reject?")
    rownames(hyp2.table) <- ""
    hyp2.table <- as.table(hyp2.table)
    print(hyp2.table)

##############################
## Generating a pdf file which summarizes your code and your output
# go to File\Compile report to generate a pdf file containing code and output
    