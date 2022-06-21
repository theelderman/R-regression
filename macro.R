
##############################
## Preparatory steps
# Housekeeping
  rm(list=ls())

# Packages 
  library(rmarkdown)  # to generate pdf file
  library(tinytex)    # to generate pdf file
  library(readxl)     # to read Excel files
  library(car)        # to perform joint hypothesis tests

# Set working directory
  setwd("/Users/taihanrui/Desktop/工作路径")

  ##############################
  ## Import data
  data <- read_excel("./data/macro.xlsx")
  colnames(data) <- c("month", "MICROSOFT", "SANDP", "CPI", "INDPRO", "M1SUPPLY", "CCREDIT", 
                      "BMINUSA", "USTB3M", "USTB10Y")
  data       <- as.data.frame(data)
  data$month <- as.Date(data$month)
  str(data)
  
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

  head(data[ , c(1, 11:15, 17, 21:22) ])
  tail(data[ , c(1, 11:15, 17, 21:22) ])

# Restrict sample period
  data <- data[ (data$month >= "1986-05-01")  &  (data$month <= "2018-03-01"), ]
  head(data[ , c(1, 11:15, 17, 21:22) ])
  tail(data[ , c(1, 11:15, 17, 21:22) ])
  
##############################
## Multivariate regression  
  unr_regr  <- lm(ermsoft ~   ersandp 
                            + dcredit 
                            + dprod 
                            + dinflation 
                            + dmoney 
                            + dspread 
                            + rterm, 
                  data = data)
  summary(unr_regr)
  
  T <- nrow(unr_regr$model)
  print(T)
  
  k <- ncol(unr_regr$model)
  print(k)

##############################
## Joint hypothesis test: beta_3=0 and ... and beta_7=0
  alpha <- 0.05
  m <- 5
  
# Approach 1: run unrestricted and restricted regression
# - Compare test statistic and critical value
    urss <- sum( summary(unr_regr)$residuals^2 )
    print(urss)
    
    res_regr = lm(ermsoft ~ ersandp + rterm, data=data)
    summary(res_regr)
    
    rrss <- sum( summary(res_regr)$residuals^2 )
    print(rrss)
    
    hyp1.F_stat <- (rrss - urss)/urss * (T-k)/m
    print(hyp1.F_stat)
    
    hyp1.F_crit <- qf(1-alpha, m, T-k)
    print(hyp1.F_crit)
    
    if ( hyp1.F_stat > hyp1.F_crit ) {
      hyp1.reject <- "reject H0"
    } else {
      hyp1.reject <- "do not reject H0"
    }
    print(hyp1.reject)

# - Compare p-value and significance level alpha  
    hyp1.p <- pf(hyp1.F_stat, m, T-k, lower.tail = FALSE)
    print(hyp1.p)
    
    print(alpha)
    
    if ( hyp1.p < alpha ) {
      hyp1.reject <- "reject H0"
    } else {
      hyp1.reject <- "do not reject H0"
    }
    print(hyp1.reject)

# - summarize results
    hyp1.table <- matrix( c(round(hyp1.p, 4),
                            alpha, 
                            round(hyp1.F_stat, 4), 
                            round(hyp1.F_crit, 4), 
                            hyp1.reject), 
                          ncol=5)
    colnames(hyp1.table) <- c("p-value", 
                              "alpha", 
                              "F-test statistic",
                              "F-critical value", 
                              "reject?")
    rownames(hyp1.table) <- ""
    hyp1.table <- as.table(hyp1.table)
    print(hyp1.table)

# Approach 2: linearHypothesis function
# - compare test statistic and critical value
    hyp1 <- linearHypothesis( unr_regr, c( "dcredit    = 0", 
                                           "dprod      = 0", 
                                           "dinflation = 0", 
                                           "dmoney     = 0", 
                                           "dspread    = 0" ))
    print(hyp1)
    
    hyp1.F_stat <- hyp1$F[2]
    print(hyp1.F_stat)
    
    hyp1.F_crit <- qf(1-alpha, m, T-k)
    print(hyp1.F_crit)
    
    if ( hyp1.F_stat > hyp1.F_crit ) {
      hyp1.reject <- "reject H0"
    } else {
      hyp1.reject <- "do not reject H0"
    }
    print(hyp1.reject)

# - compare p-value and significance level alpha  
    hyp1.p <- pf(hyp1.F_stat, m, T-k, lower.tail = FALSE)
    print(hyp1.p)
    
    print(alpha)
    
    if ( hyp1.p < alpha ) {
      hyp1.reject <- "reject H0"
    } else {
      hyp1.reject <- "do not reject H0"
    }
    print(hyp1.reject)

# - summarize results  
    hyp1.table <- matrix( c(round(hyp1.p, 4),
                            alpha, 
                            round(hyp1.F_stat, 4), 
                            round(hyp1.F_crit, 4), 
                            hyp1.reject), 
                          ncol=5)
    colnames(hyp1.table) <- c("p-value", 
                              "alpha", 
                              "F-test statistic",
                              "F-critical value", 
                              "reject?")
    rownames(hyp1.table) <- ""
    hyp1.table <- as.table(hyp1.table)
    print(hyp1.table)

##############################
