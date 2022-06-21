
##############################
## Preparatory steps
# Housekeeping
  rm(list=ls())

# Packages 
  library(rmarkdown)  # to generate pdf file
  library(tinytex)    # to generate pdf file
  library(readxl)     # to read Excel files
  library(vars)       # to estimate VAR model
  library(car)        # to perform joint hypothesis tests

# Working directory
  setwd("/Users/taihanrui/Desktop/工作路径")

##############################
## Import data from Excel file
  data <- read_excel("./currencies.xlsx")
  data <- as.data.frame(data)
  colnames(data) <- c("month", "EUR", "GBP", "JPY")
  data$month <- as.Date(data$month)

##############################
## Calculate log returns and adjust sample period
  data$ret_EUR <- c( NA, 100 * diff( log(data$EUR) ) )
  data$ret_GBP <- c( NA, 100 * diff( log(data$GBP) ) )
  data$ret_JPY <- c( NA, 100 * diff( log(data$JPY) ) )
  head(data)
  data <- data[-1,]
  head(data)
  
##############################
## Construct y matrix
  y <- matrix( c(data$ret_EUR, data$ret_GBP, data$ret_JPY), 
               ncol=3 )
  colnames(y) <- c("ret_EUR", "ret_GBP", "ret_JPY")
  head(y)

##############################
## Identify optimal lag length for VAR based on SBIC
  opt_lag <- VARselect(y, lag.max=10)
  print(opt_lag)
  paste0("SBIC: VAR(", opt_lag$selection[3], ")")
  
##############################
## Estimate three-dimensional VAR(1)
  model <- VAR(y, p=opt_lag$selection[3])
  summary(model)

# Just for comparison
  T <- nrow(data)
  GC <- data.frame( data$ret_EUR, c(NA, data$ret_EUR[1:T-1]), 
                    data$ret_GBP, c(NA, data$ret_GBP[1:T-1]), 
                    data$ret_JPY, c(NA, data$ret_JPY[1:T-1]) )
  colnames(GC) <- c( "ret_EUR_t", "ret_EUR_t_min_1", 
                     "ret_GBP_t", "ret_GBP_t_min_1", 
                     "ret_JPY_t", "ret_JPY_t_min_1" )
  GC <- GC[-1,]
  head(GC)
  
  reg_EUR <- lm(ret_EUR_t ~ ret_EUR_t_min_1 + ret_GBP_t_min_1 + ret_JPY_t_min_1, data = GC)
  summary(reg_EUR)
  
  reg_GBP <- lm(ret_GBP_t ~ ret_EUR_t_min_1 + ret_GBP_t_min_1 + ret_JPY_t_min_1, data = GC)
  summary(reg_GBP)
  
  reg_JPY <- lm(ret_JPY_t ~ ret_EUR_t_min_1 + ret_GBP_t_min_1 + ret_JPY_t_min_1, data = GC)
  summary(reg_JPY)

##############################
## Granger causality
# Granger causality tests for EUR returns
  causality( model, cause = c("ret_GBP", "ret_JPY") )$Granger
  
# - Just for comparison
    res <- linearHypothesis(reg_EUR, c("ret_GBP_t_min_1 = 0", "ret_JPY_t_min_1 = 0"))
    paste0("Restrict GBP and JPY to zero: p-value = ", round( res$`Pr(>F)`[2], 4) )
    
    res <- linearHypothesis(reg_EUR, c("ret_GBP_t_min_1 = 0"))
    paste0("Restrict GBP to zero: p-value = ", round( res$`Pr(>F)`[2], 4) )
    
    res <- linearHypothesis(reg_EUR, c("ret_JPY_t_min_1 = 0"))
    paste0("Restrict JPY to zero: p-value = ", round( res$`Pr(>F)`[2], 4) )

# Granger causality tests for GBP returns
  causality( model, cause = c("ret_EUR", "ret_JPY") )$Granger
  
# - Just for comparison
    res <- linearHypothesis(reg_GBP, c("ret_EUR_t_min_1 = 0", "ret_JPY_t_min_1 = 0"))
    paste0("Restrict EUR and JPY to zero: p-value = ", round( res$`Pr(>F)`[2], 4) )
    
    res <- linearHypothesis(reg_GBP, c("ret_EUR_t_min_1 = 0"))
    paste0("Restrict EUR to zero: p-value = ", round( res$`Pr(>F)`[2], 4) )
    
    res <- linearHypothesis(reg_GBP, c("ret_JPY_t_min_1 = 0"))
    paste0("Restrict JPY to zero: p-value = ", round( res$`Pr(>F)`[2], 4) )
  
# Granger causality tests for JPY returns
  causality( model, cause = c("ret_EUR", "ret_GBP") )$Granger

  # - Just for comparison
    res <- linearHypothesis(reg_JPY, c("ret_EUR_t_min_1 = 0", "ret_GBP_t_min_1 = 0"))
    paste0("Restrict EUR and GBP to zero: p-value = ", round( res$`Pr(>F)`[2], 4) )
    
    res <- linearHypothesis(reg_JPY, c("ret_EUR_t_min_1 = 0"))
    paste0("Restrict EUR to zero: p-value = ", round( res$`Pr(>F)`[2], 4) )
    
    res <- linearHypothesis(reg_JPY, c("ret_GBP_t_min_1 = 0"))
    paste0("Restrict GBP to zero: p-value = ", round( res$`Pr(>F)`[2], 4) )

##############################
## Impulse response functions
  tau <- 20
  imp_res <- irf( model, n.ahead=tau, ortho=FALSE )

  round( imp_res$irf$ret_GBP, 4)

  par(mfrow  = c(3,3),
      pty    = "s", 
      las    = 0, 
      mgp    = c(2.5, 1, 0),
      mar    = c(2, 0, 2, 0),
      family ="serif")
  plot(0:tau, imp_res$irf$ret_EUR[,1],             type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "Response of EUR to EUR",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(0:tau, imp_res$irf$ret_GBP[,1],             type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "Response of EUR to GBP",
       xlim = c(0, tau),    ylim = c(-0.1, 0.1),
       xaxp = c(0, tau, 2), yaxp = c(-0.1, 0.1, 2))
  plot(0:tau, imp_res$irf$ret_JPY[,1],             type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "Response of EUR to JPY",
       xlim = c(0, tau),    ylim = c(-0.1, 0.1),
       xaxp = c(0, tau, 2), yaxp = c(-0.1, 0.1, 2))
  plot(0:tau, imp_res$irf$ret_EUR[,2],             type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "Response of GBP to EUR",
       xlim = c(0, tau),    ylim = c(-0.1, 0.1),
       xaxp = c(0, tau, 2), yaxp = c(-0.1, 0.1, 2))
  plot(0:tau, imp_res$irf$ret_GBP[,2],             type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "Response of GBP to GBP",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(0:tau, imp_res$irf$ret_JPY[,2],             type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "Response of GBP to JPY",
       xlim = c(0, tau),    ylim = c(-0.1, 0.1),
       xaxp = c(0, tau, 2), yaxp = c(-0.1, 0.1, 2))
  plot(0:tau, imp_res$irf$ret_EUR[,3],             type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "Response of JPY to EUR",
       xlim = c(0, tau),    ylim = c(-0.1, 0.1),
       xaxp = c(0, tau, 2), yaxp = c(-0.1, 0.1, 2))
  plot(0:tau, imp_res$irf$ret_GBP[,3],             type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "Response of JPY to GBP",
       xlim = c(0, tau),    ylim = c(-0.1, 0.1),
       xaxp = c(0, tau, 2), yaxp = c(-0.1, 0.1, 2))
  plot(0:tau, imp_res$irf$ret_JPY[,3],             type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "Response of JPY to JPY",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))

##############################
## Variance decompositions
  var_dec <- fevd( model, n.ahead=tau )
  
  round( matrix( c( var_dec$ret_GBP[,1], 
					var_dec$ret_GBP[,2], 
					var_dec$ret_GBP[,3], 
					var_dec$ret_GBP[,1] + var_dec$ret_GBP[,2] + var_dec$ret_GBP[,3] ), 
				  ncol=4 ), 
		 4)

  par(mfrow  = c(3,3),
      pty    = "s", 
      las    = 0, 
      mgp    = c(2.5, 1, 0),
      mar    = c(2, 0, 2, 0),
      family ="serif")
  plot(1:tau, var_dec$ret_EUR[,1],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of EUR variance due to EUR",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_EUR[,2],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of EUR variance due to GBP",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_EUR[,3],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of EUR variance due to JPY",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_GBP[,1],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of GBP variance due to EUR",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_GBP[,2],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of GBP variance due to GBP",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_GBP[,3],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of GBP variance due to JPY",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_JPY[,1],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of JPY variance due to EUR",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_JPY[,2],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of JPY variance due to GBP",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_JPY[,3],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of JPY variance due to JPY",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))

##############################
## Robustness check: Reverse ordering
  y <- matrix( c(data$ret_JPY, data$ret_GBP, data$ret_EUR), 
               ncol=3 )
  colnames(y) <- c("ret_JPY", "ret_GBP", "ret_EUR")
  model <- VAR(y, p=opt_lag$selection[3])
  var_dec <- fevd( model, n.ahead=tau )
  
  par(mfrow  = c(3,3),
      pty    = "s", 
      las    = 0, 
      mgp    = c(2.5, 1, 0),
      mar    = c(2, 0, 2, 0),
      family ="serif")
  plot(1:tau, var_dec$ret_EUR[,3],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of EUR variance due to EUR",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_EUR[,2],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of EUR variance due to GBP",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_EUR[,1],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of EUR variance due to JPY",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_GBP[,3],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of GBP variance due to EUR",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_GBP[,2],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of GBP variance due to GBP",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_GBP[,1],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of GBP variance due to JPY",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_JPY[,3],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of JPY variance due to EUR",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_JPY[,2],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of JPY variance due to GBP",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))
  plot(1:tau, var_dec$ret_JPY[,1],                 type = "l", col  = "red",
       xlab = "",           ylab = "",             main = "% of JPY variance due to JPY",
       xlim = c(0, tau),    ylim = c(0, 1),
       xaxp = c(0, tau, 2), yaxp = c(0, 1, 2))

##############################
