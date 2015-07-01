
compute_RMSFE <- function(total_models, regfit.m2, train, df.est) {
  

total_models2 <- as.character(total_models)
total_digits <- length(strsplit(total_models2, "")[[1]])
my.models <- list()

val.errors <- matrix(data = NA, nrow = (nrow(df.est) - length(train) + 1), ncol = total_models)


for (i in 1:total_models) {
  
  i_bis <- as.character(i)
  total_digits_i <- length(strsplit(i_bis, "")[[1]])
  empty_digits <- as.numeric(total_digits) - as.numeric(total_digits_i)
  
  my.models <- c(my.models,
                 list(
                   assign(paste("coeff_", rep(0,empty_digits), i, sep=""), coef(regfit.m2, id=i))
                   )
                 )
  
}


for (k in length(train):nrow(df.est)) {
  for (i in 1:length(my.models)) {
      
    regressors <- paste(names(my.models[[i]])[2:length(names(my.models[[i]]))], collapse =" + ")
    my.formula <- as.formula(paste("pib ~ ", regressors))
    
    regfit.i <- lm(my.formula, data = df.est, subset = seq(1:(k-1)))
    
    val.errors[k + 1 -length(train), i] <- (df.est$pib[k] - predict.lm(regfit.i, df.est[k, ]))^2
  }
}

RMSFE <- sqrt(colMeans(val.errors))

return(RMSFE)

#plot(RMSFE)

scatter.smooth(x = 1:length(RMSFE),
               y = RMSFE, 
               ylim=c(0.2,0.6),
               col="#CCCCCC")

#loess_fit <- loess(RMSFE ~ seq(1:length(RMSFE)))
#lines(predict(loess_fit), col = "blue")

}

