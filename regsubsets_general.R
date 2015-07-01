
####################################   chargement des données   #############################################

setwd("C:/Users/Catherine/Desktop/R/regsubsets")
getwd()

#loading xlsx package to read excel files
library(xlsx) #needs Java, the Rjava package and the xlsxjars package
#loading leaps package to load regsubsets function
library(leaps)


source("compute_RMSFE.R")


nrow = 102 #number of rows in excel file, minus the header row
df <- read.xlsx("data_PIB_insee.xlsx", sheetIndex = "inputcodesas", endRow = nrow)
#dim(mydata)

#######################   manipulating dataset (removing NAs and adding lagged values)   #####################

#removing the crisis observations
#df[df$date == as.Date("2008-10-01") | df$date == as.Date("2009-01-01"),"pib"] <- NA

#identifying months for each variable
m1variables <- grep("_m1$",names(df))
m2variables <- grep("_m2$",names(df))
m3variables <- grep("_m3$",names(df))

#charging an appropriate lag function for data frames
lagpad <- function(x, k) {
    if (!is.vector(x)) 
        stop('x must be a vector')
    if (!is.numeric(x))
        stop('x must be numeric')
    if (!is.numeric(k))
        stop('k must be numeric')
    if (1 != length(k))
        stop('k must be a single number')
    c(rep(NA, k), x)[1 : length(x)] 
}


# adding lagging values for m3 variables into df1
df1 <- df

for (i in m3variables) {
  df1 <- cbind(df1,lagpad(df[,i],1))
}

m3variablesname <- names(df[, m3variables])
m3variableslagname <- gsub("_m3","_m3_1",m3variablesname)
names(df1) <- c(names(df),m3variableslagname)

# adding lagging values for m2 variables into df2
df2 <- df1

for (i in m2variables) {
  df2 <- cbind(df2,lagpad(df1[,i],1))
}

m2variablesname <- names(df[, m2variables])
m2variableslagname <- gsub("_m2","_m2_1",m2variablesname)
names(df2) <- c(names(df1),m2variableslagname)


#removing m3 variables (not lagged) from df2
df3 <- df2[,-grep("_m3$",names(df2))]
#taking out : 1) first row which has NAs for lagged variables 2) rows for which endogenous variable is NA
df4 <- df3[-1,]
df5 <- df4[!is.na(df4$pib),]

#creating table for estimation and removing dates variables
df.est <- df5[,-1]

#creating table for forecast
df.prev <- df4[is.na(df4$pib),]
#assigning a value for model.matrix step
df.prev$pib <- 9999

#creating matrices equivalent to those data frame 
matrix.est = model.matrix(pib ~ ., data = df.est)
matrix.prev = model.matrix(pib ~ ., data = df.prev)

#and cleaning a bit
rm(list = c("nrow","df1","df2","df3","df4","df5","m1variables","m2variables","m3variables",
            "m2variablesname","m3variablesname","m2variableslagname","m3variableslagname"))

###################################################################################################
########################################## REGSUBSETS PROCEDURE ############################################
###################################################################################################



max_numvariables <- 6
sub_models <- 3
total_models <- max_numvariables * sub_models

train = seq(1:(round(nrow(df.est)/2)))
#échantillon de réestimation
train2000=df.est[c(43:73,76:nrow(df.est)-8),]

crise=(74:75)

regfit.m2 = regsubsets(pib ~ ., 
                       data = df.est, 
                       subset = train, 
                       nvmax = max_numvariables, 
                       nbest = sub_models, 
                       method = "exhaustive", 
                       intercept = TRUE,
                       really.big=TRUE)
summary(regfit.m2)

RMSFE <- compute_RMSFE(total_models, regfit.m2, train, df.est)


#vector containing the RMSE for each model on validation set
val.errors = rep(NA,total_models)
#vector containing the predictions for  each model on total set
all.pred.total = NULL
#vector containing the forecasts for each model
all.pred.prev = NULL

for (i in 1:total_models) {
    
    #retrieving coefficients for each model
    coefi = coef(regfit.m2, id=i)
        
    
    #prediction for each model on testing set and calculating RMSE on testing set
    pred.test = matrix.est[-c(train,crise,(nrow(df.est)-8):nrow(df.est)),names(coefi)]%*%coefi
    #calculating the RMSFE for each model
    val.errors[i]=mean(sqrt((df.est$pib[-c(train,crise,(nrow(df.est)-8):nrow(df.est))]-pred.test)^2))
    
    
    
    #estimating coefficients on echantillon de reestimation
    if (length(intersect(names(coefi), "(Intercept)")) == 1) {
        coefi.witht.intercept <- names(coefi)[2:length(names(coefi))]
        reg.coefi <- paste(coefi.witht.intercept, collapse = " + ")
    }
    formul <- paste("pib",reg.coefi, sep = " ~ ")
    coefi.total <- lm(formul, data = train2000)$coefficients
    #creating a vector containing all the predictions on total data
    pred.total = matrix.est[,names(coefi)]%*%coefi.total
    all.pred.total = cbind(all.pred.total, pred.total)
    #creating a vector containing forecasts 
    pred.prev = matrix.prev[,names(coefi)]%*%coefi.total
    all.pred.prev = cbind(all.pred.prev, pred.prev)
}


#identification of the n models with the smallest RMSE on validation set
n=30

#vector containing the n smallest RMSE on validation set
mymins=NULL
#vector which will first contain all the RMSE errors, and will then exclude one by one the smallest RMSFE
val.errors.i=val.errors

for (i in 1:n){
    newmin.pos = which.min(val.errors.i)
    find1 = val.errors == val.errors.i[newmin.pos]
    # j'aurais pu faire un which(find1)
    find2 = ifelse(find1==TRUE,-1,0)
    found = which.min(find2)
    mymins = c(mymins,found)
    val.errors.i = val.errors[-mymins]
}

#plot of the MSE on validation set and models selected
plot(val.errors, xlab="modèles", ylab="RMSE sur échantillon de validation")
points(mymins, val.errors[mymins], col="red", cex=2, pch=20)

#print all n best models
noms.resultats<-matrix(nrow=n+1, ncol=6)
#resultats<-matrix(nrow=n, ncol=7)
for (i in 1:n){
  	print(coef(regfit.m2,id=mymins[i]))
	noms.resultats[i,1:(length(names(coef(regfit.m2,id=mymins[i])))-1)]<-names(coef(regfit.m2,id=mymins[i]))[2:length(names(coef(regfit.m2,id=mymins[i])))]
}
noms.resultats[n+1,1]<-paste("Moyenne des",n,"meilleurs modèles")

best.forecasts <- all.pred.prev[,mymins]
mean.forecast <- mean(best.forecasts)
mean.forecast

best.preds <- all.pred.total[,mymins]
mean.preds <- rowMeans(best.preds)
RMSE <- mean(sqrt((df.est$pib-mean.preds)^2))
RMSE

RMSE.test <- mean(sqrt((df.est$pib[-train]-mean.preds[-train])^2))
RMSE.test

RMSE_test_n<-t(cbind(t(colMeans(sqrt((df.est$pib[-train]-best.preds[-train,])^2))),RMSE.test))
RMSE_n<-t(cbind(t(colMeans(sqrt((df.est$pib-best.preds)^2))),RMSE))

nono<-c("var1","var2","var3","var4","var5","var6","RMSE","2014T1","2014T2","2014T3","2014T4","2015T1","2015T2")
toto<-data.frame(noms.resultats,round(RMSE_n,2),round(t(cbind(best.preds[(length(best.preds[,1])-4):length(best.preds[,1]),],mean.preds[(length(best.preds[,1])-4):length(best.preds[,1])])),2),t(round(cbind(t(best.forecasts),mean.forecast),2)))
names(toto)<-nono

pib<-ts(t(cbind(t(df.est$pib),NA)),start=1992.5,frequency=4)
prev<-ts(t(cbind(t(mean.preds),mean.forecast)),start=1992.5,frequency=4)
plot(pib, type = "l",main="Prévisions du PIB avec 30 meilleurs modèles sur enquêtes Insee",ylab="variations trimestrielles")
lines(prev,col="red")
legend("bottom",c("PIB","Prévisions"),col=c("black","red"),lty=1,horiz="TRUE")



