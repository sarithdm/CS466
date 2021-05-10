#Listing 6.2 Plotting churn grouped by variable 218 levels
table218 <- table(
  Var218=dTrain[,'Var218'],
  churn=dTrain[,outcome],
  useNA='ifany')
print(table218)
#Listing 6.3 Churn rates grouped by variable 218 codes
print(table218[,2]/(table218[,1]+table218[,2]))
#Listing 6.4 Function to build single-variable models for categorical variables
mkPredC <- function(outCol,varCol,appCol) {
  pPos <- sum(outCol==pos)/length(outCol)
  naTab <- table(as.factor(outCol[is.na(varCol)]))
  pPosWna <- (naTab/sum(naTab))[pos]
  vTab <- table(as.factor(outCol),varCol)
  pPosWv <- (vTab[pos,]+1.0e-3*pPos)/(colSums(vTab)+1.0e-3)
  pred <- pPosWv[appCol]
  pred[is.na(appCol)] <- pPosWna
  pred[is.na(pred)] <- pPos
  pred
}
#Listing 6.5 Applying single-categorical variable models to all of our datasets
for(v in catVars) {
  pi <- paste('pred',v,sep='')
  dTrain[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dCal[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dCal[,v])
  dTest[,pi] <- mkPredC(dTrain[,outcome],dTrain[,v],dTest[,v])
}
#Listing 6.6 Scoring categorical variables by AUC
#library('ROCR')
calcAUC <- function(predcol,outcol) {
  perf <- performance(prediction(predcol,outcol==pos),'auc')
  as.numeric(perf@y.values)
}
 for(v in catVars) {
  pi <- paste('pred',v,sep='')
  aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
  if(aucTrain>=0.8) {
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                  pi,aucTrain,aucCal))
  }
 }
#Using numeric features
#Listing 6.7 Scoring numeric variables by AUC
mkPredN <- function(outCol,varCol,appCol) {
  cuts <- unique(as.numeric(quantile(varCol,
                                     probs=seq(0, 1, 0.1),na.rm=T)))
  varC <- cut(varCol,cuts)
  appC <- cut(appCol,cuts)
  mkPredC(outCol,varC,appC)
}
for(v in numericVars) {
  pi <- paste('pred',v,sep='')
  dTrain[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTrain[,v])
  dTest[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dTest[,v])
  dCal[,pi] <- mkPredN(dTrain[,outcome],dTrain[,v],dCal[,v])
  aucTrain <- calcAUC(dTrain[,pi],dTrain[,outcome])
  if(aucTrain>=0.55) {
    aucCal <- calcAUC(dCal[,pi],dCal[,outcome])
    print(sprintf("%s, trainAUC: %4.3f calibrationAUC: %4.3f",
                  pi,aucTrain,aucCal))
  }
}
#Listing 6.8 Plotting variable performance
library(ggplot2)
ggplot(data=dCal) +
  geom_density(aes(x=predVar126,color=as.factor(churn)))
#Using cross-validation to estimate effects of overfitting
#Listing 6.9 Running a repeated cross-validation experiment
var <- 'Var217'
aucs <- rep(0,100)
for(rep in 1:length(aucs)) {
  useForCalRep <- rbinom(n=dim(dTrainAll)[[1]],size=1,prob=0.1)>0
  predRep <- mkPredC(dTrainAll[!useForCalRep,outcome],
                     dTrainAll[!useForCalRep,var],
                     dTrainAll[useForCalRep,var])
  aucs[rep] <- calcAUC(predRep,dTrainAll[useForCalRep,outcome])
}
mean(aucs)
sd(aucs)

#Using linear regression
dtrain <- subset(psub,ORIGRANDGROUP >= 500)
dtest <- subset(psub,ORIGRANDGROUP < 500)
model <- lm(log(PINCP,base=10) ~ AGEP + SEX + COW + SCHL,data=dtrain)
dtest$predLogPINCP <- predict(model,newdata=dtest)
dtrain$predLogPINCP <- predict(model,newdata=dtrain)
model <- lm(log(PINCP,base=10) ~ AGEP + SEX + COW + SCHL, data=dtrain)
dtest$predLogPINCP <- predict(model,newdata=dtest)
#Using logistic regression
train <- sdata[sdata$ORIGRANDGROUP<=5,]
test <- sdata[sdata$ORIGRANDGROUP>5,]
complications <- c("ULD_MECO","ULD_PRECIP","ULD_BREECH")
riskfactors <- c("URF_DIAB", "URF_CHYPER", "URF_PHYPER",
                 "URF_ECLAM")
y <- "atRisk"
x <- c("PWGT",
       "UPREVIS",
       "CIG_REC",
       "GESTREC3",
       "DPLURAL",
       complications,
       riskfactors)
fmla <- paste(y, paste(x, collapse="+"), sep="~")
print(fmla)
#Cluster analysis
#Listing 8.1 Reading the protein data
protein <- read.delim("~/GitHub/cs466/Module2/data/protein.txt")
View(protein)
summary(protein)
#Listing 8.2 Rescaling the dataset
vars.to.use <- colnames(protein)[-1]
pmatrix <- scale(protein[,vars.to.use])
pcenter <- attr(pmatrix, "scaled:center")
pscale <- attr(pmatrix, "scaled:scale")
#Listing 8.3 Hierarchical clustering
d <- dist(pmatrix, method="euclidean")
pfit <- hclust(d, method="ward")
plot(pfit, labels=protein$Country)
rect.hclust(pfit, k=5)
#Listing 8.4 Extracting the clusters found by hclust()
groups <- cutree(pfit, k=5)
print_clusters <- function(labels, k) {
  for(i in 1:k) {
    print(paste("cluster", i))
    print(protein[labels==i,c("Country","RedMeat","Fish","Fr.Veg")])
  }
}
print_clusters(groups, 5)

#Listing 8.5 Projecting the clusters on the first two principal components
library(ggplot2)
princ <- prcomp(pmatrix)
nComp <- 2
project <- predict(princ, newdata=pmatrix)[,1:nComp]
project.plus <- cbind(as.data.frame(project),
                      cluster=as.factor(groups),
                      country=protein$Country)
ggplot(project.plus, aes(x=PC1, y=PC2)) +
  geom_point(aes(shape=cluster)) +
  geom_text(aes(label=country),
            hjust=0, vjust=1)

#library(fpc)
#The k-means algorithm
kbest.p<-5
pclusters <- kmeans(pmatrix, kbest.p, nstart=100, iter.max=100)
summary(pclusters)
pclusters$centers
groups <- pclusters$cluster
print_clusters(groups, kbest.p)
