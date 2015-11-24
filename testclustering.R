#Check the number of likely clusters in the data:
names(loaddata)
data <- loaddata[,c(3:6,10:13)]
data <- na.omit(data)
data <- scale(data)

wss <- (nrow(data)-1)*sum(apply(data,2,var))
twss <- 1
bwss <- 1
for (i in 2:15){
    k <- kmeans(data, 
                centers=i)
    twss[i] <- k$totss
    bwss[i] <- k$betweenss
    wss[i] <- sum(k$withinss)  
} 
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
lines(1:15, twss, type="l", col="red")

library(cluster)
library(fpc)
asw <- numeric(20)
for (k in 2:20)
    asw[[k]] <- pam(data, k) $ silinfo $ avg.width
k.best <- which.max(asw) # 2!
cat("silhouette-optimal number of clusters:", k.best, "\n")
gskmn <- clusGap(data, FUN = kmeans, nstart = 20, K.max = 15, B = 60)
k <- maxSE(gskmn$Tab[, "gap"], gskmn$Tab[, "SE.sim"], method="Tibs2001SEmax")
plot(gskmn) # This shows some kind of elbow around 8, although we should be looking for a peak!

# How many clusters if we have only the 4 eyetracking ones?
data <- loaddata[,c(3:6)]
data <- na.omit(data)
data <- scale(data)

library(cluster)
library(fpc)
asw <- numeric(20)
for (k in 2:20)
    asw[[k]] <- pam(data, k) $ silinfo $ avg.width
k.best <- which.max(asw) # 2!
gskmn <- clusGap(data, FUN = kmeans, nstart = 20, K.max = 15, B = 60)
k <- maxSE(gskmn$Tab[, "gap"], gskmn$Tab[, "SE.sim"], method="Tibs2001SEmax")
plot(gskmn) # gives 1, but the next first peak is 5


library(depmixS4)

states <- 2:20

logLiks <- -20000
BICs <- 40000
AICs <- 40000

for(numstates in states){
print(paste(numstates,"..."))
# Overall
set.seed(1)
#set up the HMM model
mod <- depmix(list(value.Mean~1,value.SD~1,value.Sac~1,value.Fix~1,value.Jerk.Mean~1,value.Jerk.SD~1,value.Theta~1,value.Attention~1), data=loaddata, nstates=numstates, family=list(gaussian(),gaussian(),gaussian(),poisson(),gaussian(),gaussian(),gaussian(),gaussian()),ntimes=summary(as.factor(loaddata$session)), verbose=F)
#fit the model 
f <- fit(mod, verbose=F)
print(f)
logLiks[numstates] <- logLik(f)
BICs[numstates] <- BIC(f)
AICs[numstates] <- AIC(f)
}

qplot(1:20,logLiks,geom="line")
qplot(1:20,BICs,geom="line") # 8 is not the minimum, but it is the smallest k near that one
qplot(1:20,AICs,geom="line")




states <- 2:16

logLiks[1] <- 0
BICs[1] <- 0
AICs[1] <- 0

for(i in states){
    print(paste(numstates,"..."))
    # Overall
    set.seed(1)
    #set up the HMM model
    mod <- depmix(list(value.Mean~1,value.SD~1,value.Sac~1,value.Fix~1), data=loaddata, nstates=i, family=list(gaussian(),gaussian(),gaussian(),poisson()),ntimes=summary(as.factor(loaddata$session)), verbose=F)
    #fit the model 
    f <- fit(mod, verbose=F)
    print(f)
    logLiks[i] <- logLik(f)
    BICs[i] <- BIC(f)
    AICs[i] <- AIC(f)
}

qplot(1:16,logLiks,geom="line")
qplot(1:16,BICs,geom="line") # This points to 6 as the 1st small number close to the minimum
qplot(1:16,AICs,geom="line") # This still decreases after 16!

