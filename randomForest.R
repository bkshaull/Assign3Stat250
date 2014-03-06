library(randomForest)
y=load("/Users/brittneyshaull/Documents/UCDavis/Stat250/Assign3/digitsTrain.rda")
train=sampleTrain
ytest=load("/Users/brittneyshaull/Documents/UCDavis/Stat250/Assign3/digitsTest.rda")
test=newTest

###Want to get rid of all of the pixels that don't have any variation
i=1
while (i<=ncol(train)) {
	if (sum(train[ , i])==0) {
		train[ ,i]=NULL
		i=i-1
		}
		i=i+1
}
test=subset(test, select=names(train))

ytrain=train[ ,1]
xtrain=train[ ,2:ncol(train)]
ytest=test[ ,1]
xtest=test[ ,2:ncol(test)]
ytrain=as.factor(ytrain)
ytest=as.factor(ytest)
rf=randomForest(x=xtrain, y=ytrain, xtest=xtest, ytest=ytest, ntree=100)
