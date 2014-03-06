library(MASS)
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

ytrain=train[ ,1]
xtrain=train[ ,2:ncol(train)]
ytest=test[ ,1]
xtest=test[ ,2:ncol(test)]

fx=as.formula(paste("label~ ", paste(names(xtrain), collapse="+")))


LD=lda(fx, train)
test=subset(test, select=names(train))
predict=predict(LD, test)$class
actual=as.integer(test$label)
predict=as.integer(predict)
error=predict-actual
errorp=which(error==0)
errorpercentLDA=(nrow(test)-length(errorp))/nrow(test)


corr=cor(ytrain, xtrain)
corr=abs(corr)
f=which(corr>=.20)
xtrain=xtrain[f]
sub=c("label", names(xtrain))
train=subset(train, select=sub)

fx=as.formula(paste("label~ ", paste(names(xtrain), collapse="+")))

QD=qda(fx,train)

test=subset(test, select=names(train))
predict=predict(QD, test)$class
actual=as.integer(test$label)
predict=as.integer(predict)
error=predict-actual
errorp=which(error==0)
errorpercentQDA=(nrow(test)-length(errorp))/nrow(test)
