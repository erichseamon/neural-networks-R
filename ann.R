
set.seed(500)
cv.error <- NULL
k <- 10

library(MASS)
data <- Boston

apply(data,2,function(x) sum(is.na(x)))

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

library(neuralnet)
library(plyr) 
pbar <- create_progress_bar('text')
pbar$init(k)

f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))

for(i in 1:k){
  
  index <- sample(1:nrow(data),round(0.9*nrow(data)))
  train.cv <- scaled[index,]
  test.cv <- scaled[-index,]
  
  lm.fit <- glm(medv~., data=train.cv)
  summary(lm.fit)
  pr.lm <- predict(lm.fit,test.cv)
  MSE.lm <- sum((pr.lm - test.cv$medv)^2)/nrow(test.cv)
  
  nn <- neuralnet(f,data=train.cv,hidden=c(5,2),linear.output=T)
  
  pr.nn <- compute(nn,test.cv[,1:13])
  pr.nn_ <- pr.nn$net.result*(max(scaled$medv)-min(scaled$medv))+min(scaled$medv)
  
  test.cv.r <- (test.cv$medv)*(max(scaled$medv)-min(scaled$medv))+min(scaled$medv)
  
  cv.error[i] <- sum((test.cv.r - pr.nn_)^2)/nrow(test.cv)
  
  pbar$step()
}

#--------Plotting
  
par(mfrow=c(1,1))

  
plot(test.cv$medv,pr.lm,col='blue', pch=18,cex=0.7)
#abline(0,1,lwd=2)
#par(new=TRUE)
plot(test.cv$medv,pr.nn_, col='red', main='Real vs predicted NN', pch=18,cex=0.7)

abline(0,1,lwd=2)
legend('bottomright',legend=c('NN','LM'),pch=18,col=c('red','blue'))




par(mfrow=c(1,2))

plot(test.cv$medv,pr.nn_,col='red',main='Real vs predicted NN',pch=18,cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='NN',pch=18,col='red', bty='n')


plot(test.cv$medv,pr.lm,col='blue',main='Real vs predicted lm',pch=18, cex=0.7)
abline(0,1,lwd=2)
legend('bottomright',legend='LM',pch=18,col='blue', bty='n', cex=.95)
