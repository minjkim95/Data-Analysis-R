#salmon data

library(rrcov)
par(mfrow=c(1,1), mar=c(4,4,2,1))
data(salmon)
View(salmon)
plot(salmon$Freshwater,salmon$Marine,xlab="Freshwater",ylab="Marine",
     pch=rep(c(18,20),each=50),col=rep(c(2,4),each=50),main="")
legend("topright",legend=c("Alaskan","Canadian"),pch=c(18,20),col=c(2,4),cex=1)

# Method 1

x1 <- salmon[1:50,2:3]
View(x1)
x2 <- salmon[51:100,2:3]
View(x2)
# compute sample mean vectors:
x1.mean <- colMeans(x1)
x2.mean <- colMeans(x2)


# compute pooled estimate for the covariance matrix:
S.u <- 49*(var(x1)+var(x2))/98
w <- solve(S.u)%*%(x1.mean-x2.mean)
w0 <- -(x1.mean+x2.mean)%*%w/2
lines(salmon[,2],-(w[1]*salmon[,2]+w0)/w[2])
View(salmon[,2]) #Freshwater
w[1]
## Method 2: use function LDA in MASS package:

library(MASS)
lda.obj <- lda(Origin~Freshwater+Marine,data=salmon,prior=c(1,1)/2)
lda.obj$scaling
plda <- predict(object=lda.obj,newdata=salmon)

# Confusion matrix
table(salmon[,4],plda$class)

#plot the decision line
gmean <- lda.obj$prior %*% lda.obj$means
const <- as.numeric(gmean %*%lda.obj$scaling)
slope <- - lda.obj$scaling[1] / lda.obj$scaling[2]
intercept <- const / lda.obj$scaling[2]

#Plot decision boundary
plot(salmon[,2:3],pch=rep(c(18,20),each=50),col=rep(c(2,4),each=50))
abline(intercept, slope)
legend("topright",legend=c("Alaskan","Canadian"),pch=c(18,20),col=c(2,4))




## IRIS DATA
data(iris)
View(iris)
table(iris$Species)
