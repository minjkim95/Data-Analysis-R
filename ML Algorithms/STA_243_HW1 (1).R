## STA 243 HW1 R Code
## Name: Min Kim, Kenneth Broadhead

install.packages("phangorn")
install.packages("MASS")

## 4. Implement Algorithm 2: Randomized Matrix Multiplication
## The function that takes matrices A,B and r columns to return 
## an approximation matrix M for C = AB.
# a)
RMM = function(A,B,r) {
  n = dim(A)[2]  # n is the 2nd dimension of matrix A, column of matrix A
  prob = rep(0,n) # list probabilities (created a list of n empty prob entries)
  for (i in 1:n) {
    prob[i] = norm(A[,i], "2")*norm(B[i,],"2") ## "2" L-2 Norm
  }
  prob = prob/(sum(prob)) ## Normalize prob
  
  S = 0
  
  for (i in 1:r) {
    l = sample(1:n, size= 1, replace = T, prob = prob)
    S = S + outer(A[,l], B[l,])/prob[l]
  }
  return(S/r) # S/r = M
   
}

# b) Apply the algorithm for r columns = 20,50,100,200
Mat_B = STA243_homework_1_matrix_B
Mat_A = STA243_homework_1_matrix_A
Mat_A = data.matrix(Mat_A)
Mat_B = data.matrix(Mat_B)

M_20 = RMM(Mat_A,Mat_B, 20)
M_50 = RMM(Mat_A,Mat_B, 50)
M_100 = RMM(Mat_A,Mat_B, 100)
M_200 = RMM(Mat_A,Mat_B, 200)

# c) Relative Approximation error for each of the estimates found in b). 
#    provide the result in table.
C = 0
for (i in 1:dim(Mat_A)[2]) {
    C = C + outer(Mat_A[,i],Mat_B[i,])
}

r = c(20,50,100,200)
for (i in r){
  M = RMM(Mat_A, Mat_B, i)
  error = norm(M-C ,"F")/(norm(Mat_A,"F")*norm(Mat_B, "F"))
  print(error)
}
# 0.2035108
# 0.142128
# 0.08237366
# 0.0575035

#d)
par(mfrow = c(2,2))
image(M_20, xlab = "Approximation with r = 20")
image(M_50, xlab = "Approximation with r = 50")
image(M_100, xlab = "Approximation with r = 100")
image(M_200,xlab = "Approximation with r = 200")
?image

##Question 5. Build power_interation function 
#  that takes in the matrix X and initial vector v0 and 
#  returns the eigenvector
power_iteration = function(A, v0, eps = 1e-6, maxiter=100) {
  v_int = v0
  for (i in 1:maxiter) {
    v_new = A%*%v_int
    if (sqrt(sum((abs(v_new)- abs(v0))^2)) <= eps) break  
    v_new = v_new/sqrt(sum(v_new^2))
    v_int = v_new
    if (i == maxiter) break 
  }
  return(v_new)
}

set.seed(5)
E = matrix(rnorm(100), 10, 10)
v = c(1, rep(0, 9))
lams = 1:10
v0 = rep(1,nrow(E))
prods = c()
for (lambda in lams) {
  X = lambda*outer(v, v) + E
  v0 = rep(1, nrow(E))
  v0 = v0/sqrt(sum(v0^2)) #normalized v0
  vv = power_iteration(X, v0)
  prods = c(prods, abs(v %*% vv))
}
par(mfrow = c(1,1))
plot(lams, prods, "b", xlab= "Lambda", ylab = "Correlation", main = "Correlation between 
     Estimated Eigenvector and True Eigenvector")

##Question 6
library(phangorn)
library(MASS)


##Function SKETCH Performs sketched OLS given a data 
##matrix 'X', response vectory 'y', and error parameter 'ep'
##Returns the coefficient vector if 'beta=TRUE' and returns 
##PHI*X and PHI*y as a single matrix otherwise. (note 'beta=TRUE' by default)
sketch<-function(X,y,ep, beta=TRUE)
{
n<-length(X[,1])
d<-length(X[1,])  
r<-d*log(n)/ep
  
##Compute DX and DY
DX<-matrix(,n,d)
DY<-matrix(,n,1)
for(i in 1:n){
  d_ii<-sample(c(-1,1),1)
  DX[i,]<-d_ii*X[i,]
  DY[i,1]<-d_ii*y[i]
}
  
##Compute HDX and HDY using 'fhm'
HDX<-matrix(,n,0)
HDY<-matrix(,n,1)
HDY<-fhm(DY[,1])
for(i in 1:d){
  HDX<-cbind(HDX, fhm(DX[,i]))
}
  
##Compute StHDX and StHDY
X_1<-matrix(, r,d)
Y_1<-matrix(, r,1)
nrm<-sqrt(n/r)
for(t in 1:r){
  i<-sample.int(n=n,size=1)
  X_1[t,]<-nrm*HDX[i,]
  Y_1[t,1]<-nrm*HDY[i] 
}
if(beta){
  return(solve(t(X_1)%*%X_1)%*%t(X_1)%*%Y_1)
}
  else{
  return(cbind(Y_1,X_1))
}
}


##OLS & Sketched OLS comparisons
set.seed(1313)
Y<-runif(2^20,0,1)
X<-matrix(runif(20*2^20,0,1),2^20,20)

##epsilon = 0.1
sk<-sketch(X,Y, ep=0.1, beta=FALSE)
Y_1<-sk[,1]
X_1<-sk[,-1]
system.time({beta_true<-solve(t(X)%*%X)%*%t(X)%*%Y})
system.time({beta_sk<-solve(t(X_1)%*%X_1)%*%t(X_1)%*%Y_1})

##epsilon = 0.05
sk<-sketch(X,Y, ep=0.05, beta=FALSE)
Y_1<-sk[,1]
X_1<-sk[,-1]
system.time({beta_true<-solve(t(X)%*%X)%*%t(X)%*%Y})
system.time({beta_sk<-solve(t(X_1)%*%X_1)%*%t(X_1)%*%Y_1})


##epsilon = 0.01
sk<-sketch(X,Y, ep=0.01, beta=FALSE)
Y_1<-sk[,1]
X_1<-sk[,-1]
system.time({beta_true<-solve(t(X)%*%X)%*%t(X)%*%Y})
system.time({beta_sk<-solve(t(X_1)%*%X_1)%*%t(X_1)%*%Y_1})

##epsilon = 0.001
sk<-sketch(X,Y, ep=0.001, beta=FALSE)
Y_1<-sk[,1]
X_1<-sk[,-1]
system.time({beta_true<-solve(t(X)%*%X)%*%t(X)%*%Y})
system.time({beta_sk<-solve(t(X_1)%*%X_1)%*%t(X_1)%*%Y_1})