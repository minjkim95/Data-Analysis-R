# STA 243 HW2


## 2.
# a) Build a linear model on the training data using lm() by regessing the housing price on
# these variables: bedrooms, bathrooms, sqft living, and sqft lot. What's the R2 of
# the model on training data? What's the R2 on testing data.

# Housing price data
housingprice = read.csv("C:/Users/minjk/Downloads/housingprice.csv", header = FALSE)
names(housingprice) = c("id","date","price","bedrooms","bathrooms","sqft_living","sqft_lot","floors","waterfront","view","condition","grade", "sqft_above",	"sqft_basement","yr_built","yr_renovated","zipcode","lat","long",	"sqft_living15","sqft_lot15")
housingprice = housingprice[-1,]

# Train data
train = read.csv("C:/Users/minjk/Downloads/train.data.csv")
# Linear Model on train data with
# bedrooms, bathrooms, sqft living, and sqft lot
price_model_train = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot, data = train)
summary(price_model_train)

# R^2 = 0.5101

# Test data
test = read.csv("C:/Users/minjk/Downloads/test.data.csv")
View(test)
price_model_test = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot,data = test)
summary(price_model_test)

#R^2 = 0.5054

# b) Predict price of Bill Gates' house
fancyhouse = read.csv("C:/Users/minjk/Downloads/fancyhouse.csv")
View(fancyhouse)
summary(price_model_train)
predict(price_model_train, fancyhouse)
?predict

# c) Add bedrooms*bathrooms 

price_model_train1 = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+bedrooms*bathrooms, data = train)
summary(price_model_train1)
# R^2 = 0.5174
price_model_test1 = lm(price~bedrooms+bathrooms+sqft_living+sqft_lot+bedrooms*bathrooms, data = test)
summary(price_model_test1)
# R^2 = 0.5111

View(test)
# d) Perform all thing above using gradient descent algorithm
## Gradient Descent 

GD <- function(X, y, b0=rep(0, ncol(X)), step_size=1, tol=10^-4, max_iters=1000){
  
  step_size <- 2*step_size # Account for factor of 2
  
  sol_iters <- matrix(NA, max_iters+1, length(b0)) # Track position and objective value
  obj_iters <- numeric(max_iters+1)
  
  sol_iters[1, ] <- b0 # Set initial vector
  res <- as.numeric(X%*%b0 - y) # Run one step from initial position
  grad <- as.numeric(res%*%X)
  obj_iters[1] <- sum(res^2)
  
  for(iter in 2:(max_iters+1)){
    
    sol_iters[iter, ] <- sol_iters[iter-1, ] - step_size*grad # Gradient descent step
    
    res <- as.numeric(X%*%sol_iters[iter, ] - y)
    obj_iters[iter] <- sum(res^2)
    
    if(sqrt(sum(grad^2)) < tol){ # If at tolerance for the gradient norm, break the loop
      sol_iters <- sol_iters[1:iter, ]
      obj_iters <- obj_iters[1:iter]
      break
    }
    if(iter == max_iters+1){
      warning('Tolerance not reached before maximum iterations.')
    }
    
    grad <- as.numeric(res%*%X)
    iter <- iter+1
  }
  
  return(list( sol=sol_iters[nrow(sol_iters), ], obj=obj_iters[length(obj_iters)], sol_iters=sol_iters, obj_iters=obj_iters ))
}


#No interaction

# Training Dataset
X = as.matrix(data.frame(rep(1,length(train$price)),train[,5:8]))
col_means <- colMeans(X)
col_sds <- apply(X, 2, sd)
X <- sweep(sweep(X, 2, col_means, '-'), 2, col_sds, '/') # Standardize columns
X[, 1] <- 1 

y = as.matrix(data.frame(train$price))
model1_gd = GD(X,y,b0=rep(0, ncol(X)), step_size= 10^-5, tol=10^-6, max_iters=1000)
est_train <- as.numeric(X%*%model1_gd$sol) # estimated housing price for training data
model1_gd$sol

View(model1_gd)
# Calculate r-squared
ssr_train<- sum((y-est_train)^2) 
ssto_train <- sum((y- mean(y))^2)
r2_train <- 1 - ssr_train/ssto_train
r2_train

############################
### Testing Dataset
X1 = as.matrix(data.frame(rep(1,length(test$price)),test[,5:8]))
col_means1 <- colMeans(X1)
col_sds1 <- apply(X1, 2, sd)
X1 <- sweep(sweep(X1, 2, col_means1, '-'), 2, col_sds1, '/') # Standardize columns
X1[, 1] <- 1 

y1 = as.matrix(data.frame(test$price))
model_gd = GD(X1,y1,b0=rep(0, ncol(X1)), step_size= 10^-5, tol=10^-6, max_iters=1000)
est_test <- as.numeric(X1%*%model_gd$sol)
View(model_gd)
ssr_test<- sum((y1-est_test)^2) # Calculate r-squared
ssto_test <- sum((y1- mean(y1))^2)
r2_test <- 1 - ssr_test/ssto_test
r2_test

# With interaction
############# Training
X_int = as.matrix(data.frame(rep(1,length(train$price)),train[,5:8],train[5]*train[6]))
col_means_int <- colMeans(X_int)
col_sds_int <- apply(X_int, 2, sd)
X_int <- sweep(sweep(X_int, 2, col_means_int, '-'), 2, col_sds_int, '/') # Standardize columns
X_int[, 1] <- 1 
y_int = as.matrix(data.frame(train$price))
model_int_gd = GD(X_int,y_int,b0=rep(0, ncol(X_int)), step_size= 10^-5, tol=10^-6, max_iters=1000)
est_train_int <- as.numeric(X_int%*%model_int_gd$sol)
View(est_train_int)

ssr_train1 <- sum((y_int-est_train_int)^2) # Calculate r-squared
ssto_train1 <- sum((y_int- mean(y_int))^2)
r2_train1 <- 1 - ssr_train1/ssto_train1
r2_train1

############# Testing
X_int1 = as.matrix(data.frame(rep(1,length(test$price)),test[,5:8],test[5]*test[6]))
col_means_int1 <- colMeans(X_int1)
col_sds_int1 <- apply(X_int1, 2, sd)
X_int1 <- sweep(sweep(X_int1, 2, col_means_int1, '-'), 2, col_sds_int1, '/') # Standardize columns
X_int1[, 1] <- 1 
y_int1 = as.matrix(data.frame(test$price))
model_int1_gd = GD(X_int1,y_int1,b0=rep(0, ncol(X_int1)), step_size= 10^-5, tol=10^-6, max_iters=1000)
est_test_int <- as.numeric(X_int1%*%model_int1_gd$sol)
model_int1_gd$sol

ssr_test1<- sum((y_int1-est_test_int)^2) # Calculate r-squared
ssto_test1 <- sum((y_int1- mean(y_int1))^2)
r2_test1 <- 1 - ssr_test1/ssto_test1
r2_test1
# e) Perform all things above using stochastic gradient descent algorithm
#    one sample in each iteration 

## SGD

SGD <- function(X, y, b0=rep(0, ncol(X)), batch_size=1, replace=TRUE, C=1, max_iters=1000){
  
  C <- C*2*length(y)/batch_size # Account for minibatch size
  n <- length(y)
  
  sol_iters <- matrix(NA, max_iters+1, length(b0))
  obj_iters <- numeric(max_iters+1)
  
  sol_iters[1, ] <- b0 # Set initial vector
  res <- as.numeric(X%*%b0 - y)
  obj_iters[1] <- sum(res^2)
  
  samp <- sample(n)
  
  for(iter in 2:(max_iters+1)){
    
    if(replace == TRUE){
      ind <- sample(n, batch_size, replace=TRUE)
    }
    if(replace == FALSE){
      lower <- ((iter-2)*batch_size+1) %% n + 1
      upper <- ((iter-1)*batch_size) %% n + 1
      
      if(upper < lower){
        ind[(upper+1):batch_size] <- samp[lower:n]
        samp <- sample(n)
        ind[1:upper] <- samp[1:upper]
      } else {
        ind <- samp[lower:upper]
      }
    }
    
    ind <- sample(n, batch_size, replace=TRUE)
    stoch_grad <- as.numeric(X[ind, ]%*%sol_iters[iter-1, ]-y[ind])%*%X[ind, ]
    
    sol_iters[iter, ] <- sol_iters[iter-1, ] - C/iter*stoch_grad # Stochastic gradient descent step
    
    res <- as.numeric(X%*%sol_iters[iter, ] - y)
    obj_iters[iter] <- sum(res^2)
  }
  
  return(list( sol=sol_iters[nrow(sol_iters), ], obj=obj_iters[length(obj_iters)], sol_iters=sol_iters, obj_iters=obj_iters ))
}

# No interaction
# Training Dataset
X = as.matrix(data.frame(rep(1,length(train$price)),train[,5:8]))
col_means <- colMeans(X)
col_sds <- apply(X, 2, sd)
X <- sweep(sweep(X, 2, col_means, '-'), 2, col_sds, '/') # Standardize columns
X[, 1] <- 1 

y = as.matrix(data.frame(train$price))
model1_sgd = SGD(X, y, b0=rep(0, ncol(X)), C=10^-5, replace=FALSE, max_iters=10^6)
View(model1_sgd)
model1_sgd$sol
est_train_sgd <- as.numeric(X%*%model1_sgd$sol) # estimated housing price for training data

# Calculate r-squared
ssr_train_sgd<- sum((y-est_train_sgd)^2) 
ssto_train_sgd <- sum((y- mean(y))^2)
r2_train_sgd <- 1 - ssr_train_sgd/ssto_train_sgd
r2_train_sgd

############################
### Testing Dataset
X1 = as.matrix(data.frame(rep(1,length(test$price)),test[,5:8]))
col_means1 <- colMeans(X1)
col_sds1 <- apply(X1, 2, sd)
X1 <- sweep(sweep(X1, 2, col_means1, '-'), 2, col_sds1, '/') # Standardize columns
X1[, 1] <- 1 

y1 = as.matrix(data.frame(test$price))
model_sgd = SGD(X1,y1,b0=rep(0, ncol(X1)), C=10^-5, replace=FALSE, max_iters=10^6)
model_sgd$sol
est_test_sgd <- as.numeric(X1%*%model_sgd$sol)
View(model_gd)
ssr_test_sgd<- sum((y1-est_test_sgd)^2) # Calculate r-squared
ssto_test_sgd <- sum((y1- mean(y1))^2)
r2_test_sgd <- 1 - ssr_test_sgd/ssto_test_sgd
r2_test_sgd

# With interaction
############# Training
X_int = as.matrix(data.frame(rep(1,length(train$price)),train[,5:8],train[5]*train[6]))
col_means_int <- colMeans(X_int)
col_sds_int <- apply(X_int, 2, sd)
X_int <- sweep(sweep(X_int, 2, col_means_int, '-'), 2, col_sds_int, '/') # Standardize columns
X_int[, 1] <- 1 
y_int = as.matrix(data.frame(train$price))
model_int_sgd = SGD(X_int,y_int,b0=rep(0, ncol(X_int)), C=10^-5, replace=FALSE, max_iters=10^6)
model_int_sgd$sol
est_train_int_sgd <- as.numeric(X_int%*%model_int_sgd$sol)

ssr_train1_sgd <- sum((y_int-est_train_int_sgd)^2) # Calculate r-squared
ssto_train1_sgd <- sum((y_int- mean(y_int))^2)
r2_train1_sgd <- 1 - ssr_train1_sgd/ssto_train1_sgd
r2_train1_sgd

############# Testing
X_int1 = as.matrix(data.frame(rep(1,length(test$price)),test[,5:8],test[5]*test[6]))
col_means_int1 <- colMeans(X_int1)
col_sds_int1 <- apply(X_int1, 2, sd)
X_int1 <- sweep(sweep(X_int1, 2, col_means_int1, '-'), 2, col_sds_int1, '/') # Standardize columns
X_int1[, 1] <- 1 
y_int1 = as.matrix(data.frame(test$price))
model_int1_sgd = SGD(X_int1,y_int1,b0=rep(0, ncol(X_int1)), C=10^-5, replace=FALSE, max_iters=10^6)

est_test_int_sgd <- as.numeric(X_int1%*%model_int1_sgd$sol)
model_int1_sgd$sol

ssr_test1_sgd<- sum((y_int1-est_test_int_sgd)^2) # Calculate r-squared
ssto_test1_sgd <- sum((y_int1- mean(y_int1))^2)
r2_test1_sgd <- 1 - ssr_test1_sgd/ssto_test1_sgd
r2_test1_sgd

