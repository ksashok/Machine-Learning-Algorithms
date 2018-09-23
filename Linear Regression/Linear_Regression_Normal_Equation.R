
#Linear Regression function using normal equation
linear_regression <- function(train.x,train.y){
  
  #Adding the column with value 1 for train.x[1]
  X <- as.matrix(cbind("X0"=1,train.x))
  Y <- as.matrix(train.y)
  
  #Finding the coefficients using normal equation
  theta <- solve(t(X) %*% X) %*% t(X) %*% Y
  return(theta)
  
}


#Training the model
train <- mtcars
train.x <- train[,c(2,3,4)]
train.y <- train[,1,drop=FALSE]

#Calling the function
theta <- linear_regression(train.x,train.y)

#Predicting
pred_y <- as.matrix(cbind("X0"=1,train.x))%*%theta 

#Finding the correlation between the predicted and the actual
cor(pred_y,train.y)^2
