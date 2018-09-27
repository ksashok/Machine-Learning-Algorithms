# auxiliary function to predict based on the estimated coefficients
predict_func <- function(Phi, w){
  return(Phi%*%w)
} 

# auxiliary function to calculate a cost function
error_func <- function (Phi, w, label){
  #Error Function. MSE
  return(mean((predict_func(Phi, w) - label)^2))
}


bgd <- function(train.data,train.label,test.data,test.label,tau.max,eta,epsilon)
{
  
  #Initializing the variables
  train.len <- nrow(train.data)
  
  error <- data.frame('tau'=1:tau.max)  # to be used to trace the test and training errors in each iteration
  Phi <- as.matrix(cbind('X0'=1, train.data))
  T <- train.label
  W <- matrix(,nrow=tau.max, ncol=ncol(Phi)) # be used to store the estimated oefficients
  W[1,] <- runif(ncol(Phi)) # initial weight
  tau <- 1 #Setting the counter to 1
  terminate <- FALSE
  
  while(!terminate){
    
    # check termination criteria:
    terminate <- tau >= tau.max-1 | error_func(Phi, W[tau,],T)<=epsilon
    
    #Finding the predicted value
    t_pred = predict_func(Phi, W[tau,]) 
    
    #Updating the weights with the L2 regression
    W[(tau+1),] <- W[tau,] - (eta * (1/nrow(Phi)) * (t(Phi)%*%(t_pred - T)))
    
    #Finding the train and test error
    error[tau, 'train'] <- error_func(as.matrix(cbind(1, train.data)), W[tau,],train.label)
    error[tau, 'test'] <- error_func(as.matrix(cbind(1, test.data)), W[tau,],test.label)
    
    #Incrementing the counter
    tau <- tau + 1
  }
  #Incrementing the counter
  return(error)
}


train <- mtcars[train_ind, ]
test <- mtcars[-train_ind, ]

#Splitting the columns into x and y for train and test
train.x <- train[,c(2,3,4)]
train.y <- train[,1,drop=FALSE]

test.x <- test[,c(2,3,4)]
test.y <- test[,1,drop=FALSE]

tau.max <- 18 * nrow(train.x) # maximum number of iterations
eta <- 0.01 # learning rate
epsilon <- 0.1 # a threshold on the cost (to terminate the process)
lambda <- 0.5 # Regression Cofficient

error <- bgd(train.x,train.y,test.x,test.y,tau.max,eta,epsilon)
