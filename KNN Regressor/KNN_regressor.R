#KNN Regressor Function
knn <- function(train.data, train.label, test.data, K){
  
  train.len <- nrow(train.data)
  test.len <- nrow(test.data)
  pred.label <- data.frame()
  dist <- as.matrix(dist(rbind(test.data, train.data), method= 'euclidean'))[1:test.len, (test.len+1):(test.len+train.len)]
  
  for (i in 1:test.len){
    ### ...find its K nearest neighbours from training sampels...
    nn <- as.data.frame(sort(dist[i,], index.return = TRUE))[1:K,2]
    
    ###... and calculate the predicted labels according to the majority vote
    pred.label[i,1]<- mean(train.label[nn,])
  }
  return(pred.label)
}

