
# One Hot Encoding function which takes a factor column and converts it into numerical values
oneHotEncoding <- function(column){
  
  num_df <- data.frame(matrix(nrow=length(column)))
  for(value in unique(column)){
    num_df[,value] <- ifelse(column==value,1,0)
  }
  
  return(num_df[,-1])
}

#Testing the Function
titanic  <- data.frame(Titanic)

#Passing the column needed to be converted into one hot coding
one_hot_df <- oneHotEncoding(titanic$Class)

#Removing the column from the data frame
titanic$Class <- NULL

#Combining the dataframe with one hot encoded column
titanic <- cbind(titanic,one_hot_df)

#This can be extended by calling all columns
