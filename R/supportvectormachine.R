supportvectormachine <- function(data, formula, formula2, k, var_x, value1, var_y, value2){
  library(jsonlite)
  data < toJSON(data)
  data <- fromJSON(data)
  library(e1071)
  mymodel <- svm(formula, data=data, kernel=k)
  list2 <-list()
  list2$var_x <- value1
  list2$var_y <- value2
  names(list2) <- c(var_x, var_y)
  plot(mymodel, data, formula2, slice=list2)
}


