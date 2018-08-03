supportvectormachine <- function(dat, formul, formula2, k, var_x, value1, var_y, value2){
  print(formul)
  print(formula2)
  print(k)
  print(var_x)
  print(value1)
  print(var_y)
  print(value2)
  library(jsonlite)
  dat <- fromJSON(dat)
  dat <- na.omit(dat)
  library(e1071)
  mymodel <- svm(formula=formul, data=dat, kernel=k, type="C-classification", scale=FALSE, y=NULL, x=NULL)
  list2 <-list()
  list2$var_x <- value1
  list2$var_y <- value2
  names(list2) <- c(var_x, var_y)
  plot(mymodel, dat, formula2, slice=list2)
}


