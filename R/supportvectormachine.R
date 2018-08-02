supportvectormachine <- function(dat, formul, formula2, var_x, value1, var_y, value2){
  library(jsonlite)
  dat <- fromJSON(dat)
  dat <- na.omit(dat)
  library(e1071)
  mymodel <- svm(formul, data=dat,  na.action =
                   na.omit, scale = FALSE)
  list2 <-list()
  list2$var_x <- value1
  list2$var_y <- value2
  names(list2) <- c(var_x, var_y)
  plot(mymodel, dat, formula2, slice=list2)
}


