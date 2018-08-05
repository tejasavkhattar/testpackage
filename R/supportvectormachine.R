supportvectormachine <- function(dat, formul, formula2, new, k, var_x, value1, var_y, value2) {
  library(jsonlite)
  dat <- fromJSON(dat)
  dat <- na.omit(dat)
  .formul <- reformulate(termlabels = c('.'), response=formul)
  .formula2 <- reformulate(termlabels = c(new), response=formula2)
  dat <- as.data.frame(dat)
  library(e1071)
  mymodel <- svm(.formul, data=dat, kernel=k,type="C-classification", scale = FALSE)
  list2 <-list()
  list2$var_x <- value1
  list2$var_y <- value2
  names(list2) <- c(var_x, var_y)
  plot(mymodel, dat, .formula2, slice=list2)
}

#supportvectormachine(iris,"Species", "Petal.Length", "Petal.Width", "radial", "Sepal.Width", 3, "Sepal.Length", 4)


