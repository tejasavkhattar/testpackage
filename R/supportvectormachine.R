supportvectormachine <- function(dat, formul, formula2, k, var_x, value1, var_y, value2) {
  .formul <-reformulate(formul)
  .formula2 <-reformulate(formula2)
  dat <- model.matrix(dat)
  library(e1071)
  mymodel <- svm(.formul, data=dat, kernel=k)
  list2 <-list()
  list2$var_x <- value1
  list2$var_y <- value2
  names(list2) <- c(var_x, var_y)
  plot(mymodel, dat, .formula2, slice=list2)
}

supportvectormachine(iris,"Species~.", "Petal.Width~Petal.Length", "polynomial", "Sepal.Width", 3, "Sepal.Length", 4)

