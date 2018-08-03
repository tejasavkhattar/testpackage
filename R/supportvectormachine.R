supportvectormachine <- function(dat, formul, formula2, k, var_x, value1, var_y, value2){
  library(jsonlite)
  dat <- fromJSON(dat)

  .formul <-reformulate(formul)
  .formula2 <-reformulate(formula2)
  library(e1071)
  mymodel <- svm(.formul, data=dat, kernel=k, scale= FALSE, na.action =
                   na.omit)
  list2 <-list()
  list2$var_x <- value1
  list2$var_y <- value2
  names(list2) <- c(var_x, var_y)

  plot(mymodel, dat, .formula2, slice=list2)

}

tejasav <- function(data, classify) {
  y <- data[,classify]
  x <- data[,-grep(classify, names(data))]
  x <- as.matrix(x)
  model <- svm(y~x, type ="C-classification", scale = FALSE)

  plot(model, data )
}

