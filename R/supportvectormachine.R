supportvectormachine <- function(dat, formul, k) {
  library(jsonlite)
  dat <- fromJSON(dat)
  dat <- na.omit(dat)
  .formul <- reformulate(termlabels = c('.'), response=formul)
  dat <- as.data.frame(dat)
  dat <- na.omit(dat)
  library(e1071)
  mymodel <- svm(.formul, data=dat, kernel=k,type="C-classification", scale = FALSE)

  plot(mymodel, dat)
}


