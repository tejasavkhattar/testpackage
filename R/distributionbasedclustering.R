distributionbasedclustering <- function(data, classify) {
  library(jsonlite)
  data <- fromJSON(data)
  data <- na.omit(data)
  library(mclust)
  cl <- data[,grep(classify, names(data))]
  X <- data[,-grep(classify, names(data))]
  clPairs(X, cl)
  BIC <- mclustBIC(X)
  mod1 <- Mclust(X, x = BIC)
  plot(mod1, what = "classification")
}


