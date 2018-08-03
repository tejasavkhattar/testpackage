principalcomp <- function(data, classify){
  library(jsonlite)
  data <- fromJSON(data)
  library(ggfortify)
  df <- data[,-grep(classify, names(data))]
  autoplot(prcomp(df))
  autoplot(prcomp(df), data = data, colour = classify,
           loadings = TRUE, loadings.colour = 'blue',
           loadings.label = TRUE, loadings.label.size = 3)
}
