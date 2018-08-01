densitybasedclustering <- function(data, var_x, var_y, eps, MinPts) {
  library(jsonlite)
  data <- fromJSON(data)
  data <- na.omit(data)
  var_x <- data[,var_x]
  var_y <- data[,var_y]
  df <- cbind(var_x,var_y)
  library("fpc")
  db <- fpc::dbscan(df, eps, MinPts)

  fviz_cluster(db, data = df, stand = FALSE,
               ellipse = FALSE, show.clust.cent = FALSE,
               geom = "point",palette = "jco", ggtheme = theme_classic())
}
