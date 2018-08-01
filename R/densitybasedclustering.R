densitybasedclustering <- function(data, eps, MinPts) {
  library(jsonlite)
  data <- fromJSON(data)
  data <- na.omit(data)
  library("fpc")
  db <- fpc::dbscan(data, eps, MinPts)
  library("factoextra")
  fviz_cluster(db, data = data, stand = FALSE,
               ellipse = FALSE, show.clust.cent = FALSE,
               geom = "point",palette = "jco", ggtheme = theme_classic())
}
