# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

densitybasedclustering <- function(data, var_x, var_y, eps, MinPts) {
  library(jsonlite)
  data <- fromJSON(data)
  data <- na.omit(data)
  var_x <- data[,var_x]
  var_y <- data[,var_y]
  df <- cbind(var_x,var_y)
  library("fpc")
  db <- fpc::dbscan(df, eps, MinPts)
  library("factoextra")
  fviz_cluster(db, data = df, stand = FALSE,
               ellipse = FALSE, show.clust.cent = FALSE,
               geom = "point",palette = "jco", ggtheme = theme_classic())
}


