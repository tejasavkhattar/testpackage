#copyright 2016 Helikar Lab
#copyright 2018 Helikar Lab

#Developed by Achilles Gasper Rasquinah, Tejasav Khattar,Shubham Kumar, Vinit Ravishankar and Akram Mohammed

#This program is free software: you can redistribute it and/or modify it under
#the terms of the GNU General Public License as published by the Free Software
#Foundation, either version 3 of the License, or (at your option) any later
#version. This program is distributed in the hope that it will be useful, but
#WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
#FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
#details. You should have received a copy of the GNU General Public License
#along with this program. If not, see <http://www.gnu.org/licenses/>

comatrix <- function(data = "", method = ""){

  library(jsonlite)

  data <- fromJSON(data)
  data <- data.frame(data)
  data <- na.omit(data)
  rownames(data) <- as.vector(data[,1])
  data[,1] <- NULL
  data <- as.matrix(data)

  if(method == 'cor'){
    mat = cor(data)
  }
  if(method == 'cov'){
    mat = cov(data)
  }

  t <- dim(mat)[1]

  V1 <- rep("R", t)
  V2 <- rownames(mat)
  columns <- data.frame(V1, V2)
  columns <- as.matrix(columns)

  index <- V2
  data <- as.matrix(mat)

  out <- list(columns = columns, index = index, data = data)
  out <- toJSON(out)

  list(message = paste(out))
}
