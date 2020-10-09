euclideanDistance <-function(u, v) {
  sqrt(sum((u - v) ^ 2))
}

weight <- function(i, k) {
  return((k + 1 - i) / k)
}

sortObjectsByDist <- function(xl, z, metricFunction = euclideanDistance) {
  l <-dim(xl)[1]
  n <-dim(xl)[2] - 1
  
  distances <- matrix(NA, l, 2)
  
  for (i in 1:l) {
    distances[i, ] <- c(i,metricFunction(xl[i, 1:n], z))
  }
  
  orderedXl <- xl[order(distances[, 2]), ]
  return (orderedXl);
}

kNN <- function(xl, z, k) {
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return (class)
}

kWNN <- function(xl, z, k) {
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  weights <- rep(0,3)
  names(weights) <- c("setosa", "versicolor", "virginica")
  classes <- orderedXl[1:k, n + 1]
  for(i in 1:k) {
    weights[classes[i]] <- weight(i,k) + weights[classes[i]];
  }
  class <- names(which.max(weights))
  return(class)
}

LOO <- function(xl) {
  l = dim(xl)[1]
  Err = matrix(0, l-1, 1)
  for (i in 1:l) {
    z = xl[i, 1:2]
    new_xl = xl[-i, ]
    new_xl = sortObjectsByDist(new_xl,z)
    for (k in 1:l-1) {
      class <- KNN(new_xl, z,k)
      if (class != xl[i,3]) {
        Err[k] = Err[k] + 1
      }
    }
  }
  return (which.min(Err))
}

colors <- c("setosa" = "red", "versicolor" = "green3", "virginica" = "blue")
plot(iris[, 3:4], pch = 21, bg = colors[iris$Species], col = colors[iris$Species], asp = 1)

xl <- iris[, 3:5]
#k <- LOO(xl)
k <- 6
print(k)

for(i in seq(1,7,0.1)) {
  for(j in seq(0,3,0.1)) {
    z <- c(i,j)
    xl <- iris[, 3:5]
    class <- kWNN(xl, z, k)
    points(z[1], z[2], pch = 1, col = colors[class], asp = 20)
  }
}
