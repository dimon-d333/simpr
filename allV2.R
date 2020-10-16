euclideanDistance <-function(u, v) {
  sqrt(sum((u - v) ^ 2))
}

weight <- function(i, k, q) {
  return (q ^ i)
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

kNN2 <- function(xl, z, k) {
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  classes <- orderedXl[1:k, n + 1]
  counts <- table(classes)
  class <- names(which.max(counts))
  return (class)
}

kWNN <- function(xl, z, k, q) {
  orderedXl <- sortObjectsByDist(xl, z)
  n <- dim(orderedXl)[2] - 1
  weights <- rep(0,3)
  names(weights) <- c("class-1", "class-2", "class-3")
  classes <- orderedXl[1:k, n + 1]
  for(i in 1:k) {
    weights[classes[i]] <- weight(i,k,q) + weights[classes[i]];
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
      class <- kWNN(new_xl, z,k)
      if (class != xl[i,3]) {
        Err[k] = Err[k] + 1
      }
    }
  }
  return (which.min(Err))
}

colors <- c("class-1" = "red", "class-2" = "green3", "class-3" = "blue")

df = data.frame(x = double(), y = double(), class = character())

df <- rbind(df, data.frame(x = 1, y = 0.1, class = "class-1"))
df <- rbind(df, data.frame(x = 1.15, y = 0.15, class = "class-1"))
df <- rbind(df, data.frame(x = 1.1, y = 0.1, class = "class-1"))
df <- rbind(df, data.frame(x = 1.5, y = 0.2, class = "class-2"))
df <- rbind(df, data.frame(x = 1.35, y = 0.2, class = "class-2"))
df <- rbind(df, data.frame(x = 1.3, y = 0.25, class = "class-2"))
df <- rbind(df, data.frame(x = 1.5, y = 0.25, class = "class-2"))
df <- rbind(df, data.frame(x = 1.35, y = 0.3, class = "class-2"))
df <- rbind(df, data.frame(x = 1.4, y = 0.25, class = "class-2"))
df <- rbind(df, data.frame(x = 1.4, y = 0.25, class = "class-2"))
df <- rbind(df, data.frame(x = 3, y = 0.85, class = "class-3"))
df <- rbind(df, data.frame(x = 3.2, y = 0.9, class = "class-3"))
df <- rbind(df, data.frame(x = 3.25, y = 1, class = "class-3"))
df <- rbind(df, data.frame(x = 3.1, y = 0.8, class = "class-3"))
df <- rbind(df, data.frame(x = 3.15, y = 0.85, class = "class-3"))
plot(df[c("x", "y")], pch = 19, col = colors[df$class])

z <- c(1.2, 0.15)
#class <- kNN(df, z, k=7)
class <- kWNN(df, z, k=7, q=0.56)
print(class)
points(z[1], z[2], pch = 22, bg = colors[class], asp = 1)
