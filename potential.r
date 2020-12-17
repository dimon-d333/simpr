potentialF <- function(x, z, g, F, h=c()) {
    m <- dim(x)[1]
    n <- dim(x)[2]-1
    if(sum(h) == 0) h <- c(rep(1, m/3), rep(0.25,(m - m / 3)))
    classes <- rep(0, length(names(table(x[,n+1]))))
    names(classes) <- names(table(x[,n+1]))
    for(i in 1:m) {
        y <- x[i, n+1]
        dist <- Distanse(x[i,1:n],z)
        w <- F(dist/h[i]) * g[i]
        classes[y] <- classes[y] + w
    }
    if(sum(classes) > 0) {
        class <- names(which.max(classes))
    } else {
        class <- "unknown"
    }
    return(class)
}

pError <- function(x,g,F, h) {
    error <- 0
    m <- dim(x)[1]
    n <- dim(x)[2]-1
    for(i in 1:m) {
        class1 <- potentialF(x,x[i,1:n],g,F,h)
        class2 <- x[i,n+1]
        if(class1 != class2) {
            error <- error + 1
        }
    }
    return(error)
}

Gamma <- function(x, F=KnFunction, h=c(), delta = 10) {
    m <- dim(x)[1]
    n <- dim(x)[2]-1
    
    if(sum(h) == 0) h <- c(rep(1, m/3), rep(0.25,(m - m / 3)))

    g <- rep(0,m)
    i <- 1
    while(pError(x,g,F,h) > delta) {
        class1 <- potentialF(x,x[i,1:n],g,F,h)
        class2 <- x[i,n+1]
        if(class1 != class2) g[i] <- g[i] + 1
        i <- ((40 + sample(1:110, 1)[1])%%m) + 1
    }
    return(g)
}
