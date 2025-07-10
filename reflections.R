reflections <- function(x, iter = 20) { #x is a matrix with people as rows and groups as columns iters is number of reflections
    z <- t(x)
    p <- nrow(x)
    g <- ncol(x)
    p.c <- matrix(0, p, iter) #initialize person reflections trajectory matrix
    g.c <- matrix(0, g, iter) #initialize group reflections trajectory matrix
    rownames(p.c) <- rownames(x)
    rownames(g.c) <- colnames(x)
    colnames(p.c) <- paste("Ref_", c(1:iter), sep = "")
    colnames(g.c) <- paste("Ref_", c(1:iter), sep = "")
    p.c[, 1] <- rowSums(x) #person degree centrality 
    g.c[, 1] <- colSums(x) #group degree centrality 
    k <- 1 #initializing counter
    while (k < iter) {
        p.c[, k + 1] <- (x %*% g.c[, k]) * p.c[, 1]^-1 #assign person average reflection of groups they belong to
        g.c[, k + 1] <- (z %*% p.c[, k]) * g.c[, 1]^-1 #assign group average reflection of people in the group
        k <- k + 1 #increase counter
        } #end while loop
    p.s <- scale(p.c) #scaling person reflections
    g.s <- scale(g.c) #scaling group reflections
    p.r <- apply(p.s, 2, rank) #ranking person reflections
    g.r <- apply(g.s, 2, rank) #ranking group reflections
    return(list(p.c = p.c, g.c = g.c, p.s = p.s, g.s = g.s, p.r = p.r, g.r = g.r))
    } #end function