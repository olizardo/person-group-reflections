reflections <- function(x, iter = 20) { #x is a matrix with people as rows and groups as columns iters is number of reflections
   p <- nrow(x)
   g <- ncol(x)
   p.c <- matrix(0, p, iter) #initialize person centralities
   g.c <- matrix(0, g, iter) #initialize group centralities trajectory matrix
   rownames(p.c) <- rownames(x)
   rownames(g.c) <- colnames(x)
   colnames(p.c) <- paste("r_", c(1:iter))
   colnames(g.c) <- paste("r_", c(1:iter))
   p.c[, 1] <- rowSums(x) #person degree centrality (expansiveness)
   g.c[, 1] <- colSums(x) #group degree centrality (popularity)
   #g.c[, 1] <- colMeans(x) #alternative approach to group centralities used in Lizardo (2018)
   k <- 1 #initializing counter
   while (k < iter) {
      m <- k + 1
      for(i in 1:p) {
         p.c[i, m] <- mean(x[i, ] * g.c[, k]) #assign person avg. centrality groups they belong to
         } #end person loop
      for(j in 1:g) {
         g.c[j, m] <- mean(x[, j] * p.c[, k]) #assign genre avg. person centrality of people in group
         } #end group loop
      k <- k + 1 #increase counter
      } #end while loop
   for (i in 1:iter) {
      p.c[, i] <- scale(p.c[,i]) #rescale person reflections
      g.c[, i] <- scale(g.c[,i]) #rescale group reflections
      }
   return(list(p.r = p.c, g.r = g.c))
} #end function