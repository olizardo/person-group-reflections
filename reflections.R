reflections <- function(x, iter) { #x is a matrix with people as rows and groups as columns iters is number of reflections
   p.c <- matrix(c(rep(1, iter*nrow(x))), nrow = nrow(x)) #initialize person centralities
   
   g.c <- matrix(c(rep(1, iter*ncol(x))), nrow = ncol(x)) #initialize group centrality trajectory matrix
   pc[, 1] <- rowSums(x) #person centrality
   gc[, 1] <- colSums(x) #genre group centrality
   k <- 1 #initializing counter
   while (k < iter) {
      m <- k + 1
      for(i in 1:nrow(x)) {
         pc.t <- x[i,] * gc[,k]
         pc[i,m] <- mean(pc.t[pc.t > 0]) #assign person avg. genre centrality
      }
      k <- k + 1 #increase counter
   }
   for(i in 1:ncol(x)) {
      gc.t <- x[,i] * pc[,k]
      gc[i,m] <- mean(gc.t[gc.t > 0]) #assign genre avg. person centrality
   }
   
   rownames(gc) <- colnames(x)
   for (i in 1:iter) {
      gc[,i] <- scale(gc[,i]) #rescale higher genre reflections
      pc[,i] <- scale(pc[,i]) #rescale higher person reflections
   }
   return(list(person.reflections = p.c, genre.reflections = g.c))
}