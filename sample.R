#Example where n = 4

x <- 4

directed.m <- data.frame(unique(t(combn(rep(LETTERS[1:x],2),2))))

no_loops<-directed.m[-seq(0, nrow(directed.m), x), ]

undirected.m <- data.frame(unique(t(apply(X = no_loops, 1, sort))))

network.sample <- function(nodes){

nas <- function(x){
  if(ncol(x) < nodes){
    z <- matrix(NA, nrow = nrow(x), ncol = nodes - ncol(x))
    z <-cbind(x,z)
    return(z)
  }
  else if(ncol(x) == nodes){ return(x)}
}

out<-lapply(apply(matrix(c(1:nodes)), 1, FUN = function(x){t(combn(c(1:nodes),x))}), FUN = nas)

return(do.call(rbind,out))

}

i <- matrix(1:nrow(network.sample(nrow(no_loops))))

lapply(X = apply(i, 1, 
                 FUN = function(x){
                   no_loops[network.sample(nrow(no_loops))[x,],][complete.cases(no_loops[network.sample(nrow(no_loops))[x,],]),]
}), 
FUN = graph.data.frame)

#How many nodes? 3
#How many possible edges? 3*(3+1)
#How many possible combinations of edges? (2^(n^2)) -1
