library(igraph)

edge_samp <- function(x,n){y=x
combn(nrow(x),n,FUN=function(x) y[x,],simplify=FALSE)}

directednoloops <- function(x){
  
  flattenlist <- function(x){  
    morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
    out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
    if(sum(morelists)){ 
      Recall(out)
    }else{
      return(out)
    }
  }
  
  complete_g <- rbind(
    combn(x = x, m = 2) %>% t,
    combn(x = x, m = 2) %>% t %>% rev %>% matrix(ncol=2)
  )
  
  g_list_directednoloop <- apply(matrix(1:nrow(complete_g)), 1, function(x){edge_samp(complete_g, x)}) %>% flattenlist
  
  g_list_directednoloop[1:(x*(x-1))] <- lapply(g_list_directednoloop[1:(x*(x-1))], FUN = function(x){matrix(x, ncol=2)})
  
  g_list_directednoloop <- g_list_directednoloop %>% lapply(graph_from_data_frame)
  
  return(g_list_directednoloop)
  
}

directedloop <- function(x){
  
  complete_g <- rbind(
    combn(x = x, m = 2) %>% t,
    combn(x = x, m = 2) %>% t %>% rev %>% matrix(ncol=2),
    rep(x = c(1:x), each=2) %>% matrix(ncol=2, byrow = T)
  )
  
  g_list_directedloop <- apply(matrix(1:nrow(complete_g)), 1, function(x){edge_samp(complete_g, x)}) %>% flattenlist
  
  g_list_directedloop[1:(x^2)] <- lapply(g_list_directedloop[1:(x^2)], FUN = function(x){matrix(x, ncol=2)})
  
  g_list_directedloop <- g_list_directedloop %>% lapply(graph_from_data_frame)
  
  return(g_list_directedloop)
  
}

undirected <- function(x){
  
  complete_g <- combn(x = x, m = 2) %>% t 
  
  g_list_undirected <- apply(matrix(1:nrow(complete_g)), 1, function(x){edge_samp(complete_g, x)}) %>% flattenlist
  
  g_list_undirected[1:((x*(x-1))/2)] <- lapply(g_list_undirected[1:((x*(x-1))/2)], FUN = function(x){matrix(x, ncol=2)})
  
  g_list_undirected <- g_list_undirected %>% lapply(function(x)graph_from_data_frame(x, directed = F))
  
  return(g_list_undirected)
  
}


all_graphs <- function(x){
  
  list(
    undirected(x),
    directedloop(x),
    directednoloops(x)
  )
  
}

par(mfrow=c(1,4))

do.call(rbind,undirected(3) %>% lapply(function(x){centralization.betweenness(x)$centralization})) %>% hist

do.call(rbind,undirected(4) %>% lapply(function(x){centralization.betweenness(x)$centralization})) %>% hist

do.call(rbind,undirected(5) %>% lapply(function(x){centralization.betweenness(x)$centralization})) %>% hist

do.call(rbind,undirected(6) %>% lapply(function(x){centralization.betweenness(x)$centralization})) %>% hist

function(x){
  c(
    (2^(x^2))-1,
    (2^(x*(x-1)))-1,
    (2^((x*(x-1))/2))-1
  )
}
