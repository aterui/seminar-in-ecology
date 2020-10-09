
fw.plot <- function(S = 10, P = 0.5, node.size = 8, type = "random",
                    M, segment = T, energy = F, size.var = T){
  # load libraries
  require(MASS)
  require(bipartite)
  require(igraph)
  require(NetIndices)
  require(ggplot2)
  require(ggraph)
  
  if(type == "defined"){
    ## defined model
    im.b <- M
    
    im.graph <- graph.adjacency(im.b)
    E(im.graph)$strength <- runif(sum(im.b), 0.2, 1)
    lay <- create_layout(im.graph, "fr"); tl <- TrophInd(im.b)
    lay$y <- tl$TL
    
    if(energy == T){
      g <- ggraph(lay) + 
        geom_edge_fan(arrow = arrow(type = "closed", ends = "last",
                                    length = unit(5, "mm"), angle = 15),
                      aes(alpha = strength),
                      start_cap = circle(4, 'mm'),
                      end_cap = circle(4, 'mm'),
                      show.legend = F) +
        geom_node_point(size = node.size, color = grey(0, 0.5)) +
        theme_graph() 
    }else{
      g <- ggraph(lay) + 
        geom_edge_fan(arrow = arrow(type = "closed", ends = "last",
                                    length = unit(5, "mm"), angle = 15),
                      start_cap = circle(4, 'mm'),
                      end_cap = circle(4, 'mm'),
                      show.legend = F) +
        geom_node_point(size = node.size, color = grey(0, 0.5)) +
        theme_graph() 
    }
    
    if(segment == T){
      g <- g +  geom_segment(x = min(lay$x)-0.2, y = min(lay$y),
                             xend = min(lay$x)-0.2, yend = max(lay$y)) +
        scale_x_continuous(limits = c(min(lay$x)-0.2, max(lay$x)))
      
    }
    
    return(g)
  }# defined
  
  if(type == "linear"){
  ## linear model
    im.b <- matrix(0, S, S)
    for(i in 1:(S-1)){ im.b[i,i+1] <- 1 }
    
    im.graph <- graph.adjacency(im.b)
    E(im.graph)$strength <- runif(sum(im.b), 0.2, 1)
    
    if(size.var == T){
        V(im.graph)$size <- S:1
      }else{
        V(im.graph)$size <- node.size
    }
    
    lay <- create_layout(im.graph, "fr"); tl <- TrophInd(im.b)
    
    lay$x <- 1
    lay$y <- tl$TL
    
    if(energy == T){
      g <- ggraph(lay) + 
        geom_edge_fan(arrow = arrow(type = "closed", ends = "last",
                                    length = unit(5, "mm"), angle = 15),
                      aes(alpha = strength),
                      start_cap = circle(4, 'mm'),
                      end_cap = circle(4, 'mm'),
                      show.legend = F) +
        geom_node_point(aes(size = size),
                        color = grey(0, 0.5),
                        show.legend = F) +
        theme_graph() 
    }else{
      g <- ggraph(lay) + 
        geom_edge_fan(arrow = arrow(type = "closed", ends = "last",
                                    length = unit(5, "mm"), angle = 15),
                      start_cap = circle(4, 'mm'),
                      end_cap = circle(4, 'mm'),
                      show.legend = F) +
        geom_node_point(aes(size = size),
                        color = grey(0, 0.5),
                        show.legend = F) +
        theme_graph() 
    }
    
    if(segment == T){
      g <- g +  geom_segment(x = min(lay$x)-0.2, y = min(lay$y),
                            xend = min(lay$x)-0.2, yend = max(lay$y)) +
                scale_x_continuous(limits = c(min(lay$x)-0.2, max(lay$x)))
      
    }
  
    return(g)
  }## linear
  
  if(type == "random"){
  ## random model
    im.b <- matrix(rbinom(S*S, 1, P), S, S)
    im.b[lower.tri(im.b, diag = T)] <- 0
    
    im.graph <- graph.adjacency(im.b)
    E(im.graph)$strength <- runif(sum(im.b), 0.2, 1)
    lay <- create_layout(im.graph, "fr"); tl <- TrophInd(im.b)
    lay$y <- tl$TL
    
    if(energy == T){
      g <- ggraph(lay) + 
        geom_edge_fan(arrow = arrow(type = "closed", ends = "last",
                                    length = unit(5, "mm"), angle = 15),
                      aes(alpha = strength),
                      start_cap = circle(4, 'mm'),
                      end_cap = circle(4, 'mm'),
                      show.legend = F) +
        geom_node_point(size = node.size, color = grey(0, 0.5)) +
        theme_graph() 
    }else{
      g <- ggraph(lay) + 
        geom_edge_fan(arrow = arrow(type = "closed", ends = "last",
                                    length = unit(5, "mm"), angle = 15),
                      start_cap = circle(4, 'mm'),
                      end_cap = circle(4, 'mm'),
                      show.legend = F) +
        geom_node_point(size = node.size, color = grey(0, 0.5)) +
        theme_graph() 
    }
    
    if(segment == T){
      g <- g +  geom_segment(x = min(lay$x)-0.2, y = min(lay$y),
                             xend = min(lay$x)-0.2, yend = max(lay$y)) +
        scale_x_continuous(limits = c(min(lay$x)-0.2, max(lay$x)))
      
    }
    
    return(g)
  }# random
  
}