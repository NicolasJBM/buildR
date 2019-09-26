


net_node_equivalence <- function(graph){
  
  
  
  mwBlocks <- cohesive_blocks(as.undirected(connected), labels = TRUE)
  blocks(mwBlocks)
  cohesion(mwBlocks)
  plot(mwBlocks, connected)
}