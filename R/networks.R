#' @title Plot Network
#' @description Plots a network of kind Higher Educational space. 
#' @param g A graph object (igraph graph). 
#' @importFrom igraph plot.igraph
#' 
#' @examples 
#' plot_hes(g=ches0611)
#' @export
plot_hes <- function(g)
{
  plot.igraph(g,  vertex.label.cex = 0.5, vertex.label.font = 1, vertex.label.family = "Helvetica", vertex.label.color="black", asp =FALSE)
}