#' ches0611 dataset
#'
#' Graph of Chilean Higher Education Space between 20016-2011.
#'
#' @format A graph (iGraph object or list) with this configuration:
#' \describe{
#' \item{Vertices}{Career program. It includes 10 oder features used in the orginal paper.}
#' \item{Edges}{Binary link between programs.}
#' }
#' @source \url{http://pantheon.media.mit.edu/}
#' @references 
#' Candia, C., Encarnação, S., & Pinheiro, F. L. (2019). The higher education space: Connecting degree programs from individuals’ choices. EPJ Data Science, 8(1), 1–17. https://doi.org/10.1140/epjds/s13688-019-0218-4
#' 
#' @keywords dataset
#' @examples
#' data(ches0611)
#' summary(ches0611)
#' plot_hes(ches0611)
"ches0611"