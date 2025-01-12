#' Make Venn diagrams for two sets, three sets, and four sets.
#'
#' @param vlist a list with proper names
#' @param ... all other parameters used for customizing plot
#'
#' @returns a figure as ggplot2 object
#' @export
#' @importFrom
#' ggplot2 ggplot
#' @importFrom
#' ggforce geom_circle geom_ellipse
#' @importFrom
#' stats median optim uniroot
#' @importFrom
#' utils combn
#' @examples
#' vlist <- list(groupA=sample(1:100, 80), groupB=sample(50:150, 70), groupC=sample(1:100, 60))
#' freeVenn(vlist)
#' ## rough Weighted
#' freeVenn(vlist, weighted=TRUE)
#' ## optimize Weighted
#' freeVenn(vlist, weighted=TRUE, Optimize=TRUE)
#'
freeVenn <- function(vlist, ...){
  if(length(vlist) == 2){
    .freeVenn2(vlist, ...)
  }else if(length(vlist) == 3){
    .freeVenn3(vlist, ...)
  }else if(length(vlist) == 4){
    .freeVenn4(vlist, ...)
  }
}
