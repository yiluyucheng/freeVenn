## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(freeVenn)
set.seed(123)  # Set the seed to ensures reproducibility

vlist <- list(
  groupA=sample(1:100, 80), 
  groupB=sample(50:150, 70), 
  groupC=sample(1:100, 60)
)


## -----------------------------------------------------------------------------
freeVenn(vlist)

## -----------------------------------------------------------------------------
freeVenn(vlist, weighted=TRUE, color=c('red', 'green', 'blue'), fontface='bold') + 
  scale_fill_manual(values=c('#ff70a6', '#70d6ff', '#ffd670'))


## -----------------------------------------------------------------------------
vlist4 <- list(groupA=sample(1:100, 80), groupB=sample(50:150, 70), groupC=sample(1:100, 60), groupD=sample(50:150, 70))

freeVenn(vlist4)

## -----------------------------------------------------------------------------
vlist2 <- list(groupA=sample(1:100, 80), groupB=sample(50:150, 30))

freeVenn(vlist2, weighted=TRUE)

## -----------------------------------------------------------------------------
freeVenn(vlist, radii=0.8)

## -----------------------------------------------------------------------------
freeVenn(vlist, radii=1.5)

