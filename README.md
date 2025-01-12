# freeVenn
Give you more freedom to draw Venn diagrams

**Install from Github**
```R
## Make sure 'devetools' is installed in your R
# install.packages("devtools")
devtools::install_github("yiluyucheng/freeVenn")
```
**Dependency**

Make sure 'ggplot2' and 'ggforce' are successfully installed.

**How to use**
```
vlist <- list(groupA=sample(1:100, 80), groupB=sample(50:150, 70), groupC=sample(1:100, 60))
## default
freeVenn(vlist)

## rough Weighted
freeVenn(vlist, weighted=TRUE)

## optimize Weighted
#' freeVenn(vlist, weighted=TRUE, Optimize=TRUE)
```
