# freeVenn
It provides much more freedom for creating Venn diagrams and supports configurations for two, three, or four sets.

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
library('freeVenn')
vlist <- list(groupA=sample(1:100, 80), groupB=sample(50:150, 70), groupC=sample(1:100, 60))
## default
freeVenn(vlist)

## rough Weighted
freeVenn(vlist, weighted=TRUE)

## optimize Weighted
#' freeVenn(vlist, weighted=TRUE, Optimize=TRUE)

## Set your own colours
freeVenn(vlist, weighted=TRUE, Optimize=TRUE, alpha=0.4) + 
  scale_fill_manual(values=c('green', 'red', 'blue'))
```

**Please refer the below tutorial for more information**
https://yiluyucheng.github.io/freeVenn/introduction.html

