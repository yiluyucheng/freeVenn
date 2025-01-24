# freeVenn

**freeVenn: A R Package Offering Greater Freedom for Creating Venn Diagrams with ggplot2**.

freeVenn allows you to create Venn diagrams with two, three, or four sets.

### 1. Install from Github

```R
## Make sure 'devetools' is installed in your R
# install.packages("devtools")
devtools::install_github("yiluyucheng/freeVenn")
```

### 2. Citation
Your citation encourages me to make this package better! 

If you have benefited from this package in any way, we kindly ask you cite this work:
```
@misc{freeVenn,
  author = {Yucheng Wang},
  title = {freeVenn: A R Package Offering Greater Freedom for Creating Venn Diagrams with ggplot2},
  year = {2025},
  publisher = {GitHub},
  url = {https://github.com/yiluyucheng/freeVenn}
}
```

### 3. Dependency

Make sure '**ggplot2**' and '**ggforce**' are successfully installed.

### 4. How to use


```
## Load package and prepare input data

library(freeVenn)
set.seed(123)  # Set the seed to ensures reproducibility

vlist <- list(groupA=sample(1:100, 80), groupB=sample(50:150, 70), groupC=sample(1:100, 60))

#### unweighted Venn ####
## default
freeVenn(vlist) # Figure a

## Modify the radius of circles to adjust the overlapping proportions
## Set the transparency parameter alpha = 0 to make the circles empty.
freeVenn(vlist, radii=0.7, alpha=0) # Figure b

## Modify the 'color' parameter to set your favourite colour code for the outer lines of the circles
freeVenn(vlist, radii=1.5, alpha=0.2, color=c('red', 'green', 'blue'), fontface='bold') # Figure c


#### Weighted Venn diagram ####
## rough Weighted
freeVenn(vlist, weighted=TRUE) # Figure d

## optimize Weighted
## Use 'scale_fill_manual' to modify the colors of filled areas of circles.
freeVenn(vlist, weighted=TRUE, color=c('red', 'green', 'blue'), fontface='bold') + 
  scale_fill_manual(values=c('#ff70a6', '#70d6ff', '#ffd670')) # Figure e

## Empty circles and modify the width and type of circles.
freeVenn(vlist, weighted=TRUE, Optimize=TRUE, linewidth=1.2, linetype=2, alpha=0) +
    scale_color_manual(values=c('#ff70a6', '#70d6ff', '#ffd670')) # Figure f

### Make Venn diagrams for two and four sets###
vlist2 <- list(groupA=sample(1:100, 80), groupB=sample(60:150, 40))
vlist4 <- list(groupA=sample(1:100, 80), groupB=sample(50:150, 70), groupC=sample(1:100, 60), groupD=sample(40:200, 60))

freeVenn(vlist2)   # Figure g

## set linewidth=0 to hidden outer lines of circles
freeVenn(vlist2, weighted=TRUE, linewidth=0) # Figure h

freeVenn(vlist4, alpha=0.2) # Figure i
```

Plots generated from above codes:

<img width="960" src="https://github.com/yiluyucheng/freeVenn/blob/main/examples/example.png">


**Please refer to the below tutorial for additional information**

https://yiluyucheng.github.io/freeVenn/introduction.html

### Contact me
wangyucheng511@gmail.com

