library(freeVenn)
library(cowplot)

set.seed(123)  # Set the seed to ensures reproducibility

vlist <- list(groupA=sample(1:100, 80), groupB=sample(50:150, 70), groupC=sample(1:100, 60))
vlist2 <- list(groupA=sample(1:100, 80), groupB=sample(60:150, 40))
vlist4 <- list(groupA=sample(1:100, 80), groupB=sample(50:150, 70), groupC=sample(1:100, 60), groupD=sample(40:200, 60))


p1 <- freeVenn(vlist)
p2 <- freeVenn(vlist, radii=0.7, alpha=0)
p3 <- freeVenn(vlist, radii=1.5, alpha=0.2, color=c('red', 'green', 'blue'), fontface='bold')

p4 <- freeVenn(vlist, weighted=TRUE) 
p5 <- freeVenn(vlist, weighted=TRUE, color=c('red', 'green', 'blue'), fontface='bold') + 
  scale_fill_manual(values=c('#ff70a6', '#70d6ff', '#ffd670'))
p6 <- freeVenn(vlist, weighted=TRUE, Optimize=TRUE, linewidth=1.2, linetype=2, alpha=0) +
    scale_color_manual(values=c('#ff70a6', '#70d6ff', '#ffd670'))
    
p7 <- freeVenn(vlist2)   
p8 <- freeVenn(vlist2, weighted=TRUE, linewidth=0)  
p9 <- freeVenn(vlist4, alpha=0.2)    

plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, p9, ncol = 3, labels = "auto")

setwd('D:')

ggsave('example.png', h=9, w=12)
