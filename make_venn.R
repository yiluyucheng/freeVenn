## setwd('D:/Research/make_venn')
## source('make_venn.R')
library(ggplot2)
library(ggforce)


get_distance <- function(P_a, P_b){
    sqrt((P_a[1] - P_b[1])^2 + (P_a[2] - P_b[2])^2)
}  


find_weight_distance <- function(P_1, P_2, P_0, r){
    d_12 <- get_distance(P_1, P_2)   
    d <- 0.55 * (get_distance(P_1, P_0) * sign(get_distance(P_2, P_0) - get_distance(P_1, P_2)) + r) 
    
    # d / d_12 = (P_1[1] - x) / (P_2[1] - P_1[1])
    x <- P_1[1] - (P_2[1] - P_1[1]) * d / d_12 
    # d / d_12 = (y - P_1[2]) / (P_1[2] - P_2[2])
    y <- (P_1[2] - P_2[2]) * d / d_12 +  P_1[2]
    c(x, y)
}

#P_B_s <- find_weight_distance(colMeans(rbind(P_BC$point2, P_BC$point1), na.rm=TRUE), p_center, p_B, radiusB)
#P_BC_s <- find_weight_distance(P_BC$point2, p_center, p_A, radiusA)  
  
find_third_point <- function(P_a, P_b, D_bc, D_ac, sort_x=FALSE) {
  # Extract coordinates of points A and B
  x1 <- P_a[1]
  y1 <- P_a[2]
  x2 <- P_b[1]
  y2 <- P_b[2]
  
  # D_bc is the distance between B and C (a)
  a <- D_bc
  
  # D_ac is the distance between A and C (b)
  b <- D_ac
  
  # Calculate the distance between points A and B
  c <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  
  # Check if the triangle inequality holds
  if (a + b <= c || a + c <= b || b + c <= a) {
    # warning("The given side lengths do not form a valid triangle")
    return(list(point1 = c(NA, NA), point2 = c(NA, NA)))
  }
  
  # Determine the distance from point A along the line to the intersection
  x_part <- (b^2 - a^2 + c^2) / (2 * c)
  
  # Calculate the point along the line between A and B
  x_intermediate <- x1 + x_part * (x2 - x1) / c
  y_intermediate <- y1 + x_part * (y2 - y1) / c
  
  # Calculate the height of the triangle (distance from the intermediate point to C)
  height <- sqrt(b^2 - x_part^2)
  
  # Now calculate the coordinates of the third point (C) for both possible solutions
  x3_1 <- x_intermediate + height * (y2 - y1) / c
  y3_1 <- y_intermediate - height * (x2 - x1) / c
  
  x3_2 <- x_intermediate - height * (y2 - y1) / c
  y3_2 <- y_intermediate + height * (x2 - x1) / c
  
  if(sort_x & x3_1 > x3_2){
    return(list(point1 = c(x3_2, y3_2), point2 = c(x3_1, y3_1)))
  }else{
    # Return both possible solutions for the third point
    return(list(point1 = c(x3_1, y3_1), point2 = c(x3_2, y3_2)))
  }
}

# # Example usage with vertically aligned points
# P_a <- c(0, 0)  # Coordinates of point A
# P_b <- c(0, 4)  # Coordinates of point B
# D_bc <- 3       # Distance between B and C
# D_ac <- 5       # Distance between A and C

# third_point <- find_third_point(P_a, P_b, D_bc, D_ac)
# print(third_point)
  
  
compare_all_group_intersections <- function(group_list) {
  # Function to compute unique intersections for all possible group sizes
  n <- length(group_list)
  
  if (n < 2) {
    stop("There must be at least two groups to compute intersections.")
  }  
  # Initialize a list to store unique intersections for this group size
  size_intersections <- list()
  
  # Loop through each group size from 2 up to the total number of groups
  for (group_size in 1:n) {    
    # Generate all combinations of the vectors with the current group size
    comb <- combn(n, group_size, simplify = FALSE)
            
    # Compute unique intersections for each combination of the current group size
    for (i in seq_along(comb)) {
      # Get the vectors for the current combination
      vec_comb <- group_list[comb[[i]]]     
      # Find the intersection for the current combination
      intersection_items <- Reduce(intersect, vec_comb)
      # Get the indices of vectors not in the current combination
      remaining_indices <- setdiff(seq_along(group_list), comb[[i]])
      
      # Combine the remaining vectors into one vector
      remaining_vectors <- unlist(group_list[remaining_indices])      
      # Filter the intersection to include only items unique to this combination
      unique_items <- intersection_items[!intersection_items %in% remaining_vectors]
      
      # Store the result with combination index as the key
      size_intersections[[paste(comb[[i]], collapse = "-")]] <- length(unique_items)
    }
        
  }  
  return(size_intersections)
}


freeVenn4 <- function(tlist, alpha=0.6){ 
    angle <- 0.75
    intersections <- compare_all_group_intersections(tlist)  
    num_text <- data.frame(c_x=c(-5.5, -2.5, 2.5, 5.5, -3.3, -2.8, 0, 0, 2.8, 3.3, -1.8, 1.2, -1.2, 1.8, 0), 
        c_y=c(1.5, 5.25, 5.25, 1.5, 3, -1.8, -4, 3.6, -1.8, 3, 1.1, -2.9, -2.9, 1.1, -1.25), 
        #c_text=c(0001, 0002, 0003, 0004, 0005, 0006, 0007, 0008, 0009, 0010, 0011, 0012, 13, 14, 15))
        c_text=unlist(intersections))
    label_text <- data.frame(c_x=c(-5.5, -3, 3, 5.5), c_y=c(5.4, 7.5, 7.5, 5.4), c_text=names(tlist))

    d_ellipse <- data.frame(e_x = c(-2.7, -0.5, 0.5, 2.7), e_y=c(0, 1.6, 1.6, 0), 
        angle=c(angle, 0.8*angle, -0.8*angle, -angle), group=names(tlist))
        
    ggplot(d_ellipse) + 
       geom_ellipse(aes(x0 = e_x, y0 = e_y, a = 3, b = 6, angle = angle, fill=group), alpha= alpha, lwd=0.2) + 
       geom_text(data=num_text, aes(x=c_x, y=c_y, label=c_text), hjust='center', vjust='middle')+
       geom_text(data=label_text, aes(x=c_x, y=c_y, label=c_text), size = 6, hjust=c('right', 'center', 'center', 'left'), vjust='bottom')+
       theme_void() +
       theme(legend.position="none") +
       coord_fixed()
}       
#freeVenn4(list(groupA=sample(1:100, 80), groupB=sample(50:150, 70), groupC=sample(1:100, 60), groupD=sample(1:100, 30)))

sector_no_triangle <- function(side, radius){
    if(is.na(side)){
        return(0)
    }else if(side > 2*radius){
        return(paste0('Side :(', side, ') is greater than 2 times raius: ', radius))
    }
    h_side <- 0.5 * side ## half of the side
    sector_area <- asin(h_side / radius) * radius^2
    triangle_area <- sqrt(radius^2  - h_side^2) * h_side
    sector_area - triangle_area
}


intersect_area <- function(intersect_y, radiusA, radiusB, intersect_x=1) {
    side <- 2* intersect_y
    if(intersect_x > 0){
        sector_no_triangle(side, radiusA) + sector_no_triangle(side, radiusB)
    }else{
        pi * radiusA^2 - (sector_no_triangle(side, radiusA) - sector_no_triangle(side, radiusB))
    }
}


height_of_trangle <- function(sideA, sideB, sideC, return_Hight=TRUE){
    ## the height corresponding sideC
    ##find the height of a triangle given its three sides a, b,c  
    S <- 0.5 * (sideA + sideB + sideC)
    area_tAB <- sqrt(S * (S - sideA) * (S - sideB) * (S - sideC))
    if(return_Hight){
        2 * area_tAB / sideC
    }else{
        if(is.na(area_tAB)){
            area_tAB <- 0
        }
        area_tAB
    }

}
    

optim_circB_x <- function(cB_x, areaA, areaB, areaAB){        
    radiusA <- sqrt(areaA / pi)
    radiusB <- sqrt(areaB / pi)    
    #### S means semi_perimeter
    S <- 0.5 * (radiusA + radiusB + cB_x)
    area_tAB <- sqrt(S * (S - radiusA) * (S - radiusB) * (S - cB_x))
    intersect_y <- height_of_trangle(radiusA, radiusB, cB_x)
    
    ## when compute intersect area, ensure smaller radius ahead of the large one
    radiusA_ <- min(c(radiusA, radiusB))
    radiusB_ <- max(c(radiusA, radiusB))
    i_AB <- intersect_area(intersect_y, radiusA_, radiusB_, (radiusA_^2 + cB_x^2) - radiusB_^2)       
    #print(c(cB_x,areaAB - i_AB))
    areaAB - i_AB
    
}


get_circB_x <- function(areaA, areaB, areaAB){      
    radiusA <- sqrt(areaA / pi)
    radiusB <- sqrt(areaB / pi)
    if(areaAB == min(areaA, areaB)){
        return(abs(radiusA - radiusB))
    }         
    res <- uniroot(optim_circB_x, interval = c(abs(radiusA - radiusB)* 1.001, radiusA + radiusB), areaA, areaB, areaAB)
    res$root
}
        
    
freeVenn2 <- function(vlist, weighted=FALSE, radii=1, alpha=0.6){ 
    # vlist <- list(groupA=sample(1:100, 60), groupB=sample(1:100, 50))
    intersections <- compare_all_group_intersections(vlist)      
    #intersections$"2" <- 0
    #print(intersections)  
    if(weighted){
        areaA <- intersections[[1]] + intersections[[3]]
        areaB <- intersections[[2]] + intersections[[3]]
        areaAB <- intersections[[3]]
        cB_x <- get_circB_x(areaA, areaB, areaAB)
        radiusA <- sqrt(areaA / pi)
        radiusB <- sqrt(areaB / pi)
    }else{
        cB_x <- 1
        radiusA = radiusB <- radii
    }
         
    dat <- data.frame(coord_x=c(0, cB_x), coord_y=c(0, 0), radii=c(radiusA, radiusB), groups=names(vlist))    
    r_x <-  0.5 * c(cB_x - radiusB - radiusA, radiusA + radiusB + cB_x, radiusA + cB_x - radiusB)
    num_text <- data.frame(c_x=r_x, c_y=c(0, 0, 0), c_text=unlist(intersections))
    num_text <- num_text[num_text$c_text > 0, ] ####### catious here
    r_x_g <- r_x[-3]
    label_text <- data.frame(c_x=r_x_g, c_y=c(1.1*radiusA, 1.1*radiusB), c_text=names(vlist))

    ggplot(data = dat) +
        geom_circle(aes(x0=coord_x, y0=coord_y, r=radii, fill=groups), color=NA, alpha=alpha) +  
        geom_text(data=num_text, aes(x=c_x, y=c_y, label=c_text), hjust='center', vjust='middle')+
        geom_text(data=label_text, aes(x=c_x, y=c_y, label=c_text), hjust=c('right', 'left'), vjust='bottom')+
        coord_fixed() +
        #xlim(c(NA, (max(dat$coord_x) +radii)*1.2))+
        #theme_void() +
        theme(legend.position="none")
}      

# vlist <- list(groupA=sample(1:100, 90), groupB=sample(1:100, 60))
# freeVenn2(vlist, weighted=FALSE, radii=1)


scaled_diff <- function(d1, d2, delta=1){
    (d1 - d2)^2 / max(d1^2, delta)
}


too_far_loss <- function(distance, r1, r2){
    max(0, (distance -r1 -r2)/ (r1 + r2))
}


get_intersect_area <- function(p_A, p_B, p_C, radiusA, radiusB, radiusC){
    P_BC <- find_third_point(p_C, p_B, radiusB, radiusC, sort_x=TRUE)
    P_AC <- find_third_point(p_C, p_A, radiusA, radiusC, sort_x=TRUE)
    P_AB <- find_third_point(p_B, p_A, radiusA, radiusB)
    
    ## area of intersection of A, B, and C
    t_areaABC <- height_of_trangle(get_distance(P_AB$point1, P_BC$point1), get_distance(P_AC$point2, P_BC$point1), get_distance(P_AB$point1, P_AC$point2), return_Hight=FALSE)
    if(t_areaABC > 0){
        area_ABC_s <- t_areaABC + sector_no_triangle(get_distance(P_AB$point1, P_BC$point1), radiusB) +
            sector_no_triangle(get_distance(P_AC$point2, P_BC$point1), radiusC) + 
            sector_no_triangle(get_distance(P_AC$point2, P_AB$point1), radiusA)
    }else{
        area_ABC_s <- 0
    }
    
    area_AC_s <- sector_no_triangle(get_distance(P_AC$point1, P_AC$point2), radiusA) + 
        sector_no_triangle(get_distance(P_AC$point1, P_AC$point2), radiusC) -
        area_ABC_s
    area_AB_s <- sector_no_triangle(get_distance(P_AB$point1, P_AB$point2), radiusA) + 
        sector_no_triangle(get_distance(P_AB$point1, P_AB$point2), radiusB) -
        area_ABC_s
    area_BC_s <- sector_no_triangle(get_distance(P_BC$point1, P_BC$point2), radiusB) + 
        sector_no_triangle(get_distance(P_BC$point1, P_BC$point2), radiusC) -
        area_ABC_s
    area_A_s <- pi * radiusA^2 - area_AC_s - area_AB_s - area_ABC_s
    area_B_s <- pi * radiusB^2 - area_BC_s - area_AB_s - area_ABC_s
    area_C_s <- pi * radiusC^2 - area_AC_s - area_BC_s - area_ABC_s 
    return(unlist(data.frame(area_ABC_s, area_AC_s, area_AB_s, area_BC_s, area_A_s, area_B_s, area_C_s)))

}

    
find_2_circs_coords <- function(params, intersections){
    #print(params)
    cB_x <- params[1]
    cC_x <-  params[2]
    cC_y <- params[3]
    areaA <- intersections$"1" + intersections$"1-2" + intersections$"1-3" + intersections$"1-2-3"
    areaB <- intersections$"2" + intersections$"1-2" + intersections$"2-3" + intersections$"1-2-3"
    areaC <- intersections$"3" + intersections$"1-3" + intersections$"2-3" + intersections$"1-2-3"
    #### coordinates of cucular centers
    p_A <- c(0, 0)
    p_B <- c(cB_x, 0)
    p_C <- c(cC_x, cC_y)
    
    # dat <- data.frame(coord_x=c(p_A[1], p_B[1], p_C[1]), coord_y=c(p_A[2], p_B[2], p_C[2]), radii=c(radiusA, radiusB, radiusC), groups=names(vlist))
    # ggplot(data = dat) +
        # geom_circle(aes(x0=coord_x, y0=coord_y, r=radii, fill=groups), color=NA, alpha=alpha)

    ## distance between two circus
    d_AB <- get_distance(p_A, p_B)
    d_AC <- get_distance(p_A, p_C)
    d_BC <- get_distance(p_B, p_C)
    radiusA <- sqrt(areaA / pi)
    radiusB <- sqrt(areaB / pi)
    radiusC <- sqrt(areaC / pi)
    #cC_y <- height_of_trangle(d_AC, d_BC, d_AB) ## y coordinate of circus C
    #cC_x <- sqrt(d_AC^2 - cC_y^2)                              
    
    #### get joints between two circus
    tryCatch(
        {
        d_area <- get_intersect_area(p_A, p_B, p_C, radiusA, radiusB, radiusC)        
        error_score <- scaled_diff(intersections$'1', d_area['area_A_s']) + scaled_diff(intersections$'2', d_area['area_B_s']) + scaled_diff(intersections$'3', d_area['area_C_s']) +
            scaled_diff(intersections$'1-2', d_area['area_AB_s']) + scaled_diff(intersections$'1-3', d_area['area_AC_s']) +
            scaled_diff(intersections$'2-3', d_area['area_BC_s']) + scaled_diff(intersections$'1-2-3', d_area['area_ABC_s']) + 
            too_far_loss(d_AB, radiusA, radiusB) + too_far_loss(d_BC, radiusC, radiusB) + too_far_loss(d_AC, radiusC, radiusA)
        #print(error_score)
        error_score
        },
        error = function(e) {
          return(1000)
        }
    )
}


get_area_center <- function(joint_1, joint2, center_inner, center_outer, radius_inner, radius_outer){
    mean_joints <- colMeans(rbind(joint_1, joint2), na.rm=TRUE)
    delta <- get_distance(mean_joints, center_inner) - get_distance(center_outer, center_inner)
    if(delta > 0){
        pos <- -1
        ref_p <- mean_joints
    }else{
        pos <- 1
        ref_p <- center_outer
    }
    M_inner <- radius_inner - get_distance(mean_joints, center_inner)
    M_outer <- radius_outer - pos* get_distance(mean_joints, center_outer)
    d_target <- 0.5 * (2 * radius_inner - M_inner - M_outer)
    n_x <-  ref_p[1] +  pos*(radius_outer + d_target + max(0, delta)) * (mean_joints[1] - center_outer[1]) / get_distance(mean_joints, center_outer)
    n_y <-  ref_p[2] + pos*(radius_outer + d_target + max(0, delta)) * (mean_joints[2] - center_outer[2]) / get_distance(mean_joints, center_outer)
    c(n_x, n_y)
    
}

 #freeVenn3(vlist2, weighted=TRUE, Optimize=FALSE)


weighted_average <- function(center_1, center_2, weight_1, weight_2){
    c((center_1[1]*weight_1 + center_2[1]*weight_2) / (weight_1 + weight_2), 
        (center_1[2]*weight_1 + center_2[2]*weight_2) / (weight_1 + weight_2))
}


weighted_center <- function(P_AB, P_AC, radiusA, radiusB, radiusC, centerA, centerB, centerC, areaAB, areaAC){
    pos_AB <- get_area_center(P_AB$point1, P_AB$point2, centerA, centerB, radiusA, radiusB)
    pos_AC <- get_area_center(P_AC$point1, P_AC$point2, centerA, centerC, radiusA, radiusC)

    weighted_average(pos_AB, pos_AC, areaAB, areaAC)
}
#weighted_center(P_AB, P_BC, radiusB, radiusA, radiusC, p_B, p_A, p_C, d_area['area_AB_s'], d_area['area_BC_s'])

# P_A_s <- weighted_center(P_AB, P_AC, radiusA, radiusB, radiusC, p_A, p_B, p_C, areaAB, areaAC)

   
freeVenn3 <- function(vlist, weighted=FALSE, radii=1, alpha=0.6, Optimize=FALSE){ 
    # vlist <- list(groupA=sample(1:100, 50), groupB=sample(1:100, 40), groupC=sample(1:100, 20))
    # freeVenn(vlist)
    intersections <- compare_all_group_intersections(vlist)  
    #intersections$"1-2" <- 0
    #intersections$"1-3" <- 0
    #intersections$"1-2-3" <- 0
    #print(intersections) 
    if(weighted){
        areaA <- intersections$"1" + intersections$"1-2" + intersections$"1-3" + intersections$"1-2-3"
        areaB <- intersections$"2" + intersections$"1-2" + intersections$"2-3" + intersections$"1-2-3"
        areaC <- intersections$"3" + intersections$"1-3" + intersections$"2-3" + intersections$"1-2-3" 
        areaAB <- intersections$"1-2" + intersections$"1-2-3"
        areaAC <- intersections$"1-3" + intersections$"1-2-3"
        areaBC <- intersections$"2-3" + intersections$"1-2-3"    
        radiusA <- sqrt(areaA / pi)
        radiusB <- sqrt(areaB / pi)
        radiusC <- sqrt(areaC / pi)  

        d_AB <- get_circB_x(areaA, areaB, areaAB)
        d_AC <- get_circB_x(areaA, areaC, areaAC)
        d_BC <- get_circB_x(areaB, areaC, areaBC)

        cC_y <- height_of_trangle(d_AC, d_BC, d_AB) ## y coordinate of circus C
        ## chech if B and C are intersected
        if(areaBC > 0){
            sign_C <- 1
        }else{
            sign_C <- -1
        }
        cC_x <- sign_C * sqrt(d_AC^2 - cC_y^2)
        cB_x <- d_AB
        if(Optimize){
            init_B <- c(0, d_AB)
            init_C <- c(cC_x, cC_y)
            # Optimize
            result <- optim(
              #par = c(radiusA, radiusB, radiusA), 
              par = c(init_B[2], init_C[1], init_C[2]), 
              fn = find_2_circs_coords, 
              intersections = intersections, 
              method = "L-BFGS-B",   # Method allowing for box constraints
              #control = list(parscale = c(0.2,0.2,0.2)),
              #lmm=1,
              lower = c(-radiusA, -radiusA - radiusC, -radiusA), # Lower bounds for parameters
              upper = c(radiusA +radiusB, radiusA +radiusC, radiusA +radiusC)    # Upper bounds for parameters
            ) 
            #print(result)
            cB_x <- result$par[1]
            cC_x <- result$par[2]
            cC_y <- result$par[3]
        }                      
    }else{
        cB_x <- 1
        cC_x <- 0.5
        cC_y <- sqrt(3)*0.5
        radiusA = radiusB = radiusC <- radii
    }
    p_A <- c(0, 0)
    p_B <- c(cB_x, 0)
    p_C <- c(cC_x, cC_y) 
    dat <- data.frame(coord_x=c(p_A[1], p_B[1], p_C[1]), coord_y=c(p_A[2], p_B[2], p_C[2]), radii=c(radiusA, radiusB, radiusC), groups=names(vlist))    
    P_BC <- find_third_point(p_C, p_B, radiusB, radiusC, sort_x=TRUE)
    P_AC <- find_third_point(p_C, p_A, radiusA, radiusC, sort_x=TRUE)
    P_AB <- find_third_point(p_B, p_A, radiusA, radiusB)
    p_center <- c(mean(dat$coord_x), mean(dat$coord_y))

    d_area <- get_intersect_area(p_A, p_B, p_C, radiusA, radiusB, radiusC)
    #P_A_s <- find_weight_distance(colMeans(rbind(P_AC$point1, P_AB$point2, P_BC$point1), na.rm=TRUE), p_center, p_A, radiusA)
    P_A_s <- weighted_center(P_AB, P_AC, radiusA, radiusB, radiusC, p_A, p_B, p_C, d_area['area_AB_s'], d_area['area_AC_s'])
    P_BC_s <- find_weight_distance(P_BC$point2, p_center, p_A, radiusA)
    #P_B_s <- find_weight_distance(colMeans(rbind(P_BC$point2, P_AB$point2, P_AC$point2), na.rm=TRUE), p_center, p_B, radiusB)
    P_B_s <- weighted_center(P_AB, P_BC, radiusB, radiusA, radiusC, p_B, p_A, p_C, d_area['area_AB_s'], d_area['area_BC_s'])
    P_AC_s <- find_weight_distance(P_AC$point1, p_center, p_B, radiusB)
    #P_C_s <- find_weight_distance(colMeans(rbind(P_AC$point1, P_BC$point2, P_AB$point1), na.rm=TRUE), p_center, p_C, radiusC)
    P_C_s <- weighted_center(P_AC, P_BC, radiusC, radiusA, radiusB, p_C, p_A, p_B, d_area['area_AC_s'], d_area['area_BC_s'])
    P_AB_s <- find_weight_distance(P_AB$point2, p_center, p_C, radiusC)    
    #P_ABC <- (P_AB$point1 +P_BC$point1 + P_AC$point2) / 3
    P_ABC <- colMeans(rbind(P_AB$point1, P_BC$point1, P_AC$point2), na.rm=TRUE)
    
    num_text <- data.frame(c_x=c(P_A_s[1], P_B_s[1], P_C_s[1], P_AB_s[1], P_AC_s[1], P_BC_s[1], P_ABC[1]), 
        c_y=c(P_A_s[2], P_B_s[2], P_C_s[2], P_AB_s[2], P_AC_s[2], P_BC_s[2], P_ABC[2]), c_text=unlist(intersections))
    
    m_gap <- 0.1 *median(c(radiusA, radiusB, radiusC))
    label_text <- data.frame(c_x=c(0-0.8*radiusA, cB_x+0.8*radiusB, cC_x), c_y=c(0 - radiusA-m_gap, 0 - radiusB -m_gap, cC_y+radiusC +m_gap), c_text=names(vlist))


    ggplot(data = dat) +
        geom_circle(aes(x0=coord_x, y0=coord_y, r=radii, fill=groups), color=NA, alpha=alpha) +  
        geom_text(data=num_text, aes(x=c_x, y=c_y, label=c_text, hjust='center', vjust='middle'))+
        geom_text(data=label_text, aes(x=c_x, y=c_y, label=c_text, hjust='center', vjust='middle'))+
        coord_fixed() +
        #xlim(c(NA, (max(dat$coord_x) +radii)*1.2))+
        #theme_void() +
        theme(legend.position="none")
} 
#freeVenn3(vlist, weighted=TRUE, Optimize=FALSE)


freeVenn <- function(vlist, ...){
    if(length(vlist) == 2){
        freeVenn2(vlist, ...)
    }else if(length(vlist) == 3){
        freeVenn3(vlist, ...)
    }else if(length(vlist) == 4){
        freeVenn4(vlist, ...)
    }
}
## freeVenn(list(groupA=sample(1:100, 80), groupB=sample(50:150, 70), groupC=sample(1:100, 60), groupD=sample(1:100, 40)))


test_code <- FALSE
if(test_code){
    library(grid)

    library(patchwork)
    library(eulerr)

    # freeVenn3(list(groupA=sample(1:100, 80), groupB=sample(50:150, 70), groupC=sample(1:100, 60)))
    # vlist <- list(groupA=sample(1:1000, 400), groupB=sample(700:1500, 700), groupC=sample(500:1500, 700))

    p1 <- freeVenn3(vlist, weighted=FALSE, Optimize=FALSE)
    p2 <- freeVenn3(vlist, weighted=TRUE, Optimize=FALSE)
    p3 <- freeVenn3(vlist, weighted=TRUE, Optimize=TRUE)
    p1 + p2 + p3
    # ggsave(file='eulerr_limitn_1.pdf', h=3, w=6)

    dintersections <- unlist(compare_all_group_intersections(vlist) )
    #dintersections["1-2"] <- 0
    #dintersections["1-3"] <- 0
    #dintersections["1-2-3"] <- 0
    names(dintersections) <- gsub('-', '&', gsub('3', 'C', gsub('2', 'B', gsub('1', 'A', names(dintersections)))))

    pdf('D:/Research/make_venn/eulerr_limitn.pdf', h=4, w=4)
    #plot(euler(vlist),quantities = TRUE)
    plot(euler(dintersections),quantities = TRUE)
    dev.off()

}

