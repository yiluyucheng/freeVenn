# library(ggplot2)
# library(ggforce)

.freeVenn2 <- function(vlist, weighted=FALSE, radii=1, alpha=0.5, linewidth=1, ...){
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

  dat <- data.frame(coord_x=c(0, cB_x), coord_y=c(0, 0), radii=c(radiusA, radiusB), group=names(vlist))
  r_x <-  0.5 * c(cB_x - radiusB - radiusA, radiusA + radiusB + cB_x, radiusA + cB_x - radiusB)
  num_text <- data.frame(c_x=r_x, c_y=c(0, 0, 0), c_text=unlist(intersections))
  num_text <- num_text[num_text$c_text > 0, ] ####### catious here
  r_x_g <- r_x[-3]
  label_text <- data.frame(c_x=r_x_g, c_y=c(1.1*radiusA, 1.1*radiusB), c_text=names(vlist))

  ggplot(data = dat) +
    geom_circle(aes(x0=coord_x, y0=coord_y, r=radii, fill=group, color=group), alpha=alpha, lwd=linewidth) +
    geom_text(data=num_text, aes(x=c_x, y=c_y, label=c_text), hjust='center', vjust='middle')+
    geom_text(data=label_text, aes(x=c_x, y=c_y, label=c_text), hjust=c('right', 'left'), vjust='bottom', ...)+
    coord_fixed() +
    #xlim(c(NA, (max(dat$coord_x) +radii)*1.2))+
    theme_void() +
    theme(legend.position="none")
}


.freeVenn3 <- function(vlist, weighted=FALSE, Optimize=FALSE, radii=1, alpha=0.5, linewidth=1, ...){
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
  dat <- data.frame(coord_x=c(p_A[1], p_B[1], p_C[1]), coord_y=c(p_A[2], p_B[2], p_C[2]), radii=c(radiusA, radiusB, radiusC), group=names(vlist))
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
    geom_circle(aes(x0=coord_x, y0=coord_y, r=radii, fill=group, color=group), alpha=alpha, lwd=linewidth) +
    geom_text(data=num_text, aes(x=c_x, y=c_y, label=c_text, hjust='center', vjust='middle'))+
    geom_text(data=label_text, aes(x=c_x, y=c_y, label=c_text, hjust='center', vjust='middle'), ...)+
    coord_fixed() +
    #xlim(c(NA, (max(dat$coord_x) +radii)*1.2))+
    theme_void() +
    theme(legend.position="none")
}


.freeVenn4 <- function(tlist, alpha=0.5, linewidth=1, ...){
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
    geom_ellipse(aes(x0 = e_x, y0 = e_y, a = 3, b = 6, angle = angle, fill=group, color=group), alpha= alpha, lwd=linewidth) +
    geom_text(data=num_text, aes(x=c_x, y=c_y, label=c_text), hjust='center', vjust='middle')+
    geom_text(data=label_text, aes(x=c_x, y=c_y, label=c_text), hjust=c('right', 'center', 'center', 'left'), vjust='bottom', ...)+
    theme_void() +
    theme(legend.position="none") +
    coord_fixed()
}
