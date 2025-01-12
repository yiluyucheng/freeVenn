
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


weighted_average <- function(center_1, center_2, weight_1, weight_2){
  c((center_1[1]*weight_1 + center_2[1]*weight_2) / (weight_1 + weight_2),
    (center_1[2]*weight_1 + center_2[2]*weight_2) / (weight_1 + weight_2))
}


weighted_center <- function(P_AB, P_AC, radiusA, radiusB, radiusC, centerA, centerB, centerC, areaAB, areaAC){
  pos_AB <- get_area_center(P_AB$point1, P_AB$point2, centerA, centerB, radiusA, radiusB)
  pos_AC <- get_area_center(P_AC$point1, P_AC$point2, centerA, centerC, radiusA, radiusC)

  weighted_average(pos_AB, pos_AC, areaAB, areaAC)
}
