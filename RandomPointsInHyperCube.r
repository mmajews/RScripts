side_lenght_hypercube <- 10
number_of_points_to_generate <-1000
maximum_dimension <- 200
minimum_dimension <- 2
corner_of_hypercube = NA


parameters_beetwen_points = matrix(NA, nrow = 200, ncol = 3)
parameters_beetwen_point_and_corner = matrix(NA, nrow = 200, ncol = 3)

colnames(parameters_beetwen_points) <- c('Dimension', 'Variance', 'Expected Value')
colnames(parameters_beetwen_point_and_corner) <- c('Dimension', 'Variance', 'Expected Value')

all_distances_beetwen_points = list()
all_distances_beetwen_point_and_corner = list()

for(current_dimension in minimum_dimension:maximum_dimension)
{
  corner_of_hypercube = rep(side_lenght_hypercube, current_dimension)
  randomly_selected_points <- matrix(NA,2 * number_of_points_to_generate, current_dimension)
  distances_beetwen_points <- matrix(NA, number_of_points_to_generate, 1)
  distances_beetwen_point_and_corner <- matrix(NA, number_of_points_to_generate, 1)
  
  row_number <- 0
  for (i in 1:number_of_points_to_generate){
    coordinates_1 <- runif(current_dimension, 0.0, side_lenght_hypercube)
    randomly_selected_points[row_number,] <- coordinates_1
    row_number <- row_number + 1
    
    distances_beetwen_point_and_corner[i,] <- dist(rbind(coordinates_1, corner_of_hypercube))
    
    coordinates_2 <- runif(current_dimension, 0.0, side_lenght_hypercube)
    randomly_selected_points[row_number,] <- coordinates_2
    row_number <- row_number + 1
    
    distances_beetwen_points[i,] <- dist(rbind(coordinates_1, coordinates_2))
  }
  
  variance <- var(distances_beetwen_points)
  expected_value <- weighted.mean(distances_beetwen_points, rep(0.001, 1000))
  parameters_beetwen_points[current_dimension, ] <- c(current_dimension, variance, expected_value)
  all_distances_beetwen_points[[current_dimension]] <- distances_beetwen_points
  
  variance_point_corner <- var(distances_beetwen_point_and_corner)
  expected_value_point_corner <- weighted.mean(distances_beetwen_point_and_corner, rep(0.001, 1000))
  parameters_beetwen_point_and_corner[current_dimension, ] <- c(current_dimension, variance_point_corner, expected_value_point_corner)
  all_distances_beetwen_point_and_corner[[current_dimension]] <-distances_beetwen_point_and_corner
  
  remove(randomly_selected_points)
  remove(distances_beetwen_points)
  remove(distances_beetwen_point_and_corner)
}

dimensions =c(2,10,50,100,150,200)
#Drawing for hypercube distances between points

for(dimension in dimensions){
  hist(as.vector(all_distances_beetwen_points[dimension][[1]]), main = paste("n=",toString(dimension),sep=""),xlab = "Distance between points", col="darkgreen")
}

#Drawing for hypercube distances between point and fixed corner
for(dimension in dimensions){
  hist(as.vector(all_distances_beetwen_point_and_corner[dimension][[1]]), main = paste("n=",toString(dimension),sep=""),xlab = "Distance between point and corner", col="darkgreen")
}

