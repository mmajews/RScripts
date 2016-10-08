side_lenght_hypercube <- 10
number_of_points_to_generate <-1000
maximum_dimension <- 200
minimum_dimension <- 2

all_counted_parameters = matrix(NA, nrow = 200, ncol = 3)
colnames(all_counted_parameters) <- c('Dimension', 'Variance', 'Expected Value')
all_distances = list()

for(current_dimension in minimum_dimension:maximum_dimension)
{
  randomly_selected_points <- matrix(NA,2 * number_of_points_to_generate, current_dimension)
  distances_beetwen_points <- matrix(NA, number_of_points_to_generate, 1)
  row_number <- 0
  for (i in 1:number_of_points_to_generate){
    coordinates_1 <- runif(current_dimension, 0.0, side_lenght_hypercube)
    randomly_selected_points[row_number,] <- coordinates_1
    row_number <- row_number + 1
    coordinates_2 <- runif(current_dimension, 0.0, side_lenght_hypercube)
    randomly_selected_points[row_number,] <- coordinates_2
    row_number <- row_number + 1
    distances_beetwen_points[i,] <- dist(rbind(coordinates_1, coordinates_2))
  }
  
  
  variance <- var(distances_beetwen_points)
  expected_value <- NA
  all_counted_parameters[current_dimension, ] <- c(current_dimension, variance, expected_value)
  all_distances[[current_dimension]] <- randomly_selected_points
}





