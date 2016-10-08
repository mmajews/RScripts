dimension_hypercube <- 5
side_lenght_hypercube <- 10
number_of_points_to_generate <-1000

randommly_selected_points = matrix(NA,2 * number_of_points_to_generate, dimension_hypercube)
row_number <- 0
for (i in 1:number_of_points_to_generate){
  coordinates_1 <- runif(dimension_hypercube, 0.0, side_lenght_hypercube)
  randommly_selected_points[row_number,] <- coordinates_1
  row_number <- row_number + 1
  coordinates_2 <- runif(dimension_hypercube, 0.0, side_lenght_hypercube)
  randommly_selected_points[row_number,] <- coordinates_2
  row_number <- row_number + 1
}

