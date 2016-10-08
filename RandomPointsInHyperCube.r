library(stargazer)
library("plotrix")

side_lenght_hypercube <- 10
number_of_points_to_generate <-1500
maximum_dimension <- 200
minimum_dimension <- 3

all_points_in_cube = list()
all_points_in_sphere = list()



for(current_dimension in minimum_dimension:maximum_dimension)
{
  selected_points_cube <- matrix(NA,number_of_points_to_generate, current_dimension)
  selected_points_sphere <- matrix(NA,number_of_points_to_generate, current_dimension)
  
  #Hypercube generation
  for (i in 1:number_of_points_to_generate){
    coordinates <- runif(current_dimension, -side_lenght_hypercube/2, side_lenght_hypercube/2)
    selected_points_cube[i,] <- coordinates
  }
  
  #Hypershpere generation
  for (i in 1:number_of_points_to_generate){
    coordinates <- runif(current_dimension, -side_lenght_hypercube/2, side_lenght_hypercube/2)
    coefficient <- 0
    for(elem in coordinates) {
      coefficient <- coefficient + elem^2
    }
    coefficient <- sqrt(coefficient)
    coordinates <- (side_lenght_hypercube/2 * (runif(n = 1, min = 0, max = 1))^(1/current_dimension)/coefficient) * coordinates
    selected_points_sphere[i,] <- coordinates
  }
  
  all_points_in_sphere[[current_dimension]] <-selected_points_sphere
  all_points_in_cube[[current_dimension]] <-selected_points_cube
}


#Generate plots with given dimensions
dimensions = c(10, 50 , 100, 150 ,200)
for(dimension in dimensions){
  points_in_cube <- all_points_in_cube[[dimension]]
  points_in_cube <- points_in_cube[,c(1,2)]
  
  points_in_sphere = all_points_in_sphere[[dimension]]
  points_in_sphere <- points_in_sphere[,c(1,2)]
  
  max_coord <- side_lenght_hypercube/2
  
  plot(-max_coord:max_coord, -max_coord:max_coord, type = "n", asp = 1, main = paste("n=",toString(dimension),sep=""))
  points(points_in_cube[,1], points_in_cube[,2], col = "red")
  points(points_in_sphere[,1], points_in_sphere[,2], col = "blue")
  draw.circle(0, 0, max_coord, border = "blue")
  rect(-max_coord, -max_coord, max_coord, max_coord, border = "red")
}
