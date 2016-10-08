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
  
  png(file = paste(toString(dimension),"_sphere_cube_on_2D.png",sep="")) 
  plot(-max_coord:max_coord, -max_coord:max_coord, type = "n", asp = 1, main = paste("n=",toString(dimension),sep=""), xlab = "Points presented on 2D")
  points(points_in_cube[,1], points_in_cube[,2], col = "red")
  points(points_in_sphere[,1], points_in_sphere[,2], col = "blue")
  draw.circle(0, 0, max_coord, border = "blue")
  rect(-max_coord, -max_coord, max_coord, max_coord, border = "red")
  dev.off()
}

#Distances from 0.0
averages_cube = matrix(NA, 1, maximum_dimension)
colnames(averages_cube) <- do.call("expression", lapply(1:maximum_dimension, function(i) i))
averages_sphere = matrix(NA, 1, maximum_dimension)
colnames(averages_sphere) <- do.call("expression", lapply(1:maximum_dimension, function(i) i))
for (dimension in minimum_dimension:maximum_dimension){
  distances_cube = list()
  distances_sphere = list()
  
  center_coordinate = rep(0, current_dimension)
  points_in_cube <- all_points_in_cube[[dimension]]
  for(point in points_in_cube){
    distance <- dist(rbind(center_coordinate, point))
    distances_cube <- c(distances_cube, distance)
  }
  
  points_in_sphere = all_points_in_sphere[[dimension]]
  for(point in points_in_sphere){
    distance <- dist(rbind(center_coordinate, point))
    distances_sphere <- c(distances_sphere, distance)
  }
  
  averages_cube[1, dimension ] <- mean(unlist(distances_cube))
  averages_sphere[1, dimension ] <- mean(unlist(distances_sphere))
}
(file = paste(toString(dimension),"_bar_averages_distances.png",sep="")) 
averages_cube <- averages_cube[ , -c(1,2)]
barplot(averages_cube,main = "Average distance from center for cube", xlab = "Dimension", ylab = "Distance", col = "darkblue")
dev.off()

png(file = paste(toString(dimension),"_sphere_averages_distances.png",sep="")) 
averages_sphere <- averages_sphere[,-c(1,2)]
barplot(averages_sphere,main = "Average distance from centerfor sphere", xlab = "Dimension", ylab = "Distance", col = "darkblue")
dev.off()


#Number of point depending on r parameter
dimensions = c(10,50,100,150,200)
for (current_dimension in dimensions){
  center_coordinate = rep(0, current_dimension)
  
  coordinates <- runif(current_dimension, -side_lenght_hypercube/2, side_lenght_hypercube/2)
  coefficient <- 0
  for(elem in coordinates) {
    coefficient <- coefficient + elem^2
  }
  coefficient <- sqrt(coefficient)
  coordinates <- (side_lenght_hypercube/2 * (runif(n = 1, min = 0, max = 1))/coefficient) * coordinates
  r_distance = dist(rbind(center_coordinate, coordinates))
  
  sequence <- seq(r_distance/10, r_distance, by=(r_distance/10))
  sequence <- lapply(sequence, function(x) round(x, digits = 2))
  
  points_regarding_r_cube = matrix(NA, 1, length(sequence))
  colnames(points_regarding_r_cube) <- sequence
  points_regarding_r_sphere = matrix(NA, 1, length(sequence))
  colnames(points_regarding_r_sphere) <- sequence
  
  n <- 1
  for(i in sequence)
  {
    points_cube_count = list()
    points_sphere_count=list()
    
    points_in_cube <- all_points_in_cube[[current_dimension]]
    for(point in points_in_cube){
      distance <- dist(rbind(center_coordinate, point))
      if(distance < i){
        points_cube_count <- c(points_cube_count, distance)
      }
    }
    
    points_in_sphere = all_points_in_sphere[[current_dimension]]
    for(point in points_in_sphere){
      distance <- dist(rbind(center_coordinate, point))
      if(distance < i){
        points_sphere_count <- c(points_sphere_count, distance)
      }
    }
    
    points_regarding_r_cube[,n]<-length(points_cube_count)
    points_regarding_r_sphere[,n]<-length(points_sphere_count)
    n <- n +1
  }
  
  
  final <- list(points_regarding_r_sphere, points_regarding_r_cube)
  final <- do.call(rbind, final)
  
  png(file = paste(toString(current_dimension),paste("_distance_from_center_sphere_cube_n",current_dimension,".png"),sep="")) 
  barplot(final, xlab = "R", ylab = "Number of points", main = paste("Number of points <R for N=",current_dimension,sep = ""), legend = c("Sphere", "Cube"), col=c("yellow","red"))
  dev.off()
}



#Generate plots with given dimensions 2D
dimensions = c(10,50,100,150,200)
for(current_dimension in dimensions){
  coordinates <- runif(current_dimension, -side_lenght_hypercube/2, side_lenght_hypercube/2)
  r_distance = side_lenght_hypercube
  center_coordinate = c(0,0)
  
  points_in_cube <- all_points_in_cube[[current_dimension]]
  points_in_cube <- points_in_cube[,c(1,2)]
  
  points_in_sphere = all_points_in_sphere[[current_dimension]]
  points_in_sphere <- points_in_sphere[,c(1,2)]
  
  sequence <- seq(r_distance/10, r_distance, by=(r_distance/10))
  sequence <- lapply(sequence, function(x) round(x, digits = 2))
  
  points_regarding_r_cube = matrix(NA, 1, length(sequence))
  colnames(points_regarding_r_cube) <- sequence
  points_regarding_r_sphere = matrix(NA, 1, length(sequence))
  colnames(points_regarding_r_sphere) <- sequence
  
  n <- 1
  for(i in sequence)
  {
    points_cube_count = list()
    points_sphere_count=list()
    
    points_in_cube <- all_points_in_cube[[current_dimension]]
    for(point in points_in_cube){
      distance <- dist(rbind(center_coordinate, point))
      if(distance < i){
        points_cube_count <- c(points_cube_count, distance)
      }
    }
    
    points_in_sphere = all_points_in_sphere[[current_dimension]]
    for(point in points_in_sphere){
      distance <- dist(rbind(center_coordinate, point))
      if(distance < i){
        points_sphere_count <- c(points_sphere_count, distance)
      }
    }
    
    points_regarding_r_cube[,n]<-length(points_cube_count)
    points_regarding_r_sphere[,n]<-length(points_sphere_count)
    n <- n +1
  }

  final <- list(points_regarding_r_sphere, points_regarding_r_cube)
  final <- do.call(rbind, final)
  png(file = paste(toString(current_dimension),paste("_distance_from_center_sphere_cube_2D_n",current_dimension,".png"),sep="")) 
  barplot(final, xlab = "R", ylab = "Number of points", main = paste("Number of points <R for N=",current_dimension,sep = ""), legend = c("Sphere", "Cube"), col=c("yellow","red"))
  dev.off()
}