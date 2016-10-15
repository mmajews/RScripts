library(ggplot2)
library(spatstat)
points = list();
number_of_points = 2000;
random_points <- matrix(NA, nrow = 0, ncol = 2)
width <- 1
height <- 0.5
scale <- 1

X <- runifdisc(number_of_points)
elipse_points <- as.data.frame(affine(X, mat=diag(c(1,0.5))))
covariance <- as.data.frame(cov(elipse_points))
cov_eigen_vectors <- eigen(covariance)$vectors

plot <- ggplot(elipse_points, aes(x=x, y=y)) + geom_point()
plot <- plot + coord_cartesian(xlim = c(-scale,scale), ylim = c(-scale,scale))
plot <- plot + geom_segment(aes(x = 0, y = 0, xend = cov_eigen_vectors[1,1], yend = cov_eigen_vectors[1,2]),
                            arrow = arrow(length = unit(0.5, "cm")), size=2, color="blue")
plot <- plot + geom_segment(aes(x = 0, y = 0, xend = cov_eigen_vectors[2,1], yend = cov_eigen_vectors[2,2]),
                            arrow = arrow(length = unit(0.5, "cm")),  size=2, color="red")
print(plot)

