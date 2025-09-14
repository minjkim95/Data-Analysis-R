library("readmnist")
image_data_train = Read.mnist('C:/Users/minjk/Downloads/train-images.idx3-ubyte')
View(image_data_train)

class(image_data_train$pic) # matrix
View(image_data_train$pic)
View(image_data_train$pic[0:4,])

integrand = function(x) {(8/x)*sin(2*x) - (4/(x^3))*sin(2*x) + (8/(x^2))*cos(2*x)}
integrate(integrand,lower = 0, upper = 1)

integrand1 = function(x) {(3/4)*x^4*exp((-x^3)/4)}
integrate(integrand1, lower = 0, upper = Inf)
