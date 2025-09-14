# Example 1 US State

help(state.x77)
View(state.x77)
state.x77.df <- data.frame(state.x77)
View(state.x77.df)
names(state.x77.df) <- c("Popul", "Income", "Illit", "LifeExp", "Murder", "HSGrad", "Frost", "Area")

# use sample correlation matrix
state.pc <- princomp(state.x77.df, cor=TRUE)

summary(state.pc, loadings = TRUE)

# Showing the eigenvalues of the correlation matrix:
(state.pc$sdev)^2

# A scree plot:
plot(1:(length(state.pc$sdev)),  (state.pc$sdev)^2, type='b', 
     main="Scree Plot", xlab="Number of Components", ylab="Eigenvalue Size")

# Where does the "elbow" occur?

# The elbow point occurs at 2
# What seems to be a reasonable number of PCs to use?

# Plotting the PC scores for the sample data in the space of the first two principal components:
par(pty="s")
View(state.pc$scores[,1])
plot(state.pc$scores[,1], state.pc$scores[,2], 
     xlab="PC 1", ylab="PC 2", type ='n', lwd=2)
# labeling points with state abbreviations:
text(state.pc$scores[,1], state.pc$scores[,2], labels=state.abb, cex=0.7, lwd=2)

# We see the Southeastern states grouped in the bottom left 
# and several New England states together in the bottom right.

# The biplot can add information about the variables to the plot of the first two PC scores:

biplot(state.pc, xlabs=state.abb)

# If the data matrix X is not well approximated by a rank-two matrix, then the visual information in the biplot is not a good approximation to the data.
# In this case, you should not try to interpret the biplot. 
# However, if X is close to a rank-two matrix, then you can interpret a biplot in the following ways:
# 
# 1. The cosine of the angle between a vector and an axis indicates the importance of the contribution of the corresponding variable to the axis dimension.
# 2. The cosine of the angle between vectors indicates correlation between variables. Highly correlated variables point in the same direction; 
#   uncorrelated variables are at right angles to each other.
# 3. Points that are close to each other in the biplot represent observations with similar values.
# 4. You can approximate the coordinates of an observation by projecting the point onto the variable vectors within the biplot.

# Note the Area vector (arrow) goes almost totally in the direction of the second PC, since its loading in the first pc is almost zero. 


# Example 2 mtcars data

# mtcars data
help(mtcars)

mtcars.pca <- princomp(mtcars[,c(1:7,10,11)], cor=TRUE)
# exclude the two categorical variables
# use sample correlation matrix (equivalent to standardization)
summary(mtcars.pca, loadings=TRUE)

# A scree plot:
plot(1:(length(mtcars.pca$sdev)),  (mtcars.pca$sdev)^2, type='b', 
     main="Scree Plot", xlab="Number of Components", ylab="Eigenvalue Size")


mtcars.country <- factor(c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3),
                    rep("US",4), rep("Europe", 3), "US", rep("Europe", 3)))
plot(mtcars.pca$scores[,1], mtcars.pca$scores[,2], 
     xlab="PC 1", ylab="PC 2",  lwd=2, col=mtcars.country)
legend("topright",legend=levels(mtcars.country),pch=1,col=1:3,cex=0.7)
# Now you see something interesting: the American cars form a distinct cluster. 
?biplot
biplot(mtcars.pca,xlabs=mtcars.country)
# Looking at the axes, you see that the American cars are characterized by high values for cyl, disp, and wt. 
# Japanese cars, on the other hand, are characterized by high mpg. 
# European cars are somewhat in the middle and less tightly clustered than either group.

biplot(mtcars.pca, choices=3:4,xlabs=mtcars.country)
# You don't see much here, but this isn't too surprising. 
# PC3 and PC4 explain very small percentages of the total variation, so it would be surprising 
# if you found that they were very informative and separated the groups or revealed apparent patterns.
