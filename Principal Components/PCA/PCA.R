# PCA
# A form of dimension reduction. - Transforms our data set to one with less features
# Maximizes Variance while minimizing.
# PCA is a statistical procedure that uses Eigendecompositions (finding eigenvalues/eigenvectors)
# to convert a set of observations to a set of linearly uncorrelated variables. (can use SVD - a different method)
# Each of these linearly uncorrelated variables (features) are known as Principal components.


####Read in teeth data from csv file.
# make the directory the one with the data 
setwd("C:/Users/Spenc/Documents/Youtube/Machine Learning/R/Principal Component")
teeth <- read.csv("teeth.csv", header=TRUE)
# teeth
head(teeth)   #print a few observations
teethpc<-teeth[,2:9]   # select just the data to be used      

pc.teeth <- princomp(teethpc, cor=TRUE)
# note one problem with R is that the method works even when the code is
# not exactly correct pc.teeth <- princomp(teethpc, scale=TRUE, center=TRUE)
# produces results but they are wrong

# names() tells you what information is available from the output
names(pc.teeth)

# summarize the pca   - note the std devs are divided by n not n-1
# makes them the square root of the eigenvalue
summary(pc.teeth) # Proportion of variance = sd ^2 / number of components.
# cumultive proportion explains total variance.

# do things a bit better
eigenvectors<-pc.teeth$loadings
eigenvalues <- pc.teeth$sdev*pc.teeth$sdev 

# loading is the eigenvector.
pc.teeth$loadings    # note that these are scaled so the sum of squares is 1.0

# not loadings but eigenvectors
eigenvectors #These are the principal components that determine the directions of the new feature space
eigenvalues  # Eigenvalues determine the magnitude

# obtain the correlations with the variables
# Based on 2 Principal components (a new coordinate system and orthogonal to each other)
# the observations are then projected onto the coordinate plane.
# For more than 1 Principal Component, the score is
cor(teeth[,2:9], pc.teeth$scores)
# make things a bit nicer
round( cor(teeth[,2:9], pc.teeth$scores) , 3)   # round the output

# scree plot      - note the default is a barchart
# Scree plot is a line plot of Principal Components vs Variances
# Helps deteremine the number of factors to keep.
# This method is critized for its subjectivity because you are essentially looking
# for an 'elbow' within the graph. The graph can have many 'elbows'
screeplot(pc.teeth,type='l',main='Screeplot for teeth data') #screeplot with dots and lines
abline(1,0,col='red',lty=2) #horizontal line at 1

# scatterplot of scores with labels given by mammal in col 1
#  type=n turns off the default symbol
plot(pc.teeth$scores[,1:2], type='n',xlab="PC1(59%)", ylab="PC2(18%)" )
points(pc.teeth$scores[,1:2],cex=0.5)   # this puts a small point at the center
text(pc.teeth$scores[,1:2],label=teeth[,1], cex=0.5) #add tname to plot	

# create a new plot window
#windows()
# scatterplot of scores with labels given by mammal in col 1
# type = n turns off the default symbol
#plot(pc.teeth$scores[,1:2], type='n',xlab="PC1(59%)", ylab="PC2(18%)" )
#points(pc.teeth$scores[,1:2],cex=0.5)   # this puts a small point at the center
#text(pc.teeth$scores[,1:2],label=teeth[,1], cex=0.5) #add tname to plot

# ------------------------------------------------------------------ #
# another way to code the analysis    
pc.fit <- prcomp(~  top.incisor +bottom.incisor +   top.cannine +   bot.cannine   +  top.premol +
                   botpremol   +     topmolar  + botmolar,
                 data=teeth,   scale=TRUE     )
eigenvalues <- pc.fit$sdev * pc.fit$sdev
eigenvectors <- pc.fit$rotation
eigenvalues
round(eigenvectors,3)

summary(pc.fit)

# third approach -------------------------------------------- #
# Another approach - fit using factor analysis function
# two factor solution   - useful for printing when p is large


library(psych)

#Factor Analysis - Principal Components Method
# Types of rotation... varimax, quartimax, promax, oblimin, simplimax, cluster
pc2 <- principal(teethpc,nfactors = 2,rotate="none")
pc2
# Prints out the Correlation matrix with the corresponding factors.

