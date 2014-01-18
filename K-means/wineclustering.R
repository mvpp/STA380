setwd("~/documents/R")
winedata <- read.csv("wine.csv")


# Preprocessing
wine <- data.matrix(winedata, rownames.force = NA)

# Normalize
for (i in 2:14){
  wine[,i] <- scale(wine[,i],center=min(wine[,i]),scale=max(wine[,i])-min(wine[,i]))
}
# Now all the values in each variable have the same range[0,1] except for Class.

# K-means
# First, generate random centroids.
centroid1 <- c(runif(14,0,1))
centroid2 <- c(runif(14,0,1))
centroid3 <- c(runif(14,0,1))
 
d = 1
while(d >= 0.0001){
  clstr1 <- data.frame(centroid1)
  clstr2 <- data.frame(centroid2)
  clstr3 <- data.frame(centroid3)
  for(i in 1 : 178){
    d1 <- sum(abs(centroid1[2:14]-wine[i,2:14]))
    d2 <- sum(abs(centroid2[2:14]-wine[i,2:14]))
    d3 <- sum(abs(centroid3[2:14]-wine[i,2:14]))
    # Put each observasion into clusters that has nearest centroid to it.
    if(d1<d2 & d1<d3) {
      clstr1 <- data.frame(clstr1,wine[i,])
    }
    else if(d2<d3 & d2<d1){
      clstr2 <- data.frame(clstr2,wine[i,])
    }
    else{
      clstr3 <- data.frame(clstr3,wine[i,])
    }
  }
  # Delete the temporarily included centroids
  clstr1 <- clstr1[,-1]
  clstr2 <- clstr2[,-1]
  clstr3 <- clstr3[,-1]

  # Calculate the new centroid of each cluster and the migration length of centroids.
  d <- sum((centroid1[2:14] - apply(clstr1[2:14,],1,mean))^2) + sum((centroid2[2:14] - apply(clstr2[2:14,],1,mean))^2) + sum((centroid3[2:14] - apply(clstr3[2:14,],1,mean))^2)
  centroid1 <- apply(clstr1,1,mean)
  centroid2 <- apply(clstr2,1,mean)
  centroid3 <- apply(clstr3,1,mean)
}

# We used the puriness of each cluster to define the accuracy of our algorithm.
# Since the "Class" variable is nominal, there is no way to say a point that is clustered as Class 2 is misclassified or not.
# So assuming that the algorithm worked well(and it did), we calculated the frequency of the most frequent "Class" as the notion of puriness.
puriness1 <- max(table(t(clstr1[1,])))/ncol(clstr1)
puriness2 <- max(table(t(clstr2[1,])))/ncol(clstr2)
puriness3 <- max(table(t(clstr3[1,])))/ncol(clstr3)

# Puriness of the kmeans package in R
y <- kmeans(wine[,-1],3,10,3)
packageresult <- table(wine[,1], y$cluster)
packagepuriness1 <- max(packageresult[,1])/sum(packageresult[,1])
packagepuriness2 <- max(packageresult[,2])/sum(packageresult[,2])
packagepuriness3 <- max(packageresult[,3])/sum(packageresult[,3])
