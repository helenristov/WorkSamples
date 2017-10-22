

##  @knitr Question_1
library(ggplot2)
data <- fread('~/Interview_Projects/rev_data_for_test.csv')

g <- ggplot(data, aes(Type))
g + geom_bar()

## @knitr Question_2
g2 <- ggplot (data=data, aes(Latitude,Longitude, shape = Type, color = Type)) 
g2 + geom_point()

## @knitr Cluster_Algo

numClusters <- function(Data, clusterIter = 10, threshold = .02){
  
  for(i in 2:clusterIter){
    cData        <- kmeans(Data, centers = i, nstart= 100)  
    
    if(i == 2){ 
      
      explainedVar <- cData$betweenss/cData$totss
    }else{
      nextExpVar   <- cData$betweenss/cData$totss
      explainedVar <- rbind(explainedVar,nextExpVar)
    }
  }
  
  numClusters <- first(which(diff(explainedVar) < threshold))+1
  xaxis       <- seq(2,clusterIter, by=1)
  
  p <-  qplot(x = xaxis, y = explainedVar, geom = c("point", "line"),
              xlab = 'Clusters', main = 'Explained Variance by Number of Cluster Centers')
  p <- p + geom_line(size=0.8, colour='blue') + geom_point(colour="black", size = 4.5) 
  plot(p)
  
  return(numClusters)
}    

numClusters <- numClusters(data[,c("Latitude", "Longitude"), with = FALSE],
                           clusterIter = 10, 
                           threshold  = .05)
numClusters ##printing out the suggested number of cluster regions

##applying the clusters
clusterFit   <-    kmeans(data[,c("Latitude", "Longitude"), with=FALSE], 
                          centers = numClusters,
                          nstart = 100)

data$cluster <- clusterFit$cluster

for(i in 1:numClusters){
  
  subset <- data[which(data$cluster == i),]  
  pie <- ggplot(subset,aes(x = factor(""), fill = Type) ) 
  pie <- pie + geom_bar() +
    coord_polar(theta = "y") +
    scale_x_discrete("") +
    ggtitle(paste0("Cluster ", i, "  Types of Emergency Calls in Cluster"))
  
  plot(pie)
}

## @knitr Cluster_Categorization
options(warn=-1)
data$cluster.category[which(data$cluster == 1)] <- 'Marshawn Lynch Sighting'
data$cluster.category[which(data$cluster == 2)] <- 'Beaver Accident'
data$cluster.category[which(data$cluster == 3)] <- 'Seal Attack'
data$cluster.category[which(data$cluster == 4)] <- 'Latte Spills'

##accuracy of clustering prediction for each cluster
for(i in 1:numClusters){
  
  cluster  <- data[which(data$cluster == i)]
  accuracy <- length(which(cluster$Type == cluster$cluster.category))/nrow(cluster)
  
  if( i == 1 ) { category <- 'Marshawn Lynch Sighting'}
    else if ( i == 2 ){ category <- 'Beaver Accident'}
      else if ( i == 3 ){ category <- 'Seal Attack' }
        else if ( i == 4 ){ category <- 'Latte Spills'}
  
  print(paste0("The Accuracy in the cluster categorization for ", 
               category, " is ", round(accuracy,2)))
}

overall.accuracy <- length(which(data$Type == 
                                 data$cluster.category))/nrow(data)
overall.accuracy

