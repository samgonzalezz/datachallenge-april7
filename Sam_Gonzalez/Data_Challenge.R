
library (dplyr)
library (reshape2)
library (ggplot2)
library (readr)
library (lubridate)
library (plotly)
library (RRhelpr)
library (cluster)
library (fpc)
library (ggplot2)
library (stats)
library (scales)
library (plot_ly)

##IMPORT DATA
#Set working directory
happiness_raw <- read_csv('E:\\Data Challenge\\happiness.csv')
happiness_std <- scale(happiness[5:11], center = TRUE, scale = TRUE)
happiness2 = cbind(happiness[1:4], happiness_std)
happiness = happiness2

##K MEANS CLUSTERING
nclusters = 3
happiness_kclus <- kmeans(happiness[5:11],nclusters,nstart = 20)
happiness$Kclus = happiness_kclus$cluster
happiness_kclus$KCluster = as.factor(happiness_kclus$cluster)

happiness_numvars = ncol(happiness[5:11])
happiness_K_ClusterSummary <- as.data.frame(happiness_kclus$centers)

#---> Insert Happiness Cluster Summary table here <---###
#---> Find way to plot actuals clusters <---###

#Overlay Cluster means
happiness_K_ClusterSummary$ClusterID <- as.factor(rownames(happiness_K_ClusterSummary))
happiness_ClusterSummaryPlot <- melt(happiness_K_ClusterSummary,id = "ClusterID")
ggplotly(ggplot(data = happiness_ClusterSummaryPlot, aes(x = variable, y = value, group = ClusterID, colour = ClusterID)) + geom_line())


##PCA ANALYSIS
happiness_pca <- stats::prcomp(happiness[5:11], center = TRUE, scale. = TRUE)
happiness_proportion_pca = happiness_pca$sdev^2/sum(happiness_pca$sdev^2)
happiness$PC1 = happiness_pca$x[,1]
happiness$PC2 = happiness_pca$x[,2]
happiness$PC3 = happiness_pca$x[,3]

# Scree Plot
plot(happiness_proportion_pca[1:happiness_numvars])
#Choosing 3 Principal Components based on scree Plot
#Print percent variance captured by these PCs

happiness_plotme <- ggplot(data = as.data.frame(happiness_pca$x[,1:2]), aes(x = PC1, y = PC2)) + 
  geom_point(size = 2) + scale_color_RR(theme = "RR_Health") + theme_RR() + labs(x = paste("PC1 (", 
                                       scales::percent(happiness_proportion_pca[1]), " explained var.)", 
                                       sep = ""), y = paste("PC2 (", scales::percent(happiness_proportion_pca[2]), 
                                                            " explained var.)", sep = ""))

plot(happiness_plotme)

# PCA by region
happiness_plotme1 <- ggplot(data = as.data.frame(happiness_pca$x[,1:2]), aes(x = PC1, y = PC2)) + 
  geom_point(size = 2) + geom_point(aes(colour = happiness$Region)) + labs(x = paste("PC1 (",scales::percent(happiness_proportion_pca[1]), " explained var.)", 
                                                                                           sep = ""), y = paste("PC2 (", scales::percent(happiness_proportion_pca[2]), 
                                                                                                                " explained var.)", sep = ""))
ggplotly(happiness_plotme1)


# PCA WITH Kcluster color codes
happiness_plotme2 <- ggplot(data = as.data.frame(happiness_pca$x[,1:2]), aes(x = PC1, y = PC2)) + 
  geom_point(size = 2) + geom_point(aes(colour = as.factor(happiness$Kclus))) + labs(x = paste("PC1 (",scales::percent(happiness_proportion_pca[1]), " explained var.)", 
                                                                                     sep = ""), y = paste("PC2 (", scales::percent(happiness_proportion_pca[2]), 
                                                                                                          " explained var.)", sep = ""))
ggplotly(happiness_plotme2)

#this works but it is not interactive
test <-  scatterplot3d::scatterplot3d(happiness$PC1,happiness$PC2,happiness$PC3)
ggplotly(test)



plot_ly(happiness, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Kclus, colors = c('#BF382A', '#0C4B8E', '#FFE1A1'))
plot_ly(happiness, x = ~PC1, y = ~PC2, z = ~PC3, color = as.factor(happiness$Kclus), colors = c('#BF382A', '#0C4B8E', '#FFE1A1'))
plot_ly(happiness, x = ~PC1, y = ~PC2, z = ~PC3, color = as.factor(happiness$Kclus), colors = c('red', 'blue', 'green'))


#Plot EigenVectors
happiness_eigenvectors = as.data.frame(happiness_pca$rotation)

#Plot BiPlot
ggbiplot(happiness_pca, obs.scale = 1, var.scale = 1, ellipse = FALSE, circle = TRUE, groups = happiness$Region) +
  scale_color_RR(theme="RR_Health") + theme_RR() 

plot_ly(as.data.frame(happiness_pca$x), x=~PC1,y=~PC2,type='scatter',mode='markers', colors = happiness$Region)
# plot_ly(as.data.frame(happiness_pca$x), x=~PC1,y=~PC2,type='scatter',mode='markers',text=~paste(na.omit(happiness_pca)$datetime)) 

## HIERARCHICAL CLUSTERING
# method = "complete"
method = "ward.D"
happiness_clusters_hier <- hclust(dist((happiness[,5:11]), method = "euclidean"), method = method)
plot(happiness_clusters_hier, cex = 0.6)

# Cut tree into 3 groups
grp <- cutree(happiness_clusters_hier, k = 3)
happiness$HierCluster = grp

# # Visualize
# plot(res.hc, cex = 0.6) # plot tree
rect.hclust(happiness_clusters_hier, k = 3, border = 2:5) # add rectangles with colors

hier_clus1 = which(happiness$)


