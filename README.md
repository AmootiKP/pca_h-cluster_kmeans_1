library(ggplot2)
library(ggfortify)
library(stats)
library(tools)
#load data
data <- read.table("https://raw.githubusercontent.com/PineBiotech/omicslogic/master/LIHC_BRCA_data1_marked_no0.txt", header = TRUE, row.names = 1, stringsAsFactors = FALSE, check.names = FALSE)
#transpose data
origin_data <- t(data)
Group <- t(data[1,])
cnames <- colnames(data)
#reload data
data <- read.table("https://raw.githubusercontent.com/PineBiotech/omicslogic/master/LIHC_BRCA_data1_marked_no0.txt", header = TRUE, row.names = 1, skip = 2, stringsAsFactors = FALSE, check.names = FALSE)
colnames(data) -> cnames
#transform data
data <- transform(data, as.numeric)
#PCA----
pca <- prcomp(t(data), scale = TRUE, center = TRUE)
pca_res <- data.frame(pca$x, Group)
ggplot(pca_res, aes(x=PC1, y=PC2, colour=Group)) +geom_point() +stat_ellipse(level = 0.9)


#kmeans----
kmeans <- kmeans(data, 4)
#plot cluster
autoplot(kmeans, data =data, label = TRUE, label.size = 3, frame = TRUE, frame.type ='t')



#H-clusters----

data <- read.table ("https://raw.githubusercontent.com/PineBiotech/omicslogic/master/LIHC_BRCA_data1_marked_no0.txt", header = TRUE, row.names = 1)

#Transpose data
data1 <- t(data)

# Define clustering and save in a object
# specify distance matrix (euclidean / manhattan / maximum / canberra / binary / minkowski) and linkage(single / complete / average / mean / centroid / ward.D / ward.D2) type
cl <- hclust(dist (data1, method="euclidean"), method="ward.D2")

#Plot clusters
plot(cl, cex = 0.8)

#Define parameters and save in object
descr1 <- paste ("Distance: ", "euclidean", sep="")
descr2 <- paste ("Linkage: ", "complete", sep="")

#Plot Clusters
plot(cl, xlab=descr1, sub=descr2, cex = 0.8)

#Print cluster order
cl$order

