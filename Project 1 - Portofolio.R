#import data
library(readxl)
data_pelanggan <- read_excel("Downloads/Excel_B_2_10060120045_Indah Siti Rahmawati.xlsx")
#standardize data
scale_data <- scale(data_pelanggan[, c("Total Pembelian", "Jarak Pengiriman")])
scale_dataframe <- as.data.frame(scale(data_pelanggan[, c("Total Pembelian", "Jarak Pengiriman")])) 
#multicollinearity 
vif_data <- data.frame(feature = colnames(scale_data))

vif_data$VIF <- apply(scale_data, 2, function(x) {
  vif(lm(x ~ ., data = scale_data))
})
print(vif_data)
#KMO test
library(psych)
kmo_result <- KMO(scale_data)
kmo_result
#elbow plot
library(factoextra)
wss <- function(k) {
  kmeans(scale_data, centers = k, nstart = 10)$tot.withinss
}

#count WSS
k.values <- 1:10
wss_values <- sapply(k.values, wss)
#plot
plot(k.values, wss_values, type = "b", pch = 19, frame = FALSE,
     xlab = "Number of Clusters K",
     ylab = "Total Within-Cluster Sum of Squares (WSS)",
     main = "Elbow Plot for Optimal K")

#silhouette score
library(cluster)
range_n_clusters <- 2:8

for (num_clusters in range_n_clusters) {
  kmeans_result <- kmeans(scale_data, centers = num_clusters, nstart = 25, iter.max = 50)
  cluster_labels <- kmeans_result$cluster
  silhouette_avg <- mean(silhouette(cluster_labels, dist(scale_data))[, 3])
  cat("For n_clusters =", num_clusters, ", the silhouette score is", silhouette_avg, "\n")
}

#k-means clustering
library(stats)      
library(openxlsx)  

set.seed(100)  
kmeans_result <- kmeans(scale_data, centers = 3)
hasil_kmeans <- as.data.frame(scale_data)
hasil_kmeans$Cluster <- kmeans_result$cluster
head(hasil_kmeans)
data_pelanggan$Cluster <- kmeans_result$cluster
head(data_pelanggan)

#bar chart clustering
library(ggplot2)
cluster_counts <- as.data.frame(table(data_pelanggan$Cluster))

ggplot(cluster_counts, aes(x = Var1, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  ggtitle("Data Frequency in Each Cluster(KMeans)") +
  xlab("Cluster") +
  ylab("Frequency") +
  theme_minimal()

#scatterplot
centroid_cluster <- kmeans_result$centers
total_kmeans0 <- data_pelanggan[data_pelanggan$Cluster == 1, "Total Pembelian"]
jarak_kmeans0 <- data_pelanggan[data_pelanggan$Cluster == 1, "Jarak Pengiriman"]

total_kmeans1 <- data_pelanggan[data_pelanggan$Cluster == 2, "Total Pembelian"]
jarak_kmeans1 <- data_pelanggan[data_pelanggan$Cluster == 2, "Jarak Pengiriman"]

total_kmeans2 <- data_pelanggan[data_pelanggan$Cluster == 3, "Total Pembelian"]
jarak_kmeans2 <- data_pelanggan[data_pelanggan$Cluster == 3, "Jarak Pengiriman"]


centroid_awal <- aggregate(cbind(Total_Pembelian = data_pelanggan$`Total Pembelian`, 
                                 Jarak_Pengiriman = data_pelanggan$`Jarak Pengiriman`), 
                           by = list(Cluster = data_pelanggan$Cluster), mean)


cluster_1 <- data.frame(Total = total_kmeans0, Jarak = jarak_kmeans0, Cluster = factor(1))
cluster_2 <- data.frame(Total = total_kmeans1, Jarak = jarak_kmeans1, Cluster = factor(2))
cluster_3 <- data.frame(Total = total_kmeans2, Jarak = jarak_kmeans2, Cluster = factor(3))

plot_data <- rbind(cluster_1, cluster_2, cluster_3)
ggplot() +
  geom_point(data = plot_data, aes(x = Total.Pembelian, y = Jarak.Pengiriman, color = Cluster), size = 3) +
  geom_point(data = centroid_awal, aes(x = Total_Pembelian, y = Jarak_Pengiriman), 
             color = "black", size = 6, shape = 8) +
  ggtitle("Customer Clustering") +
  xlab("Total Purchase (Rp)") +
  ylab("Shipping Distance (km)") +
  theme_minimal()
