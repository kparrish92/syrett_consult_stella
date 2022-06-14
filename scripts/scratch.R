


d <- dist(df_int, method = "euclidean")

hc1 <- hclust(d, method = "ward.D")

plot(hc1, cex = 0.6, hang = -1)

p1 <- fviz_nbclust(df_int, FUN = hcut, method = "wss",
                   k.max = 10) +
  ggtitle("(A) Elbow method")

p2 <- fviz_nbclust(df_int, FUN = hcut, method = "silhouette",
                   k.max = 10) +
  ggtitle("(B) Silhouette method")

p3 <- fviz_nbclust(df_int, FUN = hcut, method = "gap_stat",
                   k.max = 10) +
  ggtitle("(C) Gap statistic")

gridExtra::grid.arrange(p1, p2, p3, nrow = 1)



# cl <- kmeansruns(df_raw, krange = 4:10, iter.max = 1000)


hc.cut <- hcut(df_int, k = 5, hc_method = "average")
fviz_dend(hc.cut, show_labels = TRUE, rect = TRUE)

clust_data <- select(clust_data,-speaker) # remo
head(clust_data)# show first couple rows