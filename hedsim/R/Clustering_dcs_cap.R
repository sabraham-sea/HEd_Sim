require(dplyr)
require(tidyr)



#####  hierarchical clustering based on dcs #####

prepdata<-trf_daily%>%select(school_name,degree_name,category_name.x,subject_name,cap_id)%>%
  filter(degree_name !='general')%>% na.omit()%>%
  distinct()

RID<-prepdata%>%tidyr::unite(id, cap_id,school_name,degree_name,category_name.x,subject_name, sep = '_')%>%select(id)%>%as.list()
prepdata<-cbind(prepdata,RID)
## Make RID as colname
prepdata<-tibble::column_to_rownames(prepdata, var = "id")


######################################3


# basedata0 <-trf_daily%>%select(school_name,degree_name,category_name.x,subject_name,cap_id)%>%
#   filter(degree_name !='general')%>% na.omit()%>%
#   distinct()%>%as_tibble()
#
# basedata <-trf_daily%>%select(school_name,degree_name,category_name.x,subject_name)%>%
#   filter(degree_name !='general')%>% na.omit()%>%
# distinct()%>%as_tibble()
#
# basedata1 <-trf_daily%>%select(school_name,degree_name,category_name.x,cap_id)%>%
#   filter(degree_name !='general')%>% na.omit()%>%
#   distinct()

########################################
###For clustering ###
basedata<-prepdata%>%select(degree_name,category_name.x,subject_name)

df <- na.omit(basedata)


df$degree_name <- as.factor(df$degree_name)
df$category_name.x <- as.factor(df$category_name.x
                                )
df$subject_name <- as.factor(df$subject_name
)


install.packages("cluster")
require(cluster)

# Dissimilarity matrix
gower.dist <- daisy(df, metric = c("gower"))


# Hierarchical clustering using Complete Linkage
hc1 <- hclust(gower.dist, method = "complete" )
plot(hc1, cex = 0.6, hang = -1)

#### methods to assess

m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

purrr::map_dbl(m, ac)

#### USING WARD


# Agnes
hc2 <- agnes(gower.dist, method = "ward" )
pltree(hc2, cex = 0.6, hang = -1, main = "Dendrogram of agnes")
hc3<-as.dendrogram(hc2)
plot(hc3)

pdf(file="dendrogram.pdf", height=10, width=30)
plot(hc2, which.plots=2, cex=1)
dev.off()

### Extract Cluster using agnes###

df$cluster<-cutree(hc2,17)  ## based on below
factoextra::fviz_cluster(list(data = gower.dist, cluster = df$cluster))
cluster1<-df%>%filter(cluster==1)

#Type 2
df$cluster<-cutree(hc2,5)  ## based on below
factoextra::fviz_cluster(list(data = gower.dist, cluster = df$cluster))
clusterlist<-split(df,df$cluster)
clusterlist
clust1<-clusterlist$`1`%>%rownames_to_column("id")%>% separate(id, into = c("cap_id","degree")) %>%select(c1)%>%distinct()
clust2<-clusterlist$`2`%>%rownames_to_column("id")%>% separate(id, into = c("c1")) %>%select(c1)%>%distinct()
clust3<-clusterlist$`3`%>%rownames_to_column("id")%>% separate(id, into = c("c1")) %>%select(c1)%>%distinct()

### Find correct cluster number ###
# require(factoextra)
# require(NbClust)
# # Plot cluster results
# p1 <- factoextra::fviz_nbclust(as.matrix(gower.dist), FUN = hcut, method = "wss",
#                    k.max = 10) +
#   ggtitle("(A) Elbow method")
# p2 <- fviz_nbclust(as.matrix(gower.dist), FUN = hcut, method = "silhouette",
#                    k.max = 10) +
#   ggtitle("(B) Silhouette method")
# p3 <- fviz_nbclust(as.matrix(gower.dist), FUN = hcut, method = "gap_stat",
#                    k.max = 10) +
#   ggtitle("(C) Gap statistic")
#
# # Display plots side by side
# gridExtra::grid.arrange(p1, p2, p3, nrow = 1)
#

################## FIND OPTIMAL CLUSTER NUMBER ####################
sil_width <- c(NA)

for(i in 2:20){

  pam_fit <- pam(gower.dist,
                 diss = TRUE,
                 k = i)

  sil_width[i] <- pam_fit$silinfo$avg.width

}

# Plot sihouette width (higher is better)

plot(1:20, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:20, sil_width)
#############################################

gower_mat <- as.matrix(gower.dist)

# Output most similar pair

df[which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]

### Cluster using pam ###
pam_fit <- pam(gower.dist, diss = TRUE, k = 17)

pam_results <- df %>%
  mutate(cluster = pam_fit$clustering)

cluster1<-pam_results%>%filter(cluster==1)

results <- df %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

results$the_summary



