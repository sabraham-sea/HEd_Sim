require(tidyr)
require(stringr)
require(cluster)
require(magrittr)
require(factoextra)
require(tibble)

trf_daily<-readRDS('trf_daily_final_July22.RDS')

## To find cap in cluster ##

subj_caplist<-trf_daily%>%ungroup()%>%filter(date >= '2020-01-01' )%>%select(cap_id,subject_name)%>%
  distinct()%>%filter(!is.na(cap_id) & subject_name!= 'general')%>%arrange(subject_name) 

r1<-subj_caplist %>% unnest(subject_name) %>%
  group_by(cap_id) %>%
  mutate(counter=paste0("Subj_", str_pad(1:n(),2,"left","0"))) %>%
  spread(counter, subject_name)


subj_caplist<-trf_daily%>%ungroup()%>%filter(date >= '2020-01-01' )%>%select(cap_id,subject_name,degree_name)%>%
  distinct()%>%filter(!is.na(cap_id) & subject_name!= 'general')%>%arrange(subject_name)

r2<-subj_caplist %>% unnest(subject_name,degree_name) %>%
  group_by(cap_id) %>%
  mutate(counter=paste0("Subj_", str_pad(1:n(),2,"left","0"))) %>%
  spread(counter, subject_name)



# tr_subcap<-subj_caplist%>%pivot_wider(names_from = cap_id, values_from =subject_name)%>%
# pivot_longer( cols = `1002`:`997`,names_to = 'cap_id')
#
#
# r<-tr_subcap %>% unnest(value) %>%
#   group_by(cap_id) %>%
#   mutate(counter=paste0("Subj_", str_pad(1:n(),2,"left","0"))) %>%
#   spread(counter, value)

fid<-r2%>%tidyr::unite(id, cap_id, sep = '_')%>%select(id)%>%distinct()%>%as.list()
preps<-cbind(r2,fid)

# Use only caps in current cap #
capcurrent_caplist<-readRDS('capcurrent.RDS')%>%select(cap_id)

preps<-preps%>%filter(cap_id %in% capcurrent_caplist$cap_id)


## Make RID as colname
preps<-tibble::column_to_rownames(preps, var = "id")
preps%<>%select(-c(cap_id))

preps[is.na(preps)] <- '9999'
preps1 <- preps %>% type.convert()

gower.dist <- daisy(preps1, metric = c("gower"))

#### Find which technique ###
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(preps1, method = x)$ac
}

purrr::map_dbl(m, ac)

hc2 <- agnes(gower.dist, method = "ward" )


################## FIND OPTIMAL CLUSTER NUMBER ####################
sil_width <- c(NA)

for(i in 2:100){
  
  pam_fit <- pam(gower.dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:100, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:100, sil_width)
#############################################




preps1$cluster<-cutree(hc2,4) ## based on above
preps1[preps1 ==9999] <- NA
factoextra::fviz_cluster(list(data = gower.dist, cluster = preps1$cluster))

clusterlist<-split(preps1,preps1$cluster)



cluster1<-clusterlist$`1`%>%rownames_to_column("id")
cluster2<-clusterlist$`2`%>%rownames_to_column("id")
cluster3<-clusterlist$`3`%>%rownames_to_column("id")
cluster4<-clusterlist$`4`%>%rownames_to_column("id")


saveRDS(cluster1,"Cluster1Subject.RDS")
saveRDS(cluster2,"Cluster2Subject.RDS")
saveRDS(cluster3,"Cluster3Subject.RDS")
saveRDS(cluster4,"Cluster4Subject.RDS")


saveRDS(r2, "Cap_Degree_subjects.RDS")

############################# Finalize Clusters ###########################

# Clus1<-cluster1%>%select(id)%>%distinct()%>%mutate(clusno='1')
# Clus2<-cluster2%>%select(id)%>%distinct()%>%mutate(clusno='2')
# Clus3<-cluster3%>%select(id)%>%distinct()%>%mutate(clusno='3')
# Clus4<-cluster4%>%select(id)%>%distinct()%>%mutate(clusno='4')
# Clus5<-cluster5%>%select(id)%>%distinct()%>%mutate(clusno='5')
# 
# Combined_Cluster<-rbind(Clus1,Clus2,Clus3,Clus4,Clus5)%>%rename(cap_id=id)

Combined_Cluster<-rbind(cluster1,cluster2,cluster3,cluster4)%>%rename(cap_id=id)

saveRDS(Combined_Cluster,"Combined_Cluster.RDS")


########### Adding degree back in #################

capdeg<-r2%>%select(cap_id,degree_name)

Combined_Cluster_Degree<-left_join(capdeg,Combined_Cluster,by=c("cap_id"))

saveRDS(Combined_Cluster_Degree,"Combined_Cluster_Degree.RDS")
######################################3

Cap_Degree_subjects<-readRDS('Cap_Degree_subjects.RDS')
Combined_Cluster<- readRDS('Combined_Cluster.RDS')

Cap_Degree_subjects_Clusno<-left_join(Cap_Degree_subjects,Combined_Cluster,by=c("cap_id"))

saveRDS(Cap_Degree_subjects_Clusno,'Cap_Degree_subjects_Clusno.RDS')

cap_id_school<-trf_daily_final%>%select(cap_id,school_name)%>%distinct()%>%filter(!is.na(cap_id))

Combined_Cluster_Degree<- readRDS('Combined_Cluster_Degree.RDS')
Cap_Degree_subjects_Clusno<-left_join(Combined_Cluster_Degree,cap_id_school,by=c("cap_id"))

saveRDS(Cap_Degree_subjects_Clusno,'Cap_Degree_subjects_Clusno.RDS')

cap_school_degree<-Cap_Degree_subjects_Clusno%>%select(cap_id,school_name,cluster,degree_name=degree_name.x)%>%distinct()

saveRDS(cap_school_degree,'cap_school_degree.RDS')  # Use this file


clus3<-cap_school_degree%>%filter(cluster==3)
clus2<-cap_school_degree%>%filter(cluster==2)
clus4<-cap_school_degree%>%filter(cluster==4)
clus1<-cap_school_degree%>%filter(cluster==1)


#Regroup based on degree

clus1$finalcluster<-if_else(clus1$degree_name=='associates', 11, 
                            if_else(clus1$degree_name=='bachelors', 12, 
                                                    if_else(clus1$degree_name =='masters',13,
                                                        if_else(clus1$degree_name=='doctorate',14,
                                                        if_else(clus1$degree_name=='non-degree',15,16)))))

clus2$finalcluster<-if_else(clus2$degree_name=='associates', 21, 
                            if_else(clus2$degree_name=='bachelors', 22, 
                                    if_else(clus2$degree_name =='masters',23,
                                            if_else(clus2$degree_name=='doctorate',24,
                                                    if_else(clus2$degree_name=='non-degree',25,26)))))


clus3$finalcluster<-if_else(clus3$degree_name=='associates', 31, 
                            if_else(clus3$degree_name=='bachelors', 32, 
                                    if_else(clus3$degree_name =='masters',33,
                                            if_else(clus3$degree_name=='doctorate',34,
                                                    if_else(clus3$degree_name=='non-degree',35,36)))))

clus4$finalcluster<-if_else(clus4$degree_name=='associates', 41, 
                            if_else(clus4$degree_name=='bachelors', 42, 
                                    if_else(clus4$degree_name =='masters',43,
                                            if_else(clus4$degree_name=='doctorate',44,
                                                    if_else(clus4$degree_name=='non-degree',45,46)))))

saveRDS(clus1,"cluster1.RDS")
saveRDS(clus2,"cluster2.RDS")
saveRDS(clus3,"cluster3.RDS")
saveRDS(clus4,"cluster4.RDS")


cluster1<-readRDS("cluster1.RDS")
cluster2<-readRDS("cluster2.RDS")
cluster3<-readRDS("cluster3.RDS")
cluster4<-readRDS("cluster4.RDS")

allclusters<-rbind(cluster1,cluster2,cluster3,cluster4)

saveRDS(allclusters,'Final_Cluster.RDS')

d<-rbind(cluster1,cluster2,cluster3,cluster4)
write.csv(d, file = "FinalCluster_List.csv", 
           col.names = TRUE)
