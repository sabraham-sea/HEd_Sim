require(tidyr)
require(stringr)
require(cluster)
require(magrittr)
require(factoextra)
require(tibble)

trf_daily<-readRDS('trf_daily.RDS')

## To find cap in cluster ##

subj_caplist<-trf_daily%>%filter(date >= '2020-01-01' )%>%select(cap_id,subject_name)%>%
  distinct()%>%filter(!is.na(cap_id) & subject_name!= 'general')%>%arrange(subject_name)

r1<-subj_caplist %>% unnest(subject_name) %>%
  group_by(cap_id) %>%
  mutate(counter=paste0("Subj_", str_pad(1:n(),2,"left","0"))) %>%
  spread(counter, subject_name)


subj_caplist<-trf_daily%>%filter(date >= '2020-01-01' )%>%select(cap_id,subject_name,degree_name)%>%
  distinct()%>%filter(!is.na(cap_id) & subject_name!= 'general')%>%arrange(subject_name)

r2<-subj_caplist %>% unnest(subject_name) %>%
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




preps1$cluster<-cutree(hc2,5) ## based on above
preps1[preps1 ==9999] <- NA
factoextra::fviz_cluster(list(data = gower.dist, cluster = preps1$cluster))

clusterlist<-split(preps1,preps1$cluster)

############################# Evaluate Clusters ###########################

cluster1<-clusterlist$`1`%>%rownames_to_column("id")%>% mutate(cap_degree=id)%>%separate(id, into = c("cap_id","degree"))%>%
  mutate(finalcluster= if_else(
    degree == "bachelors", "bachelors1",
    if_else( degree=="doctorate", "doctorate1",
             if_else(degree=="masters", "masters1",
                     if_else( degree=="general", "general1",
                              if_else( degree=="associates", "associates1",
                                       if_else( degree=="certificate", "certficate1",
                                                if_else( degree=="grad", "grad-certificate1","non-degree1")))))))
    
    
    
  )

cluster2<-clusterlist$`2`%>%rownames_to_column("id")%>% mutate(cap_degree=id)%>%separate(id, into = c("cap_id","degree"))%>%
  mutate(finalcluster= if_else(
    degree == "bachelors", "bachelors2",
    if_else( degree=="doctorate", "doctorate2",
             if_else(degree=="masters", "masters2",
                     if_else( degree=="general", "general2",
                              if_else( degree=="associates", "associates2",
                                       if_else( degree=="certificate", "certficate2",
                                                if_else( degree=="grad", "grad-certificate2","non-degree2")))))))
    
    
    
  )



cluster3<-clusterlist$`3`%>%rownames_to_column("id")%>% mutate(cap_degree=id)%>%separate(id, into = c("cap_id","degree"))%>%
  mutate(finalcluster= if_else(
    degree == "bachelors", "bachelors3",
    if_else( degree=="doctorate", "doctorate3",
             if_else(degree=="masters", "masters3",
                     if_else( degree=="general", "general3",
                              if_else( degree=="associates", "associates3",
                                       if_else( degree=="certificate", "certficate3",
                                                if_else( degree=="grad", "grad-certificate3","non-degree3")))))))
    
    
    
  )

cluster1<-clusterlist$`1`%>%rownames_to_column("id")
cluster2<-clusterlist$`2`%>%rownames_to_column("id")
cluster3<-clusterlist$`3`%>%rownames_to_column("id")
cluster4<-clusterlist$`4`%>%rownames_to_column("id")
cluster5<-clusterlist$`5`%>%rownames_to_column("id")

saveRDS(cluster1,"Cluster1Subject.RDS")
saveRDS(cluster2,"Cluster2Subject.RDS")
saveRDS(cluster3,"Cluster3Subject.RDS")
saveRDS(cluster4,"Cluster4Subject.RDS")
saveRDS(cluster5,"Cluster5Subject.RDS")

saveRDS(r2, "Cap_Degree_subjects.RDS")

############################# Finalize Clusters ###########################

# Clus1<-cluster1%>%select(id)%>%distinct()%>%mutate(clusno='1')
# Clus2<-cluster2%>%select(id)%>%distinct()%>%mutate(clusno='2')
# Clus3<-cluster3%>%select(id)%>%distinct()%>%mutate(clusno='3')
# Clus4<-cluster4%>%select(id)%>%distinct()%>%mutate(clusno='4')
# Clus5<-cluster5%>%select(id)%>%distinct()%>%mutate(clusno='5')
# 
# Combined_Cluster<-rbind(Clus1,Clus2,Clus3,Clus4,Clus5)%>%rename(cap_id=id)

Combined_Cluster<-rbind(cluster1,cluster2,cluster3,cluster4,cluster5)%>%rename(cap_id=id)

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

cap_school_degree<-Cap_Degree_subjects_Clusno%>%select(cap_id,school_name,cluster,degree_name)

saveRDS(cap_school_degree,'cap_school_degree.RDS')


clus3<-cap_school_degree%>%filter(cluster==3)
clus2<-cap_school_degree%>%filter(cluster==2)
clus4<-cap_school_degree%>%filter(cluster==4)
clus5<-cap_school_degree%>%filter(cluster==5)
clus1<-cap_school_degree%>%filter(cluster==1)


