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



# tr_subcap<-subj_caplist%>%pivot_wider(names_from = cap_id, values_from =subject_name)%>%
# pivot_longer( cols = `1002`:`997`,names_to = 'cap_id')
#
#
# r<-tr_subcap %>% unnest(value) %>%
#   group_by(cap_id) %>%
#   mutate(counter=paste0("Subj_", str_pad(1:n(),2,"left","0"))) %>%
#   spread(counter, value)

fid<-r1%>%tidyr::unite(id, cap_id, sep = '_')%>%select(id)%>%distinct()%>%as.list()
preps<-cbind(r1,fid)

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

############################# Finalize Clusters ###########################

Clus1<-cluster1%>%select(cap_degree,finalcluster)%>%distinct()
Clus2<-cluster2%>%select(cap_degree,finalcluster)%>%distinct()
Clus3<-cluster3%>%select(cap_degree,finalcluster)%>%distinct()

Combined_Cluster<-rbind(Clus1,Clus2,Clus3)%>%mutate(id=cap_degree)%>%separate(id, into = c("cap_id","degree"))

saveRDS(Combined_Cluster,"Combined_Cluster.RDS")
