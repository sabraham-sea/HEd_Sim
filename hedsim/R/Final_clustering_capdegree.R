trf_daily<-readRDS('trf_daily.RDS')

## To find cap in cluster ##

subj_caplist<-trf_daily%>%select(cap_id,subject_name,degree_name)%>%
  distinct()%>%filter(!is.na(cap_id))%>%arrange(degree_name,subject_name)

r1<-subj_caplist %>% unnest(subject_name) %>%
  group_by(cap_id) %>%
  mutate(counter=paste0("Subj_", str_pad(1:n(),2,"left","0"))) %>%
  spread(counter, subject_name)

r2<-subj_caplist %>% unnest(subject_name) %>%
  group_by(cap_id,degree_name) %>%
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

fid<-r1%>%tidyr::unite(id, cap_id,degree_name, sep = '_')%>%select(id)%>%distinct()%>%as.list()
preps<-cbind(r2,fid)

## Make RID as colname
preps<-tibble::column_to_rownames(preps, var = "id")
cols<-colnames(preps)
colsall<-cols[3:68]
preps1<-preps%>%select(-c(degree_name,cap_id))



preps1[is.na(preps1)] <- '9999'
preps1 %<>% mutate_at(colsall, funs(factor(.)))

gower.dist <- daisy(preps1, metric = c("gower"))

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

for(i in 2:50){
  
  pam_fit <- pam(gower.dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width (higher is better)

plot(1:50, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:50, sil_width)
#############################################




preps1$cluster<-cutree(hc2,3) ## based on below
preps1[preps1 ==9999] <- NA
factoextra::fviz_cluster(list(data = gower.dist, cluster = preps1$cluster))

clusterlist<-split(preps1,preps1$cluster)

############################# Evaluate Clusters ###########################

cluster1<-clusterlist$`1`%>%rownames_to_column("id")%>% mutate(cap_degree=id)%>%separate(id, into = c("cap_id","degree"))%>%
  mutate(finalcluster= if_else(
    cluster1$degree == "bachelors", "bachelors1",
    if_else( cluster1$degree=="doctorate", "doctorate1",
             if_else(cluster1$degree=="masters", "masters1",
                     if_else( cluster1$degree=="general", "general1",
                              if_else( cluster1$degree=="associates", "associates1",
                                       if_else( cluster1$degree=="certificate", "certficate1",
                                                if_else( cluster1$degree=="grad", "grad-certificate1","non-degree1")))))))    
    
         
         
         )

cluster2<-clusterlist$`2`%>%rownames_to_column("id")%>% mutate(cap_degree=id)%>%separate(id, into = c("cap_id","degree"))%>%
  mutate(finalcluster= if_else(
    cluster2$degree == "bachelors", "bachelors2",
    if_else( cluster2$degree=="doctorate", "doctorate2",
             if_else(cluster2$degree=="masters", "masters2",
                     if_else( cluster2$degree=="general", "general2",
                              if_else( cluster2$degree=="associates", "associates2",
                                       if_else( cluster2$degree=="certificate", "certficate2",
                                                if_else( cluster2$degree=="grad", "grad-certificate2","non-degree2")))))))    
    
    
    
  )



cluster3<-clusterlist$`3`%>%rownames_to_column("id")%>% mutate(cap_degree=id)%>%separate(id, into = c("cap_id","degree"))%>%
  mutate(finalcluster= if_else(
    cluster3$degree == "bachelors", "bachelors3",
    if_else( cluster3$degree=="doctorate", "doctorate3",
             if_else(cluster3$degree=="masters", "masters3",
                     if_else( cluster3$degree=="general", "general3",
                              if_else( cluster3$degree=="associates", "associates3",
                                       if_else( cluster3$degree=="certificate", "certficate3",
                                                if_else( cluster3$degree=="grad", "grad-certificate3","non-degree3")))))))    
    
    
    
  )

############################# Finalize Clusters ###########################

Clus1<-cluster1%>%select(cap_degree,finalcluster)%>%distinct()
Clus2<-cluster2%>%select(cap_degree,finalcluster)%>%distinct()
Clus3<-cluster3%>%select(cap_degree,finalcluster)%>%distinct()

Combined_Cluster<-rbind(Clus1,Clus2,Clus3)%>%mutate(id=cap_degree)%>%separate(id, into = c("cap_id","degree"))


