# 13 clusters - prob: some clusters gps are single

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

preps1$cluster<-cutree(hc2,13) ## based on below
preps1[preps1 ==9999] <- NA
factoextra::fviz_cluster(list(data = gower.dist, cluster = preps1$cluster))  # LOOK AT THE PLOT FOR IMMEDIDATE NEIGHBOURS



############################# Evaluate Clusters

## Overll cluster
clusterlist<-split(preps1,preps1$cluster)

cluster1<-clusterlist$`1`%>%rownames_to_column("id")%>% separate(id, into = c("cap_id","degree"))

# Since cluster 1 is big : split by degree

cluster1_bach<-cluster1%>%filter(degree=="bachelors")
cluster1_subjects<-clusterlist$`1`%>%rownames_to_column("id")%>%select(-cluster)%>%
  pivot_longer( cols = colsall,names_to = 'cap_id')%>%select(id,value)%>%distinct()%>%filter(!is.na(value))%>%separate(id,c("id","degree"))%>%
  arrange(degree,value)%>%group_by(degree,value)




cluster2<-clusterlist$`2`%>%rownames_to_column("id")%>% separate(id, into = c("cap_id","degree")) 
cluster2_subjects<-clusterlist$`2`%>%rownames_to_column("id")%>%select(-cluster)%>%
  pivot_longer( cols = colsall,names_to = 'cap_id')%>%select(id,value)%>%distinct()%>%filter(!is.na(value))%>%separate(id,c("id","degree"))

cluster3<-clusterlist$`3`%>%rownames_to_column("id")%>% separate(id, into = c("cap_id","degree")) 
cluster3_subjects<-clusterlist$`3`%>%rownames_to_column("id")%>%select(-cluster)%>%
  pivot_longer( cols = colsall,names_to = 'cap_id')%>%select(id,value)%>%distinct()%>%filter(!is.na(value))%>%separate(id,c("id","degree"))

cluster4<-clusterlist$`4`%>%rownames_to_column("id")%>% separate(id, into = c("cap_id","degree")) 
cluster4_subjects<-clusterlist$`4`%>%rownames_to_column("id")%>%select(-cluster)%>%
  pivot_longer( cols = colsall,names_to = 'cap_id')%>%select(id,value)%>%distinct()%>%filter(!is.na(value))%>%separate(id,c("id","degree"))

cluster10<-clusterlist$`10`%>%rownames_to_column("id")%>% separate(id, into = c("cap_id","degree")) 
cluster10_subjects<-clusterlist$`10`%>%rownames_to_column("id")%>%select(-cluster)%>%
  pivot_longer( cols = colsall,names_to = 'cap_id')%>%select(id,value)%>%distinct()%>%filter(!is.na(value))%>%separate(id,c("id","degree"))


cluster13<-clusterlist$`13`%>%rownames_to_column("id")%>% separate(id, into = c("cap_id","degree")) 
cluster6<-clusterlist$`6`%>%rownames_to_column("id")%>% separate(id, into = c("cap_id","degree")) 

cluster11<-preps1%>%filter(cluster==11)
cluster2<-preps1%>%filter(cluster==2)
cluster3<-preps1%>%filter(cluster==3)
cluster4<-preps1%>%filter(cluster==4)
cluster14<-preps1%>%filter(cluster==14)