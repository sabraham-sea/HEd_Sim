
require(dplyr)
require(tidyr)
require(stringr)
require(cluster)
require(magrittr)
require(factoextra)
require(tibble)

trf_daily<-readRDS('trf_daily_final.RDS')

## To find cap in cluster ##

subj_caplist<-trf_daily%>%filter(date >= '2020-01-01')%>%select(cap_id,subject_name,degree_name)%>%
  distinct()%>%filter(!is.na(cap_id) & subject_name!= 'general')%>%arrange(degree_name,subject_name)

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
preps%<>%select(-c(degree_name,cap_id))

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




preps1$cluster<-cutree(hc2,4) ## based on above
preps1[preps1 ==9999] <- NA
factoextra::fviz_cluster(list(data = gower.dist, cluster = preps1$cluster))

clusterlist<-split(preps1,preps1$cluster)

############################# Evaluate Clusters ###########################

cluster1<-clusterlist$`1`%>%rownames_to_column("id")%>% mutate(cap_degree=id)%>%separate(id, into = c("cap_id","degree"))%>%
  mutate(finalcluster= if_else(
    degree == "bachelors", "bachelors_1",
    if_else( degree=="doctorate", "doctorate_1",
             if_else(degree=="masters", "masters_1",
                     if_else( degree=="general", "general_1",
                              if_else( degree=="associates", "associates_1",
                                       if_else( degree=="certificate", "certficate_1",
                                                if_else( degree=="grad", "grad-certificate_1","non-degree_1")))))))



         )

cluster2<-clusterlist$`2`%>%rownames_to_column("id")%>% mutate(cap_degree=id)%>%separate(id, into = c("cap_id","degree"))%>%
  mutate(finalcluster= if_else(
    degree == "bachelors", "bachelors_2",
    if_else( degree=="doctorate", "doctorate_2",
             if_else(degree=="masters", "masters_2",
                     if_else( degree=="general", "general_2",
                              if_else( degree=="associates", "associates_2",
                                       if_else( degree=="certificate", "certficate_2",
                                                if_else( degree=="grad", "grad-certificate_2","non-degree_2")))))))



  )



cluster3<-clusterlist$`3`%>%rownames_to_column("id")%>% mutate(cap_degree=id)%>%separate(id, into = c("cap_id","degree"))%>%
  mutate(finalcluster= if_else(
    degree == "bachelors", "bachelors_3",
    if_else( degree=="doctorate", "doctorate_3",
             if_else(degree=="masters", "masters_3",
                     if_else( degree=="general", "general_3",
                              if_else( degree=="associates", "associates_3",
                                       if_else( degree=="certificate", "certficate_3",
                                                if_else( degree=="grad", "grad-certificate_3","non-degree_3")))))))



  )


cluster4<-clusterlist$`4`%>%rownames_to_column("id")%>% mutate(cap_degree=id)%>%separate(id, into = c("cap_id","degree"))%>%
  mutate(finalcluster= if_else(
    degree == "bachelors", "bachelors_4",
    if_else( degree=="doctorate", "doctorate_4",
             if_else(degree=="masters", "masters_4",
                     if_else( degree=="general", "general_4",
                              if_else( degree=="associates", "associates_4",
                                       if_else( degree=="certificate", "certficate_4",
                                                if_else( degree=="grad", "grad-certificate4","non-degree_4")))))))



  )

############################# Finalize Clusters ###########################

Clus1<-cluster1%>%select(cap_degree,finalcluster)%>%distinct()
Clus2<-cluster2%>%select(cap_degree,finalcluster)%>%distinct()
Clus3<-cluster3%>%select(cap_degree,finalcluster)%>%distinct()
Clus4<-cluster4%>%select(cap_degree,finalcluster)%>%distinct()

Combined_Cluster<-rbind(Clus1,Clus2,Clus3,Clus4)%>%mutate(id=cap_degree)%>%separate(id, into = c("cap_id","degree"))%>%
separate(finalcluster, into = c('degree', 'cluster'), sep = '_', convert = TRUE)%>%arrange(cluster,degree)



saveRDS(Combined_Cluster,"Combined_Cluster.RDS")


############################### DISTRIBUTION OF CAP CLUSTER ############################

combinedcluster<-Combined_Cluster

combinedcluster$suffix<-str_sub(combinedcluster$degree,1,1)
combinedcluster$id<-combinedcluster$cluster

combinedcluster<-combinedcluster%>%unite("cluster",cluster,suffix)


tt<-left_join(gped_cap,combinedcluster,by=c("cap_id","degree"))%>%filter(degree!='general')

## Just April

tot<-tt%>%filter(month==4)%>%group_by(degree,cluster,id)%>%summarize(cpl_leads=sum(cpl_leads),
                                                                     cpl_views=sum( cpl_views),cpc_clicks=sum(cpc_clicks),cpc_leads=sum(cpc_leads),imptrf=sum(imptrf))%>%
  filter(!is.na(cap_id)& !is.na(cluster))

# Stacked + percent
ggplot(tot, aes(fill=cluster,y=cpl_leads, x=id)) +
  geom_bar(position="dodge", stat="identity")


## Looking at 2 b cluster

cl1tot<-tt%>%filter(month==4 & cluster=='2_b')%>%group_by(degree,cluster,cap_id)%>%summarize(cpl_leads=sum(cpl_leads),
                                                                                             cpl_views=sum( cpl_views),cpc_clicks=sum(cpc_clicks),cpc_leads=sum(cpc_leads),imptrf=sum(imptrf))%>%
  filter(!is.na(cap_id)& !is.na(cluster))

# Stacked + percent
ggplot(cl1tot, aes(fill=cluster,y=cpl_leads, x=cap_id)) +
  geom_bar(position="dodge", stat="identity")








