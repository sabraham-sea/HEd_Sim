clust1<-clusterlist$`1`%>%rownames_to_column("id")%>% separate(id, into = c("cap"))%>%
  pivot_wider(names_from = subject_name, values_from =cap)

subs<-colnames(clust1)
subs<-subs[-c(1,2)]

m<-clust1 %>% 
  pivot_longer(subs, names_to = "degree", values_to = "cases")





clust1_caps<-clusterlist$`1`%>%rownames_to_column("id")%>% separate(id, into = c("cap"))%>%
  distinct(cap)



## To find cap in cluster ##

subj_caplist<-trf_daily%>%select(cap_id,subject_name,degree_name)%>%
  distinct()%>%filter(!is.na(cap_id))

tr_subcap<-subj_caplist%>%pivot_wider(names_from = cap_id, values_from =subject_name)%>%
pivot_longer( cols = `1014`:`1767`,names_to = 'cap_id')

r<-tr_subcap %>% unnest(value) %>%
  group_by(cap_id) %>%
  mutate(counter=paste0("Subj_", str_pad(1:n(),2,"left","0"))) %>%
  spread(counter, value)


fid<-r%>%tidyr::unite(id, cap_id,degree_name, sep = '_')%>%select(id)%>%distinct()%>%as.list()
preps<-cbind(r,fid)

## Make RID as colname
preps<-tibble::column_to_rownames(preps, var = "id")
cols<-colnames(preps)
colsall<-cols[3:94]
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



