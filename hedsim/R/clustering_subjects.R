## To find cap in cluster ##

subj_caplist<-trf_daily%>%select(cap_id,subject_name)%>%
  distinct()%>%filter(!is.na(cap_id))

tr_subcap<-subj_caplist%>%pivot_wider(names_from = cap_id, values_from =subject_name)%>%
  pivot_longer( cols = `1002`:`997`,names_to = 'cap_id')

r<-tr_subcap %>% unnest(value) %>%
  group_by(cap_id) %>%
  mutate(counter=paste0("Subj_", str_pad(1:n(),2,"left","0"))) %>%
  spread(counter, value)


fid<-r%>%tidyr::unite(id, cap_id, sep = '_')%>%select(id)%>%distinct()%>%as.list()
preps<-cbind(r,fid)

## Make RID as colname
preps<-tibble::column_to_rownames(preps, var = "id")
cols<-colnames(preps)
colsall<-cols[2:72]
preps1<-preps%>%select(-c(cap_id))



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

preps1$cluster<-cutree(hc2,14)  ## based on below
factoextra::fviz_cluster(list(data = gower.dist, cluster = preps1$cluster))
cluster1<-preps1%>%filter(cluster==1)

