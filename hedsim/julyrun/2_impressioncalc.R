options(scipen = 99999)

billable_leadrevenue<-readRDS("cpl_leadrevenue_Junepred.RDS")
enddate<-'2020-05-31'



### MODEL IMPRESSION~LEAD RELATIONSHIP ###

avg_ctr_cr<-DE_MCData3%>%filter(month==month(datechk))%>%rename("cap_id"="school_capid")
avg_ctr_cr$cap_id<-as.character(avg_ctr_cr$cap_id)


# Combine forecast to get impressions

pred_imp<-left_join(billable_leadrevenue,avg_ctr_cr,by="cap_id")%>%mutate(pred_imp=forcast/(avg_ctr*avg_cr))
pred_imp1<-left_join(pred_imp,AM_names,by="cap_id")
Combined_Cluster <- readRDS("~/Documents/Github/HEd_Sim/hedsim/Combined_Cluster.RDS")
pred_imp2 <- left_join(pred_imp1,Combined_Cluster,by="cap_id")
saveRDS(pred_imp2,'cpl_Junepred.RDS')

