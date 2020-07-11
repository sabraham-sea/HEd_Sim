options(scipen = 99999)

billable_leadrevenue<-readRDS("cpl_leadrevenue_Junepred.RDS")
enddate<-'2020-05-31'

### MODEL IMPRESSION~LEAD RELATIONSHIP ###
trf_daily_final<-readRDS('trf_daily_final_July.RDS')
overall_cap<-trf_daily_final%>%filter(date<=enddate)%>%mutate(yrmnth=zoo::as.yearmon(date))%>%
  group_by(yrmnth,cap_id)%>%
  summarise(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
            cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%
  ungroup()%>%filter(!is.na(cap_id))%>%mutate(ctr=(cpl_views/imptrf),cr=(cpl_leads/cpl_views))

# Combine with cpl
final_list<-readRDS('cpl_listJune.RDS')
overall_cap<-inner_join(overall_cap,final_list,by="cap_id")%>%filter(!is.na(accepted_revenue))


implead<-function(capid)
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  # knots <- quantile(model_data$imptrf, p = c(0.25, 0.5))
  # model_implead<-lm(imptrf~bs(cpl_leads,knots=knots),data=model_data)
  model_implead<-lm(imptrf~poly(cpl_leads,2,raw=TRUE),data=model_data)
  #model_implead<-lm(imptrf~cpl_leads,data=model_data)
  return(model_implead)
}



library(doParallel)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('overall_cap'), envir = environment())
system.time(result_implead<- parLapply(cl,final_list$cap_id,implead))
stopCluster(cl)


#### PREDICT IMPRESSION USING BILLABLE LEAD REVENUE LEAD COUNT ####

# 
# ## 1 case
# delta_imp1<-predict(result_implead[[2]], new=data.frame(cpl_leads=billable_leadrevenue$forcast[2]))
# 
# billable_leadrevenue<-billable_leadrevenue_all%>%filter(cap_id %in% final_list$cap_id)


# Loop thru all cases
pred_imp<-list()
n2<-NROW(final_list)
i=1
for (i in 1:n2)
{
pred_imp[[i]]<-predict(result_implead[[i]], new=data.frame(cpl_leads=billable_leadrevenue$forcast[i]))
i=i+1
}

predicted_impression<-pred_imp%>%unlist()%>%cbind(final_list)%>%as.data.frame()%>%rename( 'pred_imp'='.')
predicted_impression$pred_imp<-as.numeric(predicted_impression$pred_imp)

predicted_impression_fin<-predicted_impression%>%select(pred_imp,cap_id)


require(rsq)
# Loop thru all cases
rsquared_imp<-list()
n2<-NROW(final_list)
i=1
for (i in 1:n2)
{
  rsquared_imp[[i]]<-rsq(result_implead[[i]])
  i=i+1
}

# Combine with billable_leadrevenue

billable_leadrevenue_wimp<-inner_join(billable_leadrevenue,predicted_impression_fin,by="cap_id")
saveRDS(billable_leadrevenue_wimp,"billable_leadrevenue_Junepred1.RDS")

