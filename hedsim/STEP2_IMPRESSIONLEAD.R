### MODEL IMPRESSION~LEAD RELATIONSHIP ###

overall_cap<-trf_daily_clean%>%filter(date<='2020-04-30')%>%mutate(yrmnth=zoo::as.yearmon(date))%>%
  group_by(yrmnth,cap_id)%>%
  summarise(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
            cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%
  ungroup()%>%filter(!is.na(cap_id))%>%mutate(ctr=(cpl_views/imptrf),cr=(cpl_leads/cpl_views))

# Combine with cpl

overall_cap<-left_join(overall_cap,pricelist,by="cap_id")%>%filter(!is.na(accepted_revenue))



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
system.time(unlist <- parLapply(cl,cc$cap_id,implead))
stopCluster(cl)


#### PREDICT IMPRESSION USING BILLABLE LEAD REVENUE LEAD COUNT ####


## 1 case
delta_imp1<-predict(result_implead[[1]], new=data.frame(cpl_leads=billable_leadrevenue$forcast[1]))

# Loop thru all cases
pred_imp<-list()
i=1
for (i in 1:5)
{
pred_imp[[i]]<-predict(result_implead[[i]], new=data.frame(cpl_leads=billable_leadrevenue$forcast[i]))
i=i+1
}

predicted_impre<-pred_imp%>%unlist()%>%cbind(cc)%>%rename( 'pred_imp'='.')





