
billable_leadrevenue<-tbillable_leadrevenue
# Find baseline

rsquared_erpi<-rsquared_erpi%>%unlist()%>%as.data.frame()%>%rename('rsq_erpi'='.')
rsquared_imp<-rsquared_imp%>%unlist()%>%as.data.frame()%>%rename('rsq_imp'='.')
Combined_baseline<-cbind(predicted_impression,predicted_erpi,tbillable_leadrevenue,rsquared_erpi,rsquared_imp)
Combined_baseline$forcast_billrev<-Combined_baseline$pred_imp*Combined_baseline$pred_erpi
Combined_baseline<-Combined_baseline[!duplicated(as.list(Combined_baseline))]
# FIND DELTA LEAD

delta_lead<-function(capid,finalbillable)
  
{
  dat<-billable_leadrevenue%>%filter(cap_id==capid)
  revdiff<-dat$forcast_billable-finalbillable
  return(revdiff/dat$accepted_revenue)
}

#  GET DELTA IMPRESSION


implead<-function(capid)
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  model_implead<-lm(imptrf~poly(cpl_leads,2,raw=TRUE),data=model_data)
  return(model_implead)
}

deltalead<-delta_lead('3143',600000) # enter capid, and final billable

# return index of 856 in cc
ind<-which(final_list$cap_id=='3143')

#Predict the associated impressions of delta lead 

allocatable_imp<-predict(result_implead[[ind]],new=data.frame(cpl_leads=deltalead))


### Find which cluster group

clustno<-Combined_Cluster%>%filter(cap_id=='856')
clustcap<-Combined_Cluster%>%filter(clusno==clustno$clusno)%>%select(cap_id)


### For all those capids in cluscap get leads from billable_leadrevenue

# Eg 2273 ; Get impression from pred_imp

# Get predicted erpi for 2273


# Adjust the allocation



