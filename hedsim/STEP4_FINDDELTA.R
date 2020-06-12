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

deltalead<-delta_lead('856',600000) # enter capid, and final billable

# return index of 856 in cc
ind<-which(cc$cap_id=='856')

#Predict the associated impressions of delta lead 

allocatable_imp<-predict(result_implead[[ind]],new=data.frame(cpl_leads=deltalead))


### Find which cluster group

clustno<-Combined_Cluster%>%filter(cap_id=='856')
clustcap<-Combined_Cluster%>%filter(clusno==clustno$clusno)%>%select(cap_id)


### For all those capids in cluscap get leads from billable_leadrevenue

# Eg 2273 ; Get impression from pred_imp

# Get predicted erpi for 2273

Combined_baseline<-cbind(predicted_impression,predicted_epri,billable_leadrevenue)



