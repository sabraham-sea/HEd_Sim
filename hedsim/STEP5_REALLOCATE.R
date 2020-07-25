`%not_in%` = Negate(`%in%`)



capfrom<-'3143'
# Find which cluster capfrom belongs to
clustno<-Combined_Cluster%>%filter(cap_id ==capfrom)%>%select(clusno)
# Find other capids in same cluster
capto<-Combined_Cluster%>%filter(clusno==clustno$clusno)%>%select(cap_id)%>%filter(cap_id %not_in% capfrom)


impression_allocation<-impression_allocation%>%filter(cap_id %in% c(capfrom,capto$cap_id))
newrevlimit<-815788


# erpi_cap<-trf_daily_clean%>%filter(date<='2020-04-30')%>%mutate(yrmnth=zoo::as.yearmon(date))%>%
#   group_by(cap_id)%>%
#   summarise(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
#             cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%
#   ungroup()%>%filter(!is.na(cap_id))%>%mutate(ctr=(cpl_views/imptrf),cr=(cpl_leads/cpl_views))
# 
# erpi_cap<-left_join(erpi_cap,pricelist,by="cap_id")%>%filter(!is.na(accepted_revenue))
# 
# finalcap<-finalcaplist%>%as.data.frame()%>%rename("cap_id"='.')
# erpi_cap<-right_join(erpi_cap,finalcap,by="cap_id")%>%mutate(erpi=ctr*cr*accepted_revenue)

erpi_avg<-DE_MCData3%>%group_by(school_capid)%>%summarise(avg_erpi=mean(avg_erpi))%>%rename("cap_id"="school_capid")
erpi_avg$cap_id<-as.character(erpi_avg$cap_id)

Combined_baseline<-left_join(Combined_baseline,erpi_avg,by=c("cap_id"))

# Combine with cpl


billable_leadrevenue<-readRDS('cpl_Junepred.RDS')


get_return <- function(capfrom,capto,newrevlimit) {
  
 
  alloc_gp<-billable_leadrevenue%>%filter(cap_id %in% c(capfrom,capto))
  deltalead <- delta_lead(capfrom,newrevlimit)
  ind<-which(finalcaplist==capfrom)
  allocatable_imp<-predict(result_implead[[ind]],new=data.frame(cpl_leads=deltalead))
  
  #final lead
  changed_baseline<-Combined_baseline%>%mutate(newlead=forcast)
  changed_baseline$newlead[ind]<-changed_baseline$forcast[ind]-deltalead
  #final impression
  new_imp<-predict(result_implead[[ind]],new=data.frame(cpl_leads=changed_baseline$newlead[ind]))
  
  changed_baseline$newimp<-changed_baseline$pred_imp
  changed_baseline$newimp[ind]<-new_imp
  
  alloc_gpfrom<-changed_baseline%>%filter(cap_id ==capfrom)
  alloc_gpto<-changed_baseline%>%filter(cap_id %in% capto$cap_id)
  alloc_gp<-rbind(alloc_gpfrom,alloc_gpto)
  
  alloc_gp<-left_join(alloc_gp,erpi_avg,by="cap_id")
  
  newall<-alloc_gp[-1,]
  
  
  # Set coefficients of the objective function
 
  f.obj <- newall$avg_erpi
  
  # Set matrix corresponding to coefficients of constraints by rows
  # Do not consider the non-negative constraint; it is automatically assumed
  
  ## x1+x2+x3 <=sum of impressions
  ## they should all have their initial impression share as llower bounds
  ## additional constrains for upper as in snhu will only take upto 20% more of their imp
  f.con <- matrix(c(1,1,1,     
                    1,0,0,
                    0,1,0,
                    0,0,1)
                  
                    , nrow = 4, byrow = TRUE)
  
  conts1=sum(newall$newimp,allocatable_imp)
  
  # Set unequality signs
  f.dir <- c("<=",">=",">=","=")   # Budget constraint for 3rd 
  
  
  # Set right hand side coefficients
  f.rhs <- c(conts1,44906.91,133359.78,11374.02) #44906.91 - old impression * 1.5
   
  
lp("max", f.obj, f.con, f.dir, f.rhs)$solution          
newrev=lp("max", f.obj, f.con, f.dir, f.rhs)
revcapfrom=  alloc_gp$newimp[1]*alloc_gp$erpi[1]
finalrev<-sum(newrev$objval,revcapfrom)
}
