require(tidyverse)
options(scipen = 9999)

######## Change parameters#########
`%not_in%` = Negate(`%in%`)
startdate<-'2020-07-01'
datechk<-'2020-07-22'  # Last date of previous month
period_rng<-50        # Duration of remainder this month + Duration of next month
###########################3
trf_daily_final<-readRDS('trf_daily_final_July23.RDS')
CDM_appview_clean<-readRDS('CDM_appview_clean1.RDS')

trf_daily_clean<-trf_daily_final
cap_data_daily<-trf_daily_clean%>%filter(date<=datechk)%>%dplyr::select(date,cap_id,provider_id,school_id, school_name,date,degree_name,subject_name,cpl_leads,cpl_views,cpc_clicks,cpc_leads,imptraf
)%>%
  group_by(cap_id,school_name,date)%>%
  summarize(cpl_leads=sum(cpl_leads),cpl_views=sum(cpl_views),cpc_clicks=sum(cpc_clicks),
            cpc_leads=sum(cpc_leads),imptrf=sum(imptraf))%>%filter(!is.na(cap_id))%>%ungroup()

# cap_data_daily$views<-if_else(cap_data_daily$cpl_views>0,cap_data_daily$cpl_views,cap_data_daily$cpc_clicks)
# cap_data_daily$leads<-if_else(cap_data_daily$cpl_views>0,cap_data_daily$cpl_leads,cap_data_daily$cpc_leads)

# pricelist<-CDM_appview_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date>=startdate &  date<= datechk & accepted_revenue>0)%>%
#   arrange(cap_id,desc(date))%>%select(-date)%>%distinct()

pricelist<-CDM_appview_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date<= datechk & accepted_revenue>0)%>%
  arrange(cap_id,desc(date))%>%select(-date)%>%distinct()

pricelist<-pricelist[!duplicated(pricelist$cap_id),]



## Find current cpl/LO cases for August ##
current_cap<-readRDS('Current_caps.RDS')
current_cap<-current_cap%>%filter(Product=='LO')

# Check for cases that may have not enough data points

cap_traffic<-cap_data_daily%>%filter(date <= datechk )%>% group_by(cap_id)%>%count()
cap_chk_count<-inner_join(cap_traffic,current_cap,by='cap_id')
final_list<-cap_chk_count%>%filter(n>1)

cpl_list<-final_list
# cpl_list<-readRDS('cpl_listAug.RDS')
# 
# 
# #remove '3683 and 3685 3681'
# cpl_list<-cpl_list%>%filter(cap_id %not_in% c('3683','3685','3681'))
# saveRDS(cpl_list,'cpl_listAug.RDS')
caplist<-final_list$cap_id

# caplimit<-final_list%>%select(cap_id,`Allowable Cap`)
# 


pred_forecast_cpl<-function(capid,datechk,period_rng,futuredate1,futuredate2,thisdate)
{
  
  set.seed(1234)
  # Make traffic forecasts
  cap_traffic<-cap_data_daily%>%filter(cap_id==capid)%>%
    filter(date <= datechk )%>% 
    rename(y=imptrf, ds=date)
  
  # capamnt<-caplimit%>%filter(cap_id==capid)%>%ungroup()%>%select(`Allowable Cap`)
  # 
  # Make traffic forecasts
  trf_prf <- prophet(cap_traffic, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 100,interval.width = 0.80)
  trf_daily <- make_future_dataframe(trf_prf , periods = period_rng,freq="day")
  
  trf_daily_forecast <- predict(trf_prf, trf_daily)
  # Make appview forecasts
  cap_views<-cap_data_daily%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=cpl_views, ds=date)
  
  views_prf <- prophet(cap_views, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 100,interval.width = 0.80)
  views_daily <- make_future_dataframe(views_prf , periods = period_rng,freq="day")
  views_daily_forecast <- predict(views_prf, views_daily)
  ### Add traffic and views to cpl_leads
  
  leads_cpl<-cap_data_daily%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=cpl_leads, ds=date)

  cpl_prf <- prophet(leads_cpl, daily.seasonality='auto', weekly.seasonality='auto' ,mcmc.samples = 100,interval.width = 0.80,fit=FALSE,iter=3000,algorithm='Newton')
  cpl_prf<-add_country_holidays(cpl_prf,country_name='US')
  cpl_daily<-add_regressor(cpl_prf,name='imptrf')
  cpl_daily<-add_regressor(cpl_prf,name='cpl_views')
  cpl_prf2<- fit.prophet(cpl_daily, leads_cpl)
  
  leads_df <- make_future_dataframe(cpl_prf2, periods = period_rng,freq="day")
  # leads_df$cap<-capamnt$`Allowable Cap`
  df_all <- leads_df%>%left_join(dplyr::select(trf_daily_forecast,yhat,ds), by = 'ds')%>%rename(imptrf=yhat)
  df_all <- df_all  %>% left_join(dplyr::select(views_daily_forecast, yhat,ds), by = 'ds')%>%rename(cpl_views=yhat)
  forecast_cpl <- predict(cpl_prf2, df_all)
  nextmnth<-forecast_cpl%>%filter(ds>=futuredate1 & ds<= futuredate2)
  thismnth<-forecast_cpl%>%filter(ds>=thisdate & ds <= futuredate1)
  #hh<-forecast_cpl%>%group_by(month(ds),year(ds))%>%summarise(tot=sum(yhat),toth=sum(yhat_upper))
  return(list(sum(nextmnth$yhat),sum(nextmnth$yhat_lower),sum(nextmnth$yhat_upper)))
  
}

#### GET FORECAST OF LEADS #####

##  FINAL using cluster
### Loop thru all cap-ids

n1<-NROW(caplist)
cc<-head(caplist,n=n1)%>%as.data.frame()%>%rename(cap_id= ".")
thisdate <-'2020-06-30'
futuredate1<-'2020-07-31'
futuredate2<-'2020-08-31'

library(doParallel)
library(prophet)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores,type="FORK")
clusterExport(cl,list('cc','cap_data_daily','datechk','period_rng','futuredate1','futuredate2','thisdate'), envir = environment())
system.time(forcast_leads <- parLapply(cl,cc$cap_id,pred_forecast_cpl,datechk,period_rng,futuredate1,futuredate2,thisdate))
# system.time(forcast_leads1 <- mclapply(cc$cap_id,pred_forecast,datechk,period_rng,mc.cores = 7))
stopCluster(cl)

### Multiply by CPL 

forcast_leads_yhat<-sapply(forcast_leads, "[[", 1)%>%as.data.frame()%>%rename(forcast= ".")
forcast_leads_lower<-sapply(forcast_leads, "[[", 2)%>%as.data.frame()%>%rename(forcast_lower= ".")
forcast_leads_upper<-sapply(forcast_leads, "[[", 3)%>%as.data.frame()%>%rename(forcast_upper= ".")

forcast_leads1<-cbind(forcast_leads_yhat,forcast_leads_lower,forcast_leads_upper,cc)


billable_leadrevenue_newrun<-left_join(forcast_leads1,cpl_list,by="cap_id")
billable_leadrevenue_newrun<-left_join(billable_leadrevenue_newrun,pricelist,by="cap_id")

#billable_leadrevenue_newrun<-readRDS('cpl_leadrevenue_Augpred8.RDS')


saveRDS(billable_leadrevenue_newrun,"cpl_leadrevenue_Augpred10.RDS")





pred_forecast_cpc<-function(capid,datechk,period_rng,futuredate1,futuredate2,thisdate)
{
  
  set.seed(1234)
  
  # Make traffic forecasts
  # cap_traffic<-cap_data_daily%>%filter(cap_id==capid)%>%
  #   filter(date <= datechk )%>% 
  #   rename(y=imptrf, ds=date)
  # 
  # # Make traffic forecasts
  # trf_prf <- prophet(cap_traffic, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000,interval.width = 0.80)
  # trf_daily <- make_future_dataframe(trf_prf , periods = period_rng,freq="day")
  # trf_daily_forecast <- predict(trf_prf, trf_daily)
  
  # Make appview forecasts
  cap_views<-cap_data_daily%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=cpc_clicks, ds=date)
  
  views_prf <- prophet(cap_views, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 100,interval.width = 0.80)
  views_daily <- make_future_dataframe(views_prf , periods = period_rng,freq="day")
  views_daily_forecast <- predict(views_prf, views_daily)
  ### Add traffic and views to cpl_leads
  
  leads_cpl<-cap_data_daily%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=cpc_leads, ds=date)
  cpl_prf <- prophet(leads_cpl, daily.seasonality='auto', weekly.seasonality='auto' ,mcmc.samples = 100,interval.width = 0.80,fit=FALSE,iter=3000,algorithm='Newton')
  cpl_prf<-add_country_holidays(cpl_prf,country_name='US')
  # cpl_daily<-add_regressor(cpl_prf,name='imptrf')
  cpl_daily<-add_regressor(cpl_prf,name='cpc_clicks')
  cpl_prf2<- fit.prophet(cpl_daily, leads_cpl)
  
  leads_df <- make_future_dataframe(cpl_prf2, periods = period_rng,freq="day")
  # df_all <- leads_df%>%left_join(dplyr::select(trf_daily_forecast,yhat,ds), by = 'ds')%>%rename(imptrf=yhat)
  df_all <- leads_df  %>% left_join(dplyr::select(views_daily_forecast, yhat,ds), by = 'ds')%>%rename(cpc_clicks=yhat)
  forecast_cpc <- predict(cpl_prf2, df_all)
  nextmnth<-forecast_cpc%>%filter(ds>=futuredate1 & ds<= futuredate2)
  currmnth<-forecast_cpc%>%filter(ds>=thisdate & ds<= futuredate1)
  return(list(sum(nextmnth$yhat),sum(nextmnth$yhat_lower),sum(nextmnth$yhat_upper)))
  
  
}


## Find current cpc cases for August ##
current_cap<-readRDS('Current_caps.RDS')
current_cap<-current_cap%>%filter(Product!='LO')

# Check for cases that may have not enough data points

cap_traffic<-cap_data_daily%>%filter(date <= datechk )%>% group_by(cap_id)%>%count()
cap_chk_count<-inner_join(cap_traffic,current_cap,by='cap_id')
final_list<-cap_chk_count%>%filter(n>1)

cpc_list<-final_list

caplist<-final_list$cap_id
thisdate <-'2020-06-30'
futuredate1<-'2020-07-31'
futuredate2<-'2020-08-31'

n1<-NROW(caplist)
cc<-head(caplist,n=n1)%>%as.data.frame()%>%rename(cap_id= ".")

no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('cc','cap_data_daily','datechk','period_rng','futuredate1','futuredate2','thisdate'), envir = environment())
system.time(forcast_leads <- parLapply(cl,cc$cap_id,pred_forecast_cpc,datechk,period_rng,futuredate1,futuredate2,thisdate))
stopCluster(cl)


### Multiply by CPC
forcast_leads_yhat<-sapply(forcast_leads, "[[", 1)%>%as.data.frame()%>%rename(forcast= ".")
forcast_leads_lower<-sapply(forcast_leads, "[[", 2)%>%as.data.frame()%>%rename(forcast_lower= ".")
forcast_leads_upper<-sapply(forcast_leads, "[[", 3)%>%as.data.frame()%>%rename(forcast_upper= ".")

forcast_leads1<-cbind(forcast_leads_yhat,forcast_leads_lower,forcast_leads_upper,cc)


billable_leadrevenue_newrun<-left_join(forcast_leads1,cpc_list,by="cap_id")
billable_leadrevenue_newrun<-left_join(billable_leadrevenue_newrun,pricelist,by="cap_id")
# billable_leadrevenue_newrun<-readRDS('cpc_leadrevenue_Augpred7.RDS')

saveRDS(billable_leadrevenue_newrun,"cpc_leadrevenue_Augpred10.RDS")


# ADDING ERPIs

DE_MCData<-readRDS('DE_MCData3.RDS')
cpc_leadrevenue<-readRDS("cpc_leadrevenue_Augpred10.RDS")
cpl_leadrevenue<-readRDS("cpl_leadrevenue_Augpred10.RDS")
all<-rbind(cpc_leadrevenue,cpl_leadrevenue)

clean_erpi<- DE_MCData%>%filter(school_capid != 'NA')%>%rename("cap_id"="school_capid") %>% filter(month==month(startdate))# Problem data point

clean_erpi$cap_id<-as.character(clean_erpi$cap_id)
final_list<-inner_join(all,clean_erpi,by="cap_id")

saveRDS(final_list,"All_Augpred10.RDS")


## Add Cluster info ##

Final_Cluster<-readRDS('Final_Cluster.RDS')

Final_Cluster_clean<-Final_Cluster%>%filter(!(cap_id=='1751' & school_name=='southern-new-hampshire-university'))%>%select(cap_id,cluster,finalcluster)%>%distinct()

dt = data.table(Final_Cluster_clean)
# check for dupes
dtindex<-dt[duplicated(cap_id), cbind(.SD[1], number = .N), by = cap_id]
dtall<-dt[!duplicated(cap_id), cbind(.SD[1], number = .N), by = cap_id]

Final_Cluster_clean<-dtall%>%select(cap_id,cluster,finalcluster)
All_Augpred10<- readRDS("All_Augpred10.RDS")

All_Augpred10f<-left_join(All_Augpred10,Final_Cluster_clean,by="cap_id")

saveRDS(All_Augpred10f,"All_Augpred10_valattest.RDS")

