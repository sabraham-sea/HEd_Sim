require(tidyverse)
options(scipen = 9999)

######## Change parameters#########
`%not_in%` = Negate(`%in%`)
startdate<-'2020-07-01'
datechk<-'2020-07-13'  # Last date of previous month
period_rng<-50        # Duration of remainder this month + Duration of next month
###########################3
trf_daily_final<-readRDS('trf_daily_final_July14.RDS')
CDM_appview_clean<-readRDS('CDM_appview_clean.RDS')

trf_daily_clean<-trf_daily_final
cap_data_daily<-trf_daily_clean%>%filter(date<=datechk)%>%dplyr::select(date,cap_id,provider_id,school_id, school_name,date,degree_name,subject_name,cpl_leads,cpl_views,cpc_clicks,cpc_leads,imptraf
                                                                           )%>%
    group_by(cap_id,school_name,date)%>%
    summarize(cpl_leads=sum(cpl_leads),cpl_views=sum(cpl_views),cpc_clicks=sum(cpc_clicks),
              cpc_leads=sum(cpc_leads),imptrf=sum(imptraf))%>%filter(!is.na(cap_id))%>%ungroup()

# cap_data_daily$views<-if_else(cap_data_daily$cpl_views>0,cap_data_daily$cpl_views,cap_data_daily$cpc_clicks)
# cap_data_daily$leads<-if_else(cap_data_daily$cpl_views>0,cap_data_daily$cpl_leads,cap_data_daily$cpc_leads)

pricelist<-CDM_appview_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date>=startdate &  date<= datechk & accepted_revenue>0)%>%
  arrange(cap_id,desc(date))%>%select(-date)%>%distinct()

pricelist<-pricelist[!duplicated(pricelist$cap_id),]

# Check for cases that may have not enough data points
n_traffic<-trf_daily_clean%>%
  filter(date <= datechk )%>% group_by(cap_id)%>%count()

cap_traffic<-cap_data_daily%>%filter(date <= datechk )%>% group_by(cap_id)%>%count()



cpl_list<-readRDS('cpl_listAug.RDS')
#remove '3683 and 3685 3681'
cpl_list<-cpl_list%>%filter(cap_id %not_in% c('3683','3685','3681'))
saveRDS(cpl_list,'cpl_listAug.RDS')
caplist<-cpl_list$cap_id


# Check for cases that may have not enough data points
n_traffic<-trf_daily_clean%>%
  filter(date <= datechk )%>% group_by(cap_id)%>%count()

cap_traffic<-cap_data_daily%>%filter(date <= datechk & cap_id %in% cpl_list$cap_id)%>% group_by(cap_id)%>%count()


pred_forecast_cpl<-function(capid,datechk,period_rng,futuredate1,futuredate2)
{
    
    set.seed(1234)
    # Make traffic forecasts
    cap_traffic<-cap_data_daily%>%filter(cap_id==capid)%>%
        filter(date <= datechk )%>% 
        rename(y=imptrf, ds=date)
    
    # Make traffic forecasts
    trf_prf <- prophet(cap_traffic, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 100,interval.width = 0.80)
    trf_daily <- make_future_dataframe(trf_prf , periods = period_rng,freq="day", include_history = FALSE)
    
    trf_daily_forecast <- predict(trf_prf, trf_daily)
   # Make appview forecasts
    cap_views<-cap_data_daily%>%
        filter(cap_id==capid)%>%filter(date <= datechk )%>%
        rename(y=cpl_views, ds=date)
    
    views_prf <- prophet(cap_views, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 100,interval.width = 0.80)
    views_daily <- make_future_dataframe(views_prf , periods = period_rng,freq="day",include_history = FALSE)
    views_daily_forecast <- predict(views_prf, views_daily)
    ### Add traffic and views to cpl_leads
    
    leads_cpl<-cap_data_daily%>%
        filter(cap_id==capid)%>%filter(date <= datechk )%>%
        rename(y=cpl_leads, ds=date)
     cpl_prf <- prophet(leads_cpl, daily.seasonality='auto', weekly.seasonality='auto' ,mcmc.samples = 100,interval.width = 0.80,fit=FALSE,iter=3000)
    cpl_prf<-add_country_holidays(cpl_prf,country_name='US')
    cpl_daily<-add_regressor(cpl_prf,name='imptrf')
    cpl_daily<-add_regressor(cpl_prf,name='cpl_views')
    cpl_prf2<- fit.prophet(cpl_daily, leads_cpl)
  
    leads_df <- make_future_dataframe(cpl_prf2, periods = period_rng,freq="day",include_history = FALSE)
    df_all <- leads_df%>%left_join(dplyr::select(trf_daily_forecast,yhat,ds), by = 'ds')%>%rename(imptrf=yhat)
    df_all <- df_all  %>% left_join(dplyr::select(views_daily_forecast, yhat,ds), by = 'ds')%>%rename(cpl_views=yhat)
    forecast_cpl <- predict(cpl_prf2, df_all)
    nextmnth<-forecast_cpl%>%filter(ds>=futuredate1 & ds<= futuredate2)
    #hh<-forecast_cpl%>%group_by(month(ds),year(ds))%>%summarise(tot=sum(yhat),toth=sum(yhat_upper))
    return(list(sum(nextmnth$yhat),sum(nextmnth$yhat_upper)))
    
}

#### GET FORECAST OF LEADS #####

##  FINAL using cluster
### Loop thru all cap-ids

n1<-NROW(caplist)
cc<-head(caplist,n=n1)%>%as.data.frame()%>%rename(cap_id= ".")
futuredat1<-'2020-08-01'
futuredat2<-'2020-08-31'

library(doParallel)
library(prophet)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('cc','cap_data_daily','datechk','period_rng','futuredat1','futuredat2'), envir = environment())
system.time(forcast_leads <- parLapply(cl,cc$cap_id,pred_forecast_cpl,datechk,period_rng,futuredat1,futuredat2))
# system.time(forcast_leads1 <- mclapply(cc$cap_id,pred_forecast,datechk,period_rng,mc.cores = 7))
stopCluster(cl)

### Multiply by CPL 

forcast_leads_yhat<-sapply(forcast_leads, "[[", 1)%>%as.data.frame()%>%rename(forcast= ".")
forcast_leads_upper<-sapply(forcast_leads, "[[", 2)%>%as.data.frame()%>%rename(forcast_upper= ".")

forcast_leads1<-cbind(forcast_leads_yhat,forcast_leads_upper)
forcast_leads1<-cbind(forcast_leads1,cc)
billable_leadrevenue_newrun<-left_join(forcast_leads1,cpl_list,by="cap_id")
billable_leadrevenue_newrun<-billable_leadrevenue_newrun%>%mutate(forcast_billable=forcast*billable_revenue, forcast_billable_upper =forcast_upper*billable_revenue)%>%filter(!is.na(billable_revenue))

saveRDS(billable_leadrevenue_newrun,"cpl_leadrevenue_Augpred2.RDS")





pred_forecast_cpc<-function(capid,datechk,period_rng)
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
  
  views_prf <- prophet(cap_views, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000,interval.width = 0.80)
  views_daily <- make_future_dataframe(views_prf , periods = period_rng,freq="day")
  views_daily_forecast <- predict(views_prf, views_daily)
  ### Add traffic and views to cpl_leads
  
  leads_cpl<-cap_data_daily%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=cpc_leads, ds=date)
  cpl_prf <- prophet(leads_cpl, daily.seasonality='auto', weekly.seasonality='auto' ,mcmc.samples = 1000,interval.width = 0.80,fit=FALSE,iter=3000)
  cpl_prf<-add_country_holidays(cpl_prf,country_name='US')
  # cpl_daily<-add_regressor(cpl_prf,name='imptrf')
  cpl_daily<-add_regressor(cpl_prf,name='cpc_clicks')
  cpl_prf2<- fit.prophet(cpl_daily, leads_cpl)
  
  leads_df <- make_future_dataframe(cpl_prf2, periods = period_rng,freq="day")
  # df_all <- leads_df%>%left_join(dplyr::select(trf_daily_forecast,yhat,ds), by = 'ds')%>%rename(imptrf=yhat)
  df_all <- leads_df  %>% left_join(dplyr::select(views_daily_forecast, yhat,ds), by = 'ds')%>%rename(cpc_clicks=yhat)
  forecast_cpc <- predict(cpl_prf2, df_all)
  nextmnth<-forecast_cpc%>%tail(period_rng)
  return(sum(nextmnth$yhat))
  
}


cpc_list<-readRDS('cpc_listJune.RDS')
caplist<-cpc_list$cap_id

n1<-NROW(caplist)
cc<-head(caplist,n=n1)%>%as.data.frame()%>%rename(cap_id= ".")

no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('cc','cap_data_daily','datechk','period_rng'), envir = environment())
system.time(forcast_leads <- parLapply(cl,cc$cap_id,pred_forecast_cpc,datechk,period_rng))
# system.time(forcast_leads1 <- mclapply(cc$cap_id,pred_forecast,datechk,period_rng,mc.cores = 7))
stopCluster(cl)

### Multiply by CPL 
forcast_leads1<-forcast_leads%>%unlist()%>%as.data.frame()%>%rename(forcast= ".")
forcast_leads1<-cbind(forcast_leads1,cc)
billable_leadrevenue_newrun<-left_join(forcast_leads1,cpc_list,by="cap_id")
billable_leadrevenue_cpc<-billable_leadrevenue_newrun%>%mutate(forcast_billable=forcast*accepted_revenue)%>%filter(!is.na(accepted_revenue))

saveRDS(billable_leadrevenue_cpc,"cpc_leadrevenue_Junepred.RDS")

