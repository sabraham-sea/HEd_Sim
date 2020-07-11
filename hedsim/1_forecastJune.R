require(tidyverse)
options(scipen = 9999)

######## Change parameters#########33
datechk<-'2020-05-31'  # Last date of previous month
period_rng<-30         # Duration of next month
###########################3
trf_daily_final<-readRDS('trf_daily_final_July.RDS')
trf_daily_clean<-trf_daily_final%>%mutate_at(c(12,14,16), ~replace(., is.na(.), 0))
cap_data_daily<-trf_daily_clean%>%filter(date<=datechk)%>%dplyr::select(cap_id, school_name,date,degree_name,subject_name,accepted_cpl_leads,accepted_cpl_appviews,accepted_cpc_clicks,accepted_cpc_leads,
                                                                             accepted_revenue,billable_revenue,revised_revenue,
                                                                             `daily_new$combined_daily.dcs_traffic`)%>%
    group_by(cap_id,school_name,date)%>%
    summarize(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
              cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%filter(!is.na(cap_id))%>%ungroup()

# cap_data_daily$views<-if_else(cap_data_daily$cpl_views>0,cap_data_daily$cpl_views,cap_data_daily$cpc_clicks)
# cap_data_daily$leads<-if_else(cap_data_daily$cpl_views>0,cap_data_daily$cpl_leads,cap_data_daily$cpc_leads)

pricelist<-trf_daily_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date>=startdate &  date<= datechk & accepted_revenue>0)%>%
  arrange(cap_id,desc(date))%>%select(-date)%>%distinct()



cpl_list<-readRDS('cpl_listJune.RDS')
caplist<-cpl_list$cap_id


pred_forecast_cpl<-function(capid,datechk,period_rng)
{
    
    set.seed(1234)
    # Make traffic forecasts
    cap_traffic<-cap_data_daily%>%filter(cap_id==capid)%>%
        filter(date <= datechk )%>% 
        rename(y=imptrf, ds=date)
    
    # Make traffic forecasts
    trf_prf <- prophet(cap_traffic, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000,interval.width = 0.80)
    trf_daily <- make_future_dataframe(trf_prf , periods = period_rng,freq="day")
    trf_daily_forecast <- predict(trf_prf, trf_daily)
   # Make appview forecasts
    cap_views<-cap_data_daily%>%
        filter(cap_id==capid)%>%filter(date <= datechk )%>%
        rename(y=cpl_views, ds=date)
    
    views_prf <- prophet(cap_views, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000,interval.width = 0.80)
    views_daily <- make_future_dataframe(views_prf , periods = period_rng,freq="day")
    views_daily_forecast <- predict(views_prf, views_daily)
    ### Add traffic and views to cpl_leads
    
    leads_cpl<-cap_data_daily%>%
        filter(cap_id==capid)%>%filter(date <= datechk )%>%
        rename(y=cpl_leads, ds=date)
     cpl_prf <- prophet(leads_cpl, daily.seasonality='auto', weekly.seasonality='auto' ,mcmc.samples = 1000,interval.width = 0.80,fit=FALSE,iter=3000)
    cpl_prf<-add_country_holidays(cpl_prf,country_name='US')
    cpl_daily<-add_regressor(cpl_prf,name='imptrf')
    cpl_daily<-add_regressor(cpl_prf,name='cpl_views')
    cpl_prf2<- fit.prophet(cpl_daily, leads_cpl)
  
    leads_df <- make_future_dataframe(cpl_prf2, periods = period_rng,freq="day")
    df_all <- leads_df%>%left_join(dplyr::select(trf_daily_forecast,yhat,ds), by = 'ds')%>%rename(imptrf=yhat)
    df_all <- df_all  %>% left_join(dplyr::select(views_daily_forecast, yhat,ds), by = 'ds')%>%rename(cpl_views=yhat)
    forecast_cpl <- predict(cpl_prf2, df_all)
    nextmnth<-forecast_cpl%>%tail(period_rng)
    return(sum(nextmnth$yhat))
    
}

#### GET FORECAST OF LEADS #####

##  FINAL using cluster
### Loop thru all cap-ids

n1<-NROW(caplist)
cc<-head(caplist,n=n1)%>%as.data.frame()%>%rename(cap_id= ".")

library(doParallel)
library(prophet)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('cc','cap_data_daily','datechk','period_rng'), envir = environment())
system.time(forcast_leads <- parLapply(cl,cc$cap_id,pred_forecast_cpl,datechk,period_rng))
# system.time(forcast_leads1 <- mclapply(cc$cap_id,pred_forecast,datechk,period_rng,mc.cores = 7))
stopCluster(cl)

### Multiply by CPL 
forcast_leads1<-forcast_leads%>%unlist()%>%as.data.frame()%>%rename(forcast= ".")
forcast_leads1<-cbind(forcast_leads1,cc)
billable_leadrevenue_newrun<-left_join(forcast_leads1,cpl_list,by="cap_id")
billable_leadrevenue_newrun<-billable_leadrevenue_newrun%>%mutate(forcast_billable=forcast*accepted_revenue)%>%filter(!is.na(accepted_revenue))

saveRDS(billable_leadrevenue_newrun,"cpl_leadrevenue_Junepred.RDS")





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

