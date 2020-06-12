## STEP 1 FORECAST LEADS WITH REVENUE


pred_forecast<-function(capid,datechk,period_rng)
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
  # tail(trf_daily_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
  # plot(trf_prf, trf_daily_forecast)
  
  # Make appview forecasts
  cap_views<-cap_data_daily%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=cpl_views, ds=date)
  
  views_prf <- prophet(cap_views, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000,interval.width = 0.80)
  views_daily <- make_future_dataframe(views_prf , periods = period_rng,freq="day")
  views_daily_forecast <- predict(views_prf, views_daily)
  # tail(views_daily_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
  # plot(views_prf, views_daily_forecast)
  
  ### Add traffic and views to cpl_leads
  
  leads_cpl<-cap_data_daily%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=cpl_leads, ds=date)
  
  
  cpl_prf <- prophet(leads_cpl, daily.seasonality='auto', weekly.seasonality='auto' ,mcmc.samples = 1000,interval.width = 0.80,fit=FALSE,iter=3000)
  cpl_daily<-add_regressor(cpl_prf,name='imptrf')
  cpl_daily<-add_regressor(cpl_prf,name='cpl_views')
  cpl_prf2<- fit.prophet(cpl_daily, leads_cpl)
  
  leads_df <- make_future_dataframe(cpl_prf2, periods = period_rng,freq="day")
  
  # df_all <- leads_df%>%left_join(dplyr::select(trf_daily_forecast,yhat,ds), by = 'ds')%>%rename(imptrf=yhat)
  # df_all <- df_all  %>% left_join(dplyr::select(views_daily_forecast, yhat,ds), by = 'ds')%>%rename(cpl_views=yhat)
  
  df_all <- leads_df%>%left_join(dplyr::select(trf_daily_forecast,yhat_lower,ds), by = 'ds')%>%rename(imptrf=yhat_lower)
  df_all <- df_all  %>% left_join(dplyr::select(views_daily_forecast, yhat_lower,ds), by = 'ds')%>%rename(cpl_views=yhat_lower)
  
  forecast_cpl <- predict(cpl_prf2, df_all)
  # tail(forecast_cpl[c('ds', 'yhat_lower','yhat')])
  # plot(cpl_prf2, forecast_cpl)
  #
  #
  #
  #
  # prophet_plot_components(cpl_prf2, forecast_cpl)
  
  may<-forecast_cpl%>%tail(period_rng)
  # return(list(lower_estimate=sum(may$yhat_lower),estimate=sum(may$yhat)))
  # price<-pricelist%>%filter(cap_id=='1014')%>%select(accepted_revenue,billable_revenue,revised_revenue)
  #
  # forcastrev<-sum(may$yhat)*price$billable_revenue
  #
  
  return(sum(may$yhat))
  
}



#### GET FORECAST OF LEADS #####

##  FINAL using cluster
### Loop thru all cap-ids

caplist<-trf_daily_clean%>%filter(!is.na(cap_id) & date >= '2020-01-01' )%>%dplyr::select(cap_id)%>%distinct()%>%as.data.frame()
cc<-head(caplist,n=5)%>%as.data.frame()
datechk<-'2020-04-30'
period_rng<-31

library(doParallel)
library(prophet)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('cc','cap_data_daily','datechk','period_rng'), envir = environment())
datechk<-'2020-04-30'
period_rng<-31
system.time(forcast_leads <- parLapply(cl,cc$cap_id,pred_forecast,datechk,period_rng))
stopCluster(cl)


### Multiply by CPL 
forcast_leads1<-forcast_leads%>%unlist()%>%as.data.frame()%>%rename(forcast= ".")
forcast_leads1<-cbind(forcast_leads1,cc)
pricelist<-trf_daily_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date=='2020-04-01' & accepted_revenue>0)%>%distinct()
billable_leadrevenue<-left_join(forcast_leads1,pricelist,by="cap_id")%>%mutate(forcast_billable=forcast*accepted_revenue)%>%filter(!is.na(accepted_revenue))



