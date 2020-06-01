## DATE FORMAT : 2020-04-30
## THINGS NEEDED ARE DAILY DATA AND PRICELIST

pred_forecast('1014','2020-04-30')

capid<-'1014'
datechk<-'2020-04-30'
period_rng<-31

may<-pred_forecast(capid ,datechk,period_rng )

pred_forecast<-function(cap_id,datechk,period_rng)
  {
  set.seed(387482)
 
  # Make traffic forecasts
  cap_traffic<-cap_data_daily%>%filter(cap_id==capid)%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=imptrf, ds=date)
  
  # Make traffic forecasts
  trf_prf <- prophet(cap_traffic, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000)
  trf_daily <- make_future_dataframe(trf_prf , periods = period_rng,freq="day")
  trf_daily_forecast <- predict(trf_prf, trf_daily)
  tail(trf_daily_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
  plot(trf_prf, trf_daily_forecast) 
  
  # Make appview forecasts
  cap_views<-cap_data_daily%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=cpl_views, ds=date)
  
  views_prf <- prophet(cap_views, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000)
  views_daily <- make_future_dataframe(views_prf , periods = period_rng,freq="day")
  views_daily_forecast <- predict(views_prf, views_daily)
  tail(views_daily_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
  plot(views_prf, views_daily_forecast)
  
  ### Add traffic and views to cpl_leads
  
  leads_cpl<-cap_data_daily%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=cpl_leads, ds=date)
  
  
  cpl_prf <- prophet(leads_cpl, daily.seasonality='auto', weekly.seasonality='auto' ,mcmc.samples = 1000,fit=FALSE)
  cpl_daily<-add_regressor(cpl_prf,name='imptrf')
  cpl_daily<-add_regressor(cpl_prf,name='cpl_views')
  cpl_prf2<- fit.prophet(cpl_daily, leads_cpl)
  
  leads_df <- make_future_dataframe(cpl_prf2, periods = period_rng,freq="day")
  
  df_all <- leads_df  %>% left_join(select(trf_daily_forecast, yhat,ds), by = 'ds')%>%rename(imptrf=yhat)
  df_all <- df_all  %>% left_join(select(views_daily_forecast, yhat,ds), by = 'ds')%>%rename(cpl_views=yhat)
  
  forecast_cpl <- predict(cpl_prf2, df_all)
  tail(forecast_cpl[c('ds', 'yhat')])
  plot(cpl_prf2, forecast_cpl)
  
  prophet_plot_components(cpl_prf2, forecast_cpl)
  
  may<-forecast_cpl%>%tail(period_rng)
 # return(sum(may$yhat))
  return(may)
}



