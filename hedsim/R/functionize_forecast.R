## DATE FORMAT : 2020-04-30
## THINGS NEEDED ARE DAILY DATA AND PRICELIST
require(dplyr)
require(prophet)
require(tidyverse)

trf_daily_final<-readRDS('trf_daily_final.RDS')
trf_daily_clean<-trf_daily_final%>%mutate_at(c(12,14,16), ~replace(., is.na(.), 0))
cap_data_daily<-trf_daily_clean%>%dplyr::select(cap_id, school_name,date,accepted_cpl_leads,accepted_cpl_appviews,accepted_cpc_clicks,accepted_cpc_leads,
                                         accepted_revenue,billable_revenue,revised_revenue,
                                         `daily_new$combined_daily.dcs_traffic`)%>%
  filter(date <= '2020-04-30' )%>%group_by(cap_id, school_name,date)%>%
  summarize(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
            cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%filter(!is.na(cap_id))

pricelist<-trf_daily_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date=='2020-04-01' & accepted_revenue>0)%>%distinct()


capid<-'2273'
datechk<-'2020-04-30'
period_rng<-31

may<-pred_forecast(capid ,datechk,period_rng )

pred_forecast<-function(capid,datechk,period_rng)
  {

set.seed(1234)
  # Make traffic forecasts
  cap_traffic<-cap_data_daily%>%filter(cap_id==capid)%>%
    filter(cap_id==cap_id)%>%filter(date <= datechk )%>%
    rename(y=imptrf, ds=date)

  # Make traffic forecasts
  trf_prf <- prophet(cap_traffic, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000,interval.width = 0.99)
  trf_daily <- make_future_dataframe(trf_prf , periods = period_rng,freq="day")
  trf_daily_forecast <- predict(trf_prf, trf_daily)
  # tail(trf_daily_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
  # plot(trf_prf, trf_daily_forecast)

  # Make appview forecasts
  cap_views<-cap_data_daily%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=cpl_views, ds=date)

  views_prf <- prophet(cap_views, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000,interval.width = 0.99)
  views_daily <- make_future_dataframe(views_prf , periods = period_rng,freq="day")
  views_daily_forecast <- predict(views_prf, views_daily)
  # tail(views_daily_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
  # plot(views_prf, views_daily_forecast)

  ### Add traffic and views to cpl_leads

  leads_cpl<-cap_data_daily%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=cpl_leads, ds=date)


  cpl_prf <- prophet(leads_cpl, daily.seasonality='auto', weekly.seasonality='auto' ,mcmc.samples = 1000,interval.width = 0.99,fit=FALSE,iter=3000)
  cpl_daily<-add_regressor(cpl_prf,name='imptrf')
  cpl_daily<-add_regressor(cpl_prf,name='cpl_views')
  cpl_prf2<- fit.prophet(cpl_daily, leads_cpl)

  leads_df <- make_future_dataframe(cpl_prf2, periods = period_rng,freq="day")

  df_all <- leads_df%>%left_join(dplyr::select(trf_daily_forecast,yhat,ds), by = 'ds')%>%rename(imptrf=yhat)
  df_all <- df_all  %>% left_join(dplyr::select(views_daily_forecast, yhat,ds), by = 'ds')%>%rename(cpl_views=yhat)



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


### Loop thru all cap-ids

caplist<-trf_daily_clean%>%filter(!is.na(cap_id) & date >= '2020-01-01' )%>%dplyr::select(cap_id)%>%distinct()%>%as.data.frame()

res<-list()
i=1


for (i in 1: nrow(caplist))
{
  capid<-caplist$cap_id[i]
  res[[i]]<-pred_forecast(capid,'2020-04-30',period_rng=31)
  i<-i+1
}

## Comparison of regular vs parallelized
cc<-caplist%>%as.data.frame()
datechk<-'2020-04-30'
period_rng<-31


# Using mclapply

library(parallel)
numCores <- detectCores()
numCores

# Reg loop
system.time(
  results <- lapply(cc$cap_id,pred_forecast,datechk,period_rng)
)


system.time(
  results1 <- mclapply(cc$cap_id,pred_forecast,datechk,period_rng,mc.cores=numCores-2)
)


## using cluster
cc<-head(caplist,n=10)%>%as.data.frame()
datechk<-'2020-04-30'
period_rng<-31

library(doParallel)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('cc','cap_data_daily','datechk','period_rng'), envir = environment())
datechk<-'2020-04-30'
period_rng<-31
system.time(result <- parLapply(cl,cc$cap_id,pred_forecast,datechk,period_rng))
stopCluster(cl)



## COMPARISON

# system.time(result <- parLapply(cl,cc$cap_id,pred_forecast,datechk,period_rng))
# user  system elapsed
# 0.262   2.435 253.920
# There were 33 warnings (use warnings() to see them)
# > stopCluster(cl)
# > View(result)
# > system.time(
#   +   results1 <- mclapply(cc$cap_id,pred_forecast,datechk,period_rng,mc.cores=numCores-2)
#   + )
# user   system  elapsed
# 1472.672   13.521  223.822

# capid<-'2327'
# datechk<-'2020-04-30'
# period_rng<-31
# pred_forecast(capid,datechk,period_rng) #250
#
# capid<-'1146'
# datechk<-'2020-04-30'
# period_rng<-31
# pred_forecast(capid,datechk,period_rng) ##2288
