trf_daily_final<-readRDS('trf_daily_final.RDS')
trf_daily_clean<-trf_daily_final%>%mutate_at(c(12,14,16), ~replace(., is.na(.), 0))
cap_data_daily<-trf_daily_clean%>%dplyr::select(cap_id, school_name,date,accepted_cpl_leads,accepted_cpl_appviews,accepted_cpc_clicks,accepted_cpc_leads,
                                                accepted_revenue,billable_revenue,revised_revenue,
                                                `daily_new$combined_daily.dcs_traffic`)%>%
  filter(date <= '2020-04-30' )%>%group_by(cap_id, school_name,date)%>%
  summarize(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
            cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%filter(!is.na(cap_id))

pricelist<-trf_daily_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date=='2020-04-01' & accepted_revenue>0)%>%distinct()


#### GET FORECAST OF LEADS #####

##  FINAL using cluster
### Loop thru all cap-ids

caplist<-trf_daily_clean%>%filter(!is.na(cap_id) & date >= '2020-01-01' )%>%dplyr::select(cap_id)%>%distinct()%>%as.data.frame()
cc<-head(caplist,n=5)%>%as.data.frame()
datechk<-'2020-04-30'
period_rng<-31

library(doParallel)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('cc','cap_data_daily','datechk','period_rng'), envir = environment())
datechk<-'2020-04-30'
period_rng<-31
system.time(forcast_leads <- parLapply(cl,cc$cap_id,pred_forecast,datechk,period_rng))
stopCluster(cl)


### Multiply by CPL 
result1<-result%>%unlist()%>%as.data.frame()%>%rename(forcast= ".")
result1<-cbind(result1,cc)
pricelist<-trf_daily_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date=='2020-04-01' & accepted_revenue>0)%>%distinct()
result2<-left_join(result1,pricelist,by="cap_id")%>%mutate(forcast_billable=forcast*accepted_revenue)%>%filter(!is.na(accepted_revenue))


###############################


forcast_impression<-function(capid,datechk,period_rng)
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
  
  may<-trf_daily_forecast%>%tail(period_rng)
  # return(list(lower_estimate=sum(may$yhat_lower),estimate=sum(may$yhat)))
  # price<-pricelist%>%filter(cap_id=='1014')%>%select(accepted_revenue,billable_revenue,revised_revenue)
  #
  # forcastrev<-sum(may$yhat)*price$billable_revenue
  #

  return(sum(may$yhat))
  
}

library(doParallel)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('cc','cap_data_daily','datechk','period_rng'), envir = environment())
datechk<-'2020-04-30'
period_rng<-31
system.time(forcast_impression <- parLapply(cl,cc$cap_id,forcast_impression,datechk,period_rng))
stopCluster(cl)

forcast_impression1 <-forcast_impression %>%unlist()%>%as.data.frame()%>%rename(forcast= ".")
forcast_impression1<-cbind(forcast_impression1,cc)


forecast_views<-function(capid,datechk,period_rng)
{
  set.seed(1234)
  # Make appview forecasts
  cap_views<-cap_data_daily%>%
    filter(cap_id==capid)%>%filter(date <= datechk )%>%
    rename(y=cpl_views, ds=date)
  
  views_prf <- prophet(cap_views, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000,interval.width = 0.80)
  views_daily <- make_future_dataframe(views_prf , periods = period_rng,freq="day")
  views_daily_forecast <- predict(views_prf, views_daily)
  may<-views_daily_forecast%>%tail(period_rng)
  # return(list(lower_estimate=sum(may$yhat_lower),estimate=sum(may$yhat)))
  # price<-pricelist%>%filter(cap_id=='1014')%>%select(accepted_revenue,billable_revenue,revised_revenue)
  #
  # forcastrev<-sum(may$yhat)*price$billable_revenue
  #
  
  return(sum(may$yhat))
  
}


library(doParallel)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('cc','cap_data_daily','datechk','period_rng'), envir = environment())
datechk<-'2020-04-30'
period_rng<-31
system.time(forcast_views <- parLapply(cl,cc$cap_id,forecast_views,datechk,period_rng))
stopCluster(cl)


forcast_views1 <-forcast_views %>%unlist()%>%as.data.frame()%>%rename(forcast= ".")
forcast_views1<-cbind(forcast_views1,cc)
# pricelist<-trf_daily_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date=='2020-04-01' & accepted_revenue>0)%>%distinct()
# result2<-left_join(forcast_impression1,pricelist,by="cap_id")%>%mutate(forcast_billable=forcast*accepted_revenue)%>%filter(!is.na(accepted_revenue))

  
###########################################################
### MODEL IMPRESSION CTR /CR RELATIONSHIP ###

overall_cap<-trf_daily_clean%>%filter(date<='2020-04-30')%>%mutate(yrmnth=zoo::as.yearmon(date))%>%
  group_by(yrmnth,cap_id)%>%
  summarise(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
            cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%
  mutate(yr=year(yrmnth),yrmnth1=yearmonth(yrmnth))%>%
  ungroup()%>%filter(!is.na(cap_id))%>%mutate(ctr=(cpl_views/imptrf),cr=(cpl_leads/cpl_views))

predict_ctr<-function(capid)
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  knots <- quantile(model_data$imptrf, p = c(0.25, 0.5, 0.75))
  model_ctr<-lm(ctr~bs(imptrf,knots=knots),data=model_data)
  return(model_ctr)
}

predict_cr<-function(capid)
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  knots <- quantile(model_data$imptrf, p = c(0.25, 0.5, 0.75))
  model_cr<-lm(cr~bs(imptrf,knots=knots),data=model_data)
  return(model_cr)
  
}
  

library(doParallel)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('overall_cap'), envir = environment())
system.time(result_ctr <- parLapply(cl,cc$cap_id,predict_ctr))
stopCluster(cl)


library(doParallel)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('overall_cap'), envir = environment())
system.time(result_cr <- parLapply(cl,cc$cap_id,predict_cr))
stopCluster(cl)

##### PREDICT CTR AND CR FOR FORCASTED IMPRESSION

imptrf.new <- c(1147628)
newdata=data.frame(imptrf=imptrf.new)
predict(result_ctr[[1]], newdata=data.frame(imptrf=imptrf.new))

imptrf.new <- c(1347753.88)
newdata=data.frame(imptrf=imptrf.new)
predict(result_cr[[1]], newdata=data.frame(imptrf=imptrf.new))



