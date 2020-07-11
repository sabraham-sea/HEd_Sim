## STEP 1 FORECAST LEADS WITH REVENUE

trf_daily_final<-readRDS('trf_daily_final.RDS')
trf_daily_clean<-trf_daily_final%>%mutate_at(c(12,14,16), ~replace(., is.na(.), 0))
cap_data_daily<-trf_daily_clean%>%filter(date<='2020-04-30')%>%dplyr::select(cap_id, school_name,date,accepted_cpl_leads,accepted_cpl_appviews,accepted_cpc_clicks,accepted_cpc_leads,
                                                                             accepted_revenue,billable_revenue,revised_revenue,
                                                                             `daily_new$combined_daily.dcs_traffic`)%>%
    group_by(cap_id, school_name,date)%>%
    summarize(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
              cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%filter(!is.na(cap_id))

pricelist<-trf_daily_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date=='2020-04-01' & accepted_revenue>0)%>%distinct()

caplist<-pricelist$cap_id

datechk<-'2020-04-30'
period_rng<-31
capid<-'1010'

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
    
    # df_all <- leads_df%>%left_join(dplyr::select(trf_daily_forecast,yhat_lower,ds), by = 'ds')%>%rename(imptrf=yhat_lower)
    # df_all <- df_all  %>% left_join(dplyr::select(views_daily_forecast, yhat_lower,ds), by = 'ds')%>%rename(cpl_views=yhat_lower)
    
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
datechk<-'2020-04-30'
period_rng<-31
system.time(forcast_leads <- parLapply(cl,cc$cap_id,pred_forecast,datechk,period_rng))
stopCluster(cl)


### Multiply by CPL 
forcast_leads1<-forcast_leads%>%unlist()%>%as.data.frame()%>%rename(forcast= ".")
forcast_leads1<-cbind(forcast_leads1,cc)
billable_leadrevenue_all<-left_join(forcast_leads1,pricelist,by="cap_id")%>%mutate(forcast_billable=forcast*accepted_revenue)%>%filter(!is.na(accepted_revenue))

## FIND FINAL CAPLIST##
overall_cap<-combined_daily%>%filter(date<='2020-04-30')%>%mutate(yrmnth=zoo::as.yearmon(date))%>%
    group_by(yrmnth,cap_id)%>%
    summarise(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
              cpc_leads=sum(accepted_cpc_leads),imptrf=sum(dcs_traffic))%>%
    ungroup()%>%filter(!is.na(cap_id))%>%mutate(ctr=(cpl_views/imptrf),cr=(cpl_leads/cpl_views))
################


# Check from erpi data
clean_erpi<- DE_MCData3%>%filter(school_capid != 'NA')%>%rename("cap_id"="school_capid") %>%filter(month=='4')
erpi_caplist<-clean_erpi%>%select(cap_id)%>%distinct()
erpi_caplist$cap_id<-as.character(erpi_caplist$cap_id)

final_list<-inner_join(cc,erpi_caplist,by="cap_id")

billable_leadrevenue_final<-billable_leadrevenue_all%>%filter(cap_id %in% final_list$cap_id)  # for testing

