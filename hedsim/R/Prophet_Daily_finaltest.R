require(prophet)
set.seed(124)
trf_daily_final<-readRDS('trf_daily_final.RDS')

#####  Daily Prophet : Do at the cap - id level ####



trf_daily_clean<-trf_daily_final%>%mutate_at(c(12,14,16), ~replace(., is.na(.), 0))
cap_data_daily<-trf_daily_clean%>%select(cap_id, school_name,date,accepted_cpl_leads,accepted_cpl_appviews,accepted_cpc_clicks,accepted_cpc_leads,
                                         accepted_revenue,billable_revenue,revised_revenue,
                                         `daily_new$combined_daily.dcs_traffic`)%>%
  filter(date <= '2020-04-30' )%>%group_by(cap_id, school_name,date)%>%
  summarize(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
            cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%filter(!is.na(cap_id))

pricelist<-trf_daily_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date=='2020-04-01' & accepted_revenue>0)%>%distinct()


## Only SNHU Capid 1014

SNHU_daily<-cap_data_daily%>%filter(school_name=='southern-new-hampshire-university')%>%
  filter(cap_id=='1014')%>%
  rename(y=cpl_leads, ds=date)

############  CASE 4 : Adding Multiple regressors such as traffic, appviews without saturated forecast

## ---- Project traffic

SNHU_traffic<-cap_data_daily%>%filter(school_name=='southern-new-hampshire-university')%>%
  filter(cap_id=='1014')%>%filter(date <= '2020-04-30' )%>%
  rename(y=imptrf, ds=date)

trf_prf <- prophet(SNHU_traffic, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000)
trf_daily <- make_future_dataframe(trf_prf , periods = 31,freq="day")
trf_daily_forecast <- predict(trf_prf, trf_daily)
tail(trf_daily_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(trf_prf, trf_daily_forecast)

####### Check May traffic
may_traffic<-trf_daily_forecast%>%filter(ds > '2020-04-30' & ds <= '2020-05-31')

##----- Project views

SNHU_views<-cap_data_daily%>%filter(school_name=='southern-new-hampshire-university')%>%
  filter(cap_id=='1014')%>%filter(date <= '2020-04-30' )%>%
  rename(y=cpl_views, ds=date)

views_prf <- prophet(SNHU_views, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000)
views_daily <- make_future_dataframe(views_prf , periods = 31,freq="day")
views_daily_forecast <- predict(views_prf, views_daily)
tail(views_daily_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(views_prf, views_daily_forecast)

####### Check May views
may_views<-views_daily_forecast%>%filter(ds > '2020-04-30' & ds <= '2020-05-31')

### Add traffic and views to cpl_leads

SNHU_cpl<-cap_data_daily%>%filter(school_name=='southern-new-hampshire-university')%>%
  filter(cap_id=='1014')%>%filter(date <= '2020-04-30' )%>%
  rename(y=cpl_leads, ds=date)


cpl_prf <- prophet(SNHU_cpl, daily.seasonality='auto', weekly.seasonality='auto' ,mcmc.samples = 1000,fit=FALSE,uncertainty.samples=0)
cpl_daily<-add_regressor(cpl_prf,name='imptrf')
cpl_daily<-add_regressor(cpl_prf,name='cpl_views')
cpl_prf2<- fit.prophet(cpl_daily, SNHU_cpl)

leads_df <- make_future_dataframe(cpl_prf2, periods = 31,freq="day")


df_all <- leads_df  %>% left_join(select(trf_daily_forecast, yhat,ds), by = 'ds')%>%rename(imptrf=yhat)
df_all <- df_all  %>% left_join(select(views_daily_forecast, yhat,ds), by = 'ds')%>%rename(cpl_views=yhat)

forecast_cpl <- predict(cpl_prf2, df_all)
tail(forecast_cpl[c('ds', 'yhat')])
plot(cpl_prf2, forecast_cpl)

prophet_plot_components(cpl_prf2, forecast_cpl)

may<-forecast_cpl%>%filter(ds>= '2020-05-01' & ds <= '2020-05-31')
sum(may$yhat)
# [1]  11866.82

#Revenue projection
price<-pricelist%>%filter(cap_id=='1014')%>%select(accepted_revenue,billable_revenue,revised_revenue)

sum(may$yhat)*price$billable_revenue



