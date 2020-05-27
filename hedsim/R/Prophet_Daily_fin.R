require(prophet)
#####  Daily Prophet : Do at the cap - id level ####



trf_daily_clean<-trf_daily_final%>%mutate_at(c(12,14,16), ~replace(., is.na(.), 0))
cap_data_daily<-trf_daily_clean%>%select(cap_id, school_name,date,accepted_cpl_leads,accepted_cpl_appviews,accepted_cpc_clicks,accepted_cpc_leads,
                                         accepted_revenue,billable_revenue,revised_revenue,
                                         `daily_new$combined_daily.dcs_traffic`)%>%
  filter(date <= '2020-04-30')%>%group_by(cap_id, school_name,date)%>%
  summarize(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
            cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`),
            accepted_revenue=sum(accepted_revenue),billable_revenue=sum(billable_revenue),
            revised_revenue=sum(revised_revenue))


## Only SNHU Capid 1014

SNHU_daily<-cap_data_daily%>%filter(school_name=='southern-new-hampshire-university')%>%
  filter(cap_id=='1014')%>%
  rename(y=cpl_leads, ds=date)




###### Single regressor : CASE 1 : DAILY
m_daily <- prophet(SNHU_daily, daily.seasonality='auto', weekly.seasonality='auto' ,mcmc.samples = 1000)
future_daily <- make_future_dataframe(m_daily, periods = 30,freq="day")
forecast_daily <- predict(m_daily, future_daily)
tail(forecast_daily[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m_daily, forecast_daily)

prophet_plot_components(m_daily, forecast_daily)

# Check May

may<-forecast_daily%>%filter(ds>= '2020-05-01' & ds <= '2020-05-31')
sum(may$yhat)

# [1] 11907.84

############## CASE 2:Adding allowable cap in CASE 2:  NOT USING ( SINCE CAP IS PROVIDED  BY JAKE )

SNHU_daily$cap<-10495
m_daily <- prophet(SNHU_daily, growth = 'logistic')
future_daily <- make_future_dataframe(m_daily, periods = 30,freq="day")
future_daily$cap<-10495
forecast_daily <- predict(m_daily, future_daily)
tail(forecast_daily[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m_daily, forecast_daily)

prophet_plot_components(m_daily, forecast_daily)

# Check May

may<-forecast_daily%>%filter(ds>= '2020-05-01' & ds <= '2020-05-31')
sum(may$yhat)

# 9980

############ CASE 3: Adding Multiple regressors such as traffic, appviews with saturated forecast : NOT USING

## ---- Project traffic

SNHU_traffic<-cap_data_daily%>%filter(school_name=='southern-new-hampshire-university')%>%
  filter(cap_id=='1014')%>%filter(date <= '2020-04-30' )%>%
  rename(y=imptrf, ds=date)

trf_prf <- prophet(SNHU_traffic, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000)
trf_daily <- make_future_dataframe(trf_prf , periods = 30,freq="day")
trf_daily_forecast <- predict(trf_prf, trf_daily)
tail(trf_daily_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(trf_prf, trf_daily_forecast)
####### Check May traffic
may_traffic<-trf_daily_forecast%>%filter(ds > '2020-04-30' & ds <= '2020-05-31')

##-- Project views
SNHU_views<-cap_data_daily%>%filter(school_name=='southern-new-hampshire-university')%>%
  filter(cap_id=='1014')%>%filter(date <= '2020-04-30' )%>%
  rename(y=cpl_views, ds=date)

views_prf <- prophet(SNHU_views, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000)
views_daily <- make_future_dataframe(views_prf , periods = 30,freq="day")
views_daily_forecast <- predict(views_prf, views_daily)
tail(views_daily_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(trf_prf, trf_daily_forecast)
####### Check May traffic
may_views<-views_daily_forecast%>%filter(ds > '2020-04-30' & ds <= '2020-05-31')

### Add traffic to cpl_leads

SNHU_cpl<-cap_data_daily%>%filter(school_name=='southern-new-hampshire-university')%>%
  filter(cap_id=='1014')%>%filter(date <= '2020-04-30' )%>%
rename(y=cpl_leads, ds=date)
SNHU_cpl$cap<-10495

cpl_prf <- prophet(SNHU_cpl, growth = 'logistic',fit=FALSE)
cpl_daily<-add_regressor(cpl_prf,name='imptrf')
cpl_daily<-add_regressor(cpl_prf,name='cpl_views')
cpl_prf2<- fit.prophet(cpl_daily, SNHU_cpl)

leads_df <- make_future_dataframe(cpl_prf2, periods = 30,freq="day")
leads_df$cap<-10495

df_all <- leads_df  %>% left_join(select(trf_daily_forecast, yhat,ds), by = 'ds')%>%rename(imptrf=yhat)
df_all <- df_all  %>% left_join(select(views_daily_forecast, yhat,ds), by = 'ds')%>%rename(cpl_views=yhat)

forecast_cpl <- predict(cpl_prf2, df_all)
tail(forecast_cpl[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(cpl_prf2, forecast_cpl)

prophet_plot_components(m_daily, forecast_daily)

may<-forecast_cpl%>%filter(ds>= '2020-05-01' & ds <= '2020-05-31')
sum(may$yhat)
# [1]  12107.05




############  CASE 4 : Adding Multiple regressors such as traffic, appviews without saturated forecast

## ---- Project traffic

SNHU_traffic<-cap_data_daily%>%filter(school_name=='southern-new-hampshire-university')%>%
  filter(cap_id=='1014')%>%filter(date <= '2020-04-30' )%>%
  rename(y=imptrf, ds=date)

trf_prf <- prophet(SNHU_traffic, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000)
trf_daily <- make_future_dataframe(trf_prf , periods = 30,freq="day")
trf_daily_forecast <- predict(trf_prf, trf_daily)
tail(trf_daily_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(trf_prf, trf_daily_forecast)
####### Check May traffic
may_traffic<-trf_daily_forecast%>%filter(ds > '2020-04-30' & ds <= '2020-05-31')

##-- Project views
SNHU_views<-cap_data_daily%>%filter(school_name=='southern-new-hampshire-university')%>%
  filter(cap_id=='1014')%>%filter(date <= '2020-04-30' )%>%
  rename(y=cpl_views, ds=date)

views_prf <- prophet(SNHU_views, daily.seasonality='auto', weekly.seasonality='auto', mcmc.samples = 1000)
views_daily <- make_future_dataframe(views_prf , periods = 30,freq="day")
views_daily_forecast <- predict(views_prf, views_daily)
tail(views_daily_forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(views_prf, views_daily_forecast)
####### Check May views
may_views<-views_daily_forecast%>%filter(ds > '2020-04-30' & ds <= '2020-05-31')

### Add traffic and views to cpl_leads

SNHU_cpl<-cap_data_daily%>%filter(school_name=='southern-new-hampshire-university')%>%
  filter(cap_id=='1014')%>%filter(date <= '2020-04-30' )%>%
  rename(y=cpl_leads, ds=date)


cpl_prf <- prophet(SNHU_cpl, daily.seasonality='auto', weekly.seasonality='auto' ,mcmc.samples = 1000,fit=FALSE)
cpl_daily<-add_regressor(cpl_prf,name='imptrf')
cpl_daily<-add_regressor(cpl_prf,name='cpl_views')
cpl_prf2<- fit.prophet(cpl_daily, SNHU_cpl)

leads_df <- make_future_dataframe(cpl_prf2, periods = 30,freq="day")


df_all <- leads_df  %>% left_join(select(trf_daily_forecast, yhat,ds), by = 'ds')%>%rename(imptrf=yhat)
df_all <- df_all  %>% left_join(select(views_daily_forecast, yhat,ds), by = 'ds')%>%rename(cpl_views=yhat)

forecast_cpl <- predict(cpl_prf2, df_all)
tail(forecast_cpl[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(cpl_prf2, forecast_cpl)

prophet_plot_components(cpl_prf2, forecast_cpl)

may<-forecast_cpl%>%filter(ds>= '2020-05-01' & ds <= '2020-05-31')
sum(may$yhat)
# [1]  11799.78


