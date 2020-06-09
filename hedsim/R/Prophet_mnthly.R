#####  Monthly Prophet : Do at the cap - id level ####

cap_data<-trf_new%>%filter(yrmnth< 'May 2020')%>%select(cap_id, school_name,yrmnth1,cpl_leads,cpl_views,cpc_clicks,cpc_leads,`imputed_CDMdata$combined_trf_app.dcs_trf`)%>%
  group_by(cap_id, school_name,yrmnth1)%>%
  summarise(cpl_leads=sum(cpl_leads),cpl_views=sum(cpl_views),cpc_clicks=sum(cpc_clicks),
            cpc_leads=sum(cpc_leads),imptrf=sum(`imputed_CDMdata$combined_trf_app.dcs_trf`))


## Only SNHU Capid 1014

SNHU_<-cap_data%>%filter(school_name=='southern-new-hampshire-university')%>%
  filter(cap_id=='1014')%>%
rename(y=cpl_leads, ds=yrmnth1)

# Single regressor
m <- prophet(SNHU_, seasonality.mode = 'multiplicative', mcmc.samples = 300, fit=FALSE)
future <- make_future_dataframe(m, periods = 12,freq="month")


forecast <- predict(m, future)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])
plot(m, forecast)

prophet_plot_components(m, forecast)

### Adding regressor

m1 <- prophet(SNHU_, seasonality.mode = 'multiplicative', mcmc.samples = 300, fit=FALSE)
m1<-add_regressor(m1,name='cpl_views')
m1<- fit.prophet(m1, SNHU_)

future1 <- make_future_dataframe(m1, periods = 12,freq="month")

future1$date_key<-as.Date(future1$ds)
SNHU_$date_key <- as.Date(SNHU_$ds)

future_all <- future1 %>% left_join(select(SNHU_, cpl_views, date_key), by = 'date_key') %>% 
  select(-date_key)%>%replace_na(list(cpl_views=0))

forecast_all <- predict(m1, future_all)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')])

plot(m1, forecast_all)
prophet_plot_components(m1, forecast_all)

