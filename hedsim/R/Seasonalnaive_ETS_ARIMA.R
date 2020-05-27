# Make Tsibble
overall_<-trf_daily_final %>% mutate(yrmnth=zoo::as.yearmon(date))%>%
  group_by(yrmnth,cap_id,degree_name,school_name,category_name.x,subject_name)%>%
  summarise(accepted_cpl_leads=sum(accepted_cpl_leads),accepted_cpc_leads=sum(accepted_cpc_leads))%>%
  mutate(yr=year(yrmnth),yrmnth1=yearmonth(yrmnth))%>%
  ungroup()%>%
  as_tsibble(key = c(cap_id,school_name,degree_name,category_name.x,subject_name), index = yrmnth1)


#Daily TS

t<-overall_%>%filter(cap_id=='1014')%>%autoplot(accepted_cpl_leads)

# Makr tsiibble by cap-category

overall_cap<-trf_daily_final %>% mutate(yrmnth=zoo::as.yearmon(date))%>%
  group_by(yrmnth,cap_id,category_name.x)%>%
  summarise(accepted_cpl_leads=sum(accepted_cpl_leads),accepted_cpc_leads=sum(accepted_cpc_leads))%>%
  mutate(yr=year(yrmnth),yrmnth1=yearmonth(yrmnth))%>%
  ungroup()%>%
  as_tsibble(key = c(cap_id,category_name.x), index = yrmnth1)

overall_cap%>%filter(cap_id=='1014')%>%autoplot(accepted_cpl_leads)

# Predict usinng ETS, ARIMA etc at cap level - daily data FOR 1014

level_1014<-trf_daily_final %>% filter(cap_id=='1014')%>%mutate(date=zoo::as.Date(date))%>%
  group_by(date)%>%
  summarise(accepted_cpl_leads=sum(accepted_cpl_leads),accepted_cpc_leads=sum(accepted_cpc_leads),
            traffic=sum(`daily_new$combined_daily.dcs_traffic`),accepted_cpl_appviews=sum(accepted_cpl_appviews))%>%
  ungroup()%>%
  as_tsibble( index = date)

fit<-level_1014%>%model(
        snaive=SNAIVE(accepted_cpl_leads),
        ets=ETS(accepted_cpl_leads),
        arima=ARIMA(accepted_cpl_leads))

fit

# Generate forecast for next 30 days

forecast1014<-fit%>%forecast(h=31)

forecast1014%>%autoplot(level_1014,level=NULL)
 
t<-forecast1014%>%filter(.model=='arima' & date <= '2020-06-01')%>%as.tsibble()
sum(t$accepted_cpl_leads)

# [1]  4447.475
#########################################

fit_dyn <- level_1014 %>%
  model(ARIMA(accepted_cpl_leads ~ traffic+accepted_cpl_appviews))
report(fit_dyn)


