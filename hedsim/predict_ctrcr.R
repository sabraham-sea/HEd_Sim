trf_daily_clean<-trf_daily_final%>%mutate_at(c(12,14,16), ~replace(., is.na(.), 0))%>%filter(!is.na(cap_id))



overall_cap<-trf_daily_clean%>%mutate(yrmnth=zoo::as.yearmon(date))%>%
  group_by(yrmnth,cap_id)%>%
  summarise(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
            cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%
  mutate(yr=year(yrmnth),yrmnth1=yearmonth(yrmnth))%>%
  ungroup()%>%filter(!is.na(cap_id))%>%mutate(ctr=(cpl_views/imptrf),cr=(cpl_leads/cpl_views))

# Regression
model_data<-overall_cap%>%filter(cap_id=='1014')

mod<-glm(ctr~imptrf,data=model_data, family="gaussian")

# Polynomial Regression # 
model<-lm(ctr~poly(imptrf,2,raw=TRUE),data=model_data)
model_data$predictions<-predict(model,model_data)

#####  Cubic Spline #######
knots <- quantile(model_data$imptrf, p = c(0.25, 0.5, 0.75))
model_ctr<-lm(ctr~bs(imptrf,knots=knots),data=model_data)
summary(model_ctr)

model_data$pred_ctr<-predict(model1,model_data)

imptrf.new <- c(100000,200000,2500000)
newdata=data.frame(imptrf=imptrf.new)
predict(model1, newdata=data.frame(imptrf=imptrf.new))


knots <- quantile(model_data$imptrf, p = c(0.25, 0.5, 0.75))
model_cr<-lm(cr~bs(imptrf,knots=knots),data=model_data)
summary(model_cr)

imptrf.new <- c(100000,200000,2500000)
newdata=data.frame(imptrf=imptrf.new)
predict(model_cr, newdata=data.frame(imptrf=imptrf.new))



mm<-overall_cap%>%filter(cap_id=='1014')