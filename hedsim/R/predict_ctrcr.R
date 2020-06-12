trf_daily_clean<-trf_daily_final%>%mutate_at(c(12,14,16), ~replace(., is.na(.), 0))%>%filter(!is.na(cap_id))


overall_cap<-trf_daily_clean%>%mutate(yrmnth=zoo::as.yearmon(date))%>%
  group_by(yrmnth,cap_id)%>%
  summarise(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
            cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%
  mutate(yr=year(yrmnth),yrmnth1=yearmon(yrmnth))%>%
  ungroup()%>%filter(!is.na(cap_id))%>%mutate(ctr=(cpl_views/imptrf),cr=(cpl_leads/cpl_views))


pricelist<-trf_daily_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date=='2020-04-01' & accepted_revenue>0)%>%distinct()

overall_cap<-left_join(overall_cap,pricelist,by="cap_id")%>%filter(!is.na(revised_revenue))%>%mutate(erpi=(ctr*cr*accepted_revenue))

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


#########




pred_ctr<-function(capid )
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  knots <- quantile(model_data$imptrf, p = c(0.25, 0.5, 0.75))
  model_ctr<-lm(ctr~bs(imptrf,knots=knots),data=model_data)
  return(model_ctr)
  
}

pred_ctr<-function(capid )
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  knots <- quantile(model_data$imptrf, p = c(0.25, 0.5, 0.75))
  model_cr<-lm(cr~bs(imptrf,knots=knots),data=model_data)
  return(model_cr)
  
}


ctr<-pred_ctr('1014')
imptrf.new <- c(2500000)
predict(ctr, newdata=data.frame(imptrf=imptrf.new))
