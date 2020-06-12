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
forcast_leads1<-forcast_leads%>%unlist()%>%as.data.frame()%>%rename(forcast= ".")
forcast_leads1<-cbind(forcast_leads1,cc)
pricelist<-trf_daily_clean%>%select(cap_id,accepted_revenue,billable_revenue,revised_revenue,date)%>%filter(date=='2020-04-01' & accepted_revenue>0)%>%distinct()
billable_leadrevenue<-left_join(forcast_leads1,pricelist,by="cap_id")%>%mutate(forcast_billable=forcast*accepted_revenue)%>%filter(!is.na(accepted_revenue))



## GET VIEWS AND IMP FOR PROPOSED LEADS

get_views<-function(capid)
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  # knots <- quantile(model_data$imptrf, p = c(0.25, 0.5))
  # model_implead<-lm(imptrf~bs(cpl_leads,knots=knots),data=model_data)
  model_getviews<-lm(cpl_views~poly(cpl_leads,2,raw=TRUE),data=model_data)
  return(model_getviews)
}

get_imp<-function(model_getimp)
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  # knots <- quantile(model_data$imptrf, p = c(0.25, 0.5))
  # model_implead<-lm(imptrf~bs(cpl_leads,knots=knots),data=model_data)
  model_getimp<-lm(imptrf~poly(cpl_views,2,raw=TRUE),data=model_data)
  return(model_getimp)
}


library(doParallel)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('overall_cap'), envir = environment())
system.time(model_getviews <- parLapply(cl,cc$cap_id,get_views))
stopCluster(cl)

no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('overall_cap'), envir = environment())
system.time(model_getimp <- parLapply(cl,cc$cap_id,get_imp))
stopCluster(cl)

newdata = data.frame(x = c(7,8,9,10))
### Use leads to predict


new=data.frame(cpl_leads=billable_leadrevenue$forcast)

delta_imp<-predict(model_getviews[[1]], new=data.frame(cpl_leads=billable_leadrevenue$forcast))


map2(.x=model_getviews, .y=new, .f= ~predict(.x, newdata = .y))

xgb.pred <- map2(.x = xgb.model, .y = dtest, .f = ~ predict(.x, newdata = .y, type = 'prob'))

model <- lm(Total ~ Coupon, data=df)
new <- data.frame(Coupon = df$Coupon)
predict(model, newdata = new, interval="confidence")

###########################################################
### MODEL IMPRESSION~LEAD RELATIONSHIP ###

overall_cap<-trf_daily_clean%>%filter(date<='2020-04-30')%>%mutate(yrmnth=zoo::as.yearmon(date))%>%
  group_by(yrmnth,cap_id)%>%
  summarise(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
            cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%
  ungroup()%>%filter(!is.na(cap_id))%>%mutate(ctr=(cpl_views/imptrf),cr=(cpl_leads/cpl_views))

# Combine with cpl

overall_cap<-left_join(overall_cap,pricelist,by="cap_id")%>%filter(!is.na(accepted_revenue))

capid<-'856'

backtrack_impression<-function(capid)
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  # knots <- quantile(model_data$imptrf, p = c(0.25, 0.5))
  # model_implead<-lm(imptrf~bs(cpl_leads,knots=knots),data=model_data)
  model_implead<-lm(imptrf~poly(cpl_leads,2,raw=TRUE),data=model_data)
  return(model_implead)
}


redistribute_impression<-function(capid)
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  # knots <- quantile(model_data$imptrf, p = c(0.25, 0.5))
  # model_implead<-lm(imptrf~bs(cpl_leads,knots=knots),data=model_data)
  model_lead<-lm(cpl_leads~poly(imptrf,2,raw=TRUE),data=model_data)
  return(model_lead)
}


library(doParallel)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('overall_cap'), envir = environment())
system.time(result_implead <- parLapply(cl,cc$cap_id,backtrack_impression))
stopCluster(cl)

library(doParallel)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('overall_cap'), envir = environment())
system.time(result_leadimp <- parLapply(cl,cc$cap_id,redistribute_impression))
stopCluster(cl)


### PUT IN DIFFERENTIAL LEADS TO BE USED TO PREDICT DIFFERNTIAL IMPRESSION 
## EG 20% less rev for 856 : new leads 2153


cpl_leads.new<- c(2153)

delta_imp<-predict(result_implead[[1]], newdata=data.frame(cpl_leads=cpl_leads.new))


### REDISTRIBUTE DELTA IMP AMONGST OTHER CAPS ###

delta_lead<-predict(result_leadimp[[2]], newdata=data.frame(imptrf=delta_imp))


## GET VIEWS AND IMP FOR PROPOSED LEADS

get_views<-function(capid)
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  # knots <- quantile(model_data$imptrf, p = c(0.25, 0.5))
  # model_implead<-lm(imptrf~bs(cpl_leads,knots=knots),data=model_data)
  model_getviews<-lm(cpl_views~poly(cpl_leads,2,raw=TRUE),data=model_data)
  return(model_getviews)
}

get_imp<-function(model_getimp)
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  # knots <- quantile(model_data$imptrf, p = c(0.25, 0.5))
  # model_implead<-lm(imptrf~bs(cpl_leads,knots=knots),data=model_data)
  model_getimp<-lm(cpl_imp~poly(cpl_views,2,raw=TRUE),data=model_data)
  return(model_lead)
}




# # apply list to listed models
# 
# t<-map(.x=result_implead , .y=new, .f= ~predict(.x, newdata = .y))
# 
# prediction <- map2(validation, res, ~ predict(.y, .x, type = "class"))%>%unnest()
# 
# pred_df <- overall_cap %>% 
#   gather(var, val, -y) %>% 
#   nest(-var) %>% 
#   mutate(model = map(data, ~glm(y~val, data = .)), 
#          predicted = map(model, predict)) %>% 
#   unnest(data, predicted)
# 
# df.1 <- overall_cap %>%
#   group_by(cap_id) %>%
#   do(mod = lm(imptrf~poly(cpl_leads,2,raw=TRUE), data = .))
# 
# mm<-right_join(df.1,billable_leadrevenue,by="cap_id")
# preds<-map(mm$mod, predict, new=data.frame(cpl_leads=mm$forcast))
# 
# 
# t<-map(.x=df.1$mod , .y=new, .f= ~predict(.x, newdata = .y))
# 
# 
# t<-df.1%>%mutate(Pred = map(model, new, predict))
# 
# # df.1 <- overall_cap %>%
# #   nest(-cap_id) %>%
# #   mutate(mods = map(data, ~lm(imptrf ~ poly(cpl_leads, 2,raw=TRUE), data = .x)), 
# #          preds = map(mods, predict, newdata = data.frame(cpl_leads=billable_leadrevenue$forcast)))
# 
# 
# 
# lapply(new,df.1$mods,predict)


