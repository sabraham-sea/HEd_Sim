
overall_cap<-overall_cap%>%mutate(erpi=ctr*cr*accepted_revenue)

impcplerpi<-function(capid)
{
  model_data<-overall_cap%>%filter(cap_id==capid)
  # knots <- quantile(model_data$imptrf, p = c(0.25, 0.5))
  # model_implead<-lm(imptrf~bs(cpl_leads,knots=knots),data=model_data)
  model_impcplerpi<-lm(erpi~imptrf,data=model_data)
  #model_implead<-lm(imptrf~cpl_leads,data=model_data)
  return(model_impcplerpi)
}



library(doParallel)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('overall_cap'), envir = environment())
system.time(result_impcplerpi <- parLapply(cl,cc$cap_id,impcplerpi))
stopCluster(cl)


# Loop thru all cases
pred_epri<-list()
i=1
for (i in 1:5)
{
  pred_epri[[i]]<-predict(result_impcplerpi[[i]], new=data.frame(imptrf= predicted_impre$pred_imp[i]))
  i=i+1
}

predicted_epri<-pred_epri%>%unlist()%>%cbind(cc)%>%rename( 'pred_epri'='.')