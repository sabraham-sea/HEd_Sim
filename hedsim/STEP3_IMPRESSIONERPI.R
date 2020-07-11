

query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/DE_MCData.sql")
DE_MCData<- DBI::dbGetQuery(connection, query_statement)

overall_cap<-overall_cap%>%mutate(erpi=ctr*cr*accepted_revenue)%>%filter(cap_id %in% finalcaplist)

notlist<-which(!cc$cap_id %in% overall_cap$cap_id)

finalcaplist<-caplist[-notlist]


impcplerpi<-function(capid)
{
  model_data<-overall_cap%>%filter(cap_id==capid )
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
system.time(result_impcplerpi <- parLapply(cl,finalcaplist,impcplerpi))
stopCluster(cl)


# Loop thru all cases
pred_erpi<-list()
i=1
n2<-NROW(finalcaplist)
for (i in 1:n2)
{
  pred_erpi[[i]]<-predict(result_impcplerpi[[i]], new=data.frame(imptrf= predicted_impression$pred_imp[i]))
  i=i+1
}

predicted_erpi<-pred_erpi%>%unlist()%>%cbind(finalcaplist)%>%as.data.frame()%>%rename( 'pred_erpi'='.','cap_id'='finalcaplist')
predicted_erpi$pred_erpi<-as.numeric(predicted_erpi$pred_erpi)


rsquared_erpi<-list()
n2<-NROW(finalcaplist)
i=1
for (i in 1:n2)
{
  rsquared_erpi[[i]]<-rsq::rsq(result_impcplerpi[[i]])
  i=i+1
}
