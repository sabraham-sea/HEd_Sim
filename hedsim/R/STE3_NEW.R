

# average erpi
query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/DE_MCData3.sql")
DE_MCData3<- DBI::dbGetQuery(connection, query_statement)


clean_erpi<- DE_MCData3%>%filter(school_capid != 'NA')%>%rename("cap_id"="school_capid") %>% filter(cap_id %in% finalcaplist)
clean_erpi$cap_id<-as.character(clean_erpi$cap_id)


# Combine erpi with overall cap

overall_cap1<-overall_cap%>%mutate(month=month(yrmnth),year=year(yrmnth))%>%filter(year!= '2019')

overall_erpi<-inner_join(overall_cap1,clean_erpi,by=c("month","cap_id"))

erpi_caplist<-overall_erpi%>%select(cap_id)%>%distinct()

# notlist<-which(!cc$cap_id %in% overall_cap$cap_id)
# 
# finalcaplist<-caplist[-notlist]


impcplerpi<-function(capid)
{
    model_data<-overall_erpi%>%filter(cap_id==capid )
    # knots <- quantile(model_data$imptrf, p = c(0.25, 0.5))
    # model_implead<-lm(imptrf~bs(cpl_leads,knots=knots),data=model_data)
    model_impcplerpi<-lm(avg_erpi~imptrf,data=model_data)
    #model_implead<-lm(imptrf~cpl_leads,data=model_data)
    return(model_impcplerpi)
}



library(doParallel)
no_cores <- detectCores() - 1
registerDoParallel(cores=no_cores)
cl <- makeCluster(no_cores, type="FORK")
clusterExport(cl,list('overall_erpi'), envir = environment())
system.time(result_impcplerpi <- parLapply(cl,final_list$cap_id,impcplerpi))
stopCluster(cl)


# Loop thru all cases
pred_erpi<-list()
i=1
n2<-NROW(final_list)
for (i in 1:n2)
{
    pred_erpi[[i]]<-predict(result_impcplerpi[[i]], new=data.frame(imptrf= predicted_impression$pred_imp[i]))
    i=i+1
}

predicted_erpi<-pred_erpi%>%unlist()%>%cbind(final_list)%>%as.data.frame()%>%rename( 'pred_erpi'='.')
predicted_erpi$pred_erpi<-as.numeric(predicted_erpi$pred_erpi)


rsquared_erpi<-list()
n2<-NROW(final_list)
i=1
for (i in 1:n2)
{
    rsquared_erpi[[i]]<-rsq::rsq(result_impcplerpi[[i]])
    i=i+1
}
