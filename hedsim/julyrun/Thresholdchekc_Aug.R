require(data.table)




CPL<-readRDS('All_Augpred10_valattest.RDS')%>%filter(Product=='LO')


mnth_data2<-month_data_check%>%filter(month==5 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast_tm2=cpl_leads)
mnth_data1<-month_data_check%>%filter(month==6 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast_tm1=cpl_leads)
mnth_data0<-month_data_check%>%filter(month==7 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast_t=cpl_leads)

CPL_mnth<-left_join(CPL,mnth_data0,by='cap_id')
CPL_mnth<-left_join(CPL_mnth,mnth_data1,by='cap_id')
CPL_mnth<-left_join(CPL_mnth,mnth_data2,by='cap_id')

CPL_mnth[is.na(CPL_mnth)] <- 0




CPC<-readRDS('All_Augpred10_valattest.RDS')%>%filter(Product!='LO')

mnth_data2<-month_data_check%>%filter(month==5 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast_tm2=cpc_leads)
mnth_data1<-month_data_check%>%filter(month==6 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast_tm1=cpc_leads)
mnth_data0<-month_data_check%>%filter(month==7 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast_t=cpc_leads)
CPC_mnth<-left_join(CPC,mnth_data1,by='cap_id')

CPC_mnth<-left_join(CPC_mnth,mnth_data2,by='cap_id')
CPC_mnth<-left_join(CPC_mnth,mnth_data0,by='cap_id')


CPC_mnth[is.na(CPC_mnth)] <- 0

all<-rbind(CPC_mnth,CPL_mnth)


dt = data.table(all)


# check for dupes
dtindex<-dt[duplicated(cap_id), cbind(.SD[1], number = .N), by = cap_id]

finaldt<-dt%>%filter( !(cap_id %in% c('1751','1014') & leadspast_t ==0))

findt<-data.table(finaldt$CPL,finaldt$accepted_revenue)
finaldt$maxchk<-do.call(`pmax`, findt)

all1<-finaldt


all1$rowmeans<-rowMeans(all1[, c('leadspast_t', 'leadspast_tm1','leadspast_tm2')])

rowVars <- function(x, na.rm=F) {
  # Vectorised version of variance filter
  rowSums((x - rowMeans(x, na.rm=na.rm))^2, na.rm=na.rm) / (ncol(x) - 1)
}


all1$sd<-sqrt(rowVars(all1[, c('leadspast_t', 'leadspast_tm1','leadspast_tm2')]))
all1<-all1%>%rowwise()%>%mutate(lds_sd=sd+leadspast_t)

# -VE VALUES

# b<-all1%>%filter(forcast < 0 & as.numeric(`Monthly Cap`) > 500)

all1$finalforcast<-if_else(all1$forcast <= 1,as.numeric(all1$`Monthly Cap`),all1$forcast)
all1$forcast_bill<-all1$finalforcast* all1$maxchk *(1-all1$Scrub)
sum(all1$forcast_bill)

#all1$diffm<-all1$finalforcast-all1$leadspast_t 
all1$diffm2<-all1$finalforcast-all1$rowmeans
all2<-all1


#all2$ff<-if_else(all2$diffm<2,all2$lds_sd,all2$finalforcast)
all2$ff2<-if_else(all2$diffm2<1,all2$lds_sd,all2$finalforcast)


#all2$ff_bill<-ceiling(all2$ff)*all2$maxchk*(1-all2$Scrub)
all2$ff2_bill<-ceiling(all2$ff2)*all2$maxchk*(1-all2$Scrub)

#sum(all2$ff_bill)
sum(all2$ff2_bill)

finalreport<-all2%>%select(cap_id,n,provider_id,provider_cap,Product,school_name,account_manager_name,`Cap Name`,provider_name,`Monthly Cap`,`Allowable Cap`,`School Allowable`,`Provider Allowable`,
                      `AM Allowable`,accepted_revenue,leadsMay=leadspast_tm2,leadsJune=leadspast_tm1,leadsJuly=leadspast_t,forcast=ff2,forcast_billable=ff2_bill)


sum(finalreport$forcast_billable)
dt = data.table(finalreport)
# check for dupes
dtindex<-dt[duplicated(cap_id), cbind(.SD[1], number = .N), by = cap_id]

saveRDS(all2,"All_Augpred10_valattest.RDS") # Final Version

write.xlsx(all2, file = "Adjusted_July24Org.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


write.xlsx(finalreport, file = "Adjusted_July24.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
