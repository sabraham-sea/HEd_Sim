require(data.table)




CPL<-readRDS('All_Augpred10.RDS')%>%filter(Product=='LO')


mnth_data2<-month_data_check%>%filter(month==5 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast_tm2=cpl_leads)
mnth_data1<-month_data_check%>%filter(month==6 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast_tm1=cpl_leads)
mnth_data0<-month_data_check%>%filter(month==7 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast_t=cpl_leads)

CPL_mnth<-left_join(CPL,mnth_data0,by='cap_id')
CPL_mnth<-left_join(CPL_mnth,mnth_data1,by='cap_id')
CPL_mnth<-left_join(CPL_mnth,mnth_data2,by='cap_id')

CPL_mnth[is.na(CPL_mnth)] <- 0

# Positive Values check
CPL_mnth$finalforcast<-if_else((CPL_mnth$forcast <= 0),as.numeric(CPL_mnth$`Monthly Cap`),
                               if_else((CPL_mnth$forcast<1 & CPL_mnth$forcast>0),ceiling(CPL_mnth$forcast),CPL_mnth$forcast))





CPC<-readRDS('All_Augpred10.RDS')%>%filter(Product!='LO')

mnth_data2<-month_data_check%>%filter(month==5 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast_tm2=cpc_leads)
mnth_data1<-month_data_check%>%filter(month==6 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast_tm1=cpc_leads)
mnth_data0<-month_data_check%>%filter(month==7 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast_t=cpc_leads)
CPC_mnth<-left_join(CPC,mnth_data1,by='cap_id')

CPC_mnth<-left_join(CPC_mnth,mnth_data2,by='cap_id')
CPC_mnth<-left_join(CPC_mnth,mnth_data0,by='cap_id')


CPC_mnth[is.na(CPC_mnth)] <- 0

# Positive Values check
CPC_mnth$finalforcast<-if_else((CPC_mnth$forcast <= 0),as.numeric(CPC_mnth$`Monthly Cap`),
                               if_else((CPC_mnth$forcast<1 & CPC_mnth$forcast>0),ceiling(CPC_mnth$forcast),CPC_mnth$forcast))


FinalAugV3<-rbind(CPC_mnth,CPL_mnth)

dt = data.table(FinalAugV3)


# check for dupes
dtindex<-dt[duplicated(cap_id), cbind(.SD[1], number = .N), by = cap_id]

finaldt<-dt%>%filter( !(cap_id %in% c('1751','1014') & leadspast_t ==0))

m<-finaldt%>%select(`Monthly Cap`,finalforcast,forcast,n,leadspast_tm2,leadspast_tm1,leadspast_t)


# Multilpy to make billable
finaldt<-finaldt%>%mutate(forcast_billable=finalforcast*accepted_revenue*(1-Scrub))

saveRDS(finaldt,'FinalAugVersionJuly23.RDS')  # July 23

write.xlsx(finaldt, file = "FinalAug_July23.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)







# Check original and run Retrocheck

CPL<-CPL%>%select(cap_id,forcast,forcast_curr,`Monthly Cap`,`Allowable Cap`,revised_revenue,n)
CPL$`Monthly Cap`<-as.numeric(CPL$`Monthly Cap`)

mnth_data2<-month_data_check%>%filter(month==5 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast_tm2=cpl_leads)
mnth_data1<-month_data_check%>%filter(month==6 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast_tm1=cpl_leads)
mnth_data0<-month_data_check%>%filter(month==7 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast_t=cpl_leads)

CPL_mnth<-left_join(CPL,mnth_data0,by='cap_id')
CPL_mnth<-left_join(CPL_mnth,mnth_data1,by='cap_id')
CPL_mnth<-left_join(CPL_mnth,mnth_data2,by='cap_id')

CPL_mnth[is.na(CPL_mnth)] <- 0

# Positive Values check
PositiveCPL<-CPL_mnth%>%filter(forcast > 0)

dt1<-data.table(Positive$leadspast_tm1,Positive$leadspast_t)
Positive$max1<-do.call(`pmax`, dt1)
Positive<-Positive%>%mutate(delta=abs(max1-forcast))

# For small samples n() , delta should be small 
Positive$finalforcast<-if_else(((Positive$n < 20 & Positive$delta <=5)|(Positive$n > 20 & Positive$n <50 & Positive$delta <=10 )| 
                                  
                                  
                                  (Positive$n > 50 & Positive$n <80 & Positive$delta <=10 )  | (Positive$n > 80)),Positive$forcast,0)


Positive$bill<-Positive$finalforcast*Positive$revised_revenue



saveRDS(Positive,'CPL_AugPred4.RDS')


CPC<-readRDS('cpc_leadrevenue_Augpred8.RDS')%>%rename('forcast_lower'='forcast_upper','forcast_revised_lower'='forcast_revised_upper')

# Check original

CPC<-CPC%>%select(cap_id,forcast,forcast_curr,`Monthly Cap`,`Allowable Cap`,revised_revenue,n)
CPC$`Monthly Cap`<-as.numeric(CPC$`Monthly Cap`)

mnth_data2<-month_data_check%>%filter(month==5 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast_tm2=cpc_leads)
mnth_data1<-month_data_check%>%filter(month==6 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast_tm1=cpc_leads)
mnth_data0<-month_data_check%>%filter(month==7 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast_t=cpc_leads)
CPC_mnth<-left_join(CPC,mnth_data1,by='cap_id')

CPC_mnth<-left_join(CPC_mnth,mnth_data2,by='cap_id')
CPC_mnth<-left_join(CPC_mnth,mnth_data0,by='cap_id')


CPC_mnth[is.na(CPC_mnth)] <- 0

# Positive Values check
PositiveCPC<-CPC_mnth%>%filter(forcast > 0)

dt1<-data.table(Positive$leadspast_tm1,Positive$leadspast_t)
Positive$max1<-do.call(`pmax`, dt1)
Positive<-Positive%>%mutate(delta=abs(max1-forcast))

# For small samples n() , delta should be small 
Positive$finalforcast<-if_else(((Positive$n < 20 & Positive$delta <=5)|(Positive$n > 20 & Positive$n <50 & Positive$delta <=10 )| 
                                  
                                  
                                  (Positive$n > 50 & Positive$n <80 & Positive$delta <=10 )  | (Positive$n > 80)),Positive$forcast,0)


Positive$bill<-Positive$finalforcast*Positive$revised_revenue


saveRDS(Positive,'CPC_AugPred4.RDS')

CPC<-readRDS('CPC_AugPred4.RDS')
CPL<-readRDS('CPL_AugPred4.RDS')

FinalAugV3<-rbind(CPC,CPL)
# Combine with school name

FinalAugV3<-left_join(FinalAugV3,Current_caps,by=("cap_id"))


dt = data.table(FinalAugV3)


# check for dupes
dtindex<-dt[duplicated(cap_id), cbind(.SD[1], number = .N), by = cap_id]

finaldt<-dt%>%filter( !(cap_id == '1751' & leadspast_t ==0))


saveRDS(finaldt,'FinalAugVersion.RDS')  # July 23

write.xlsx(FinalAugV3, file = "FinalAug_July22.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)


##########
positive<-rbind(PositiveCPC,PositiveCPL)

dt = data.table(positive)
dtindex<-dt[duplicated(cap_id), cbind(.SD[1], number = .N), by = cap_id]

finaldt<-dt%>%filter( !(cap_id %in% c('1751',1014) & leadspast_t ==0))


FinalAugTest<-left_join(finaldt,Current_caps,by=("cap_id"))
saveRDS(FinalAugTest,'FinalAugTest.RDS')

# Adjustment for july 22nd

CPL<-readRDS('cpl_leadrevenue_Augpred8.RDS')%>%rename('forcast_lower'='forcast_upper')
CPL<-CPL%>%select(cap_id,forcast,forcast_curr,`Monthly Cap`,`Allowable Cap`,revised_revenue,accepted_revenue,n)
CPL$`Monthly Cap`<-as.numeric(CPL$`Monthly Cap`)

PositiveCPL<-CPL%>%filter(forcast > 0)
NegCPL<-CPL%>%filter(forcast < 0)
PositiveCPL<-PositiveCPL%>%mutate(acc_bill=accepted_revenue*forcast,rev_bill=revised_revenue*forcast)


CPC<-readRDS('cpc_leadrevenue_Augpred8.RDS')%>%rename('forcast_lower'='forcast_upper')
CPC<-CPC%>%select(cap_id,forcast,forcast_curr,`Monthly Cap`,`Allowable Cap`,revised_revenue,accepted_revenue,n)
CPC$`Monthly Cap`<-as.numeric(CPC$`Monthly Cap`)

PositiveCPC<-CPC%>%filter(forcast > 0)
PositiveCPC<-PositiveCPC%>%mutate(acc_bill=accepted_revenue*forcast,rev_bill=revised_revenue*forcast)


all<-rbind(PositiveCPC,PositiveCPL)

write.xlsx(all, file = "Adj_July22.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
