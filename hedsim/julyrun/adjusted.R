require(data.table)

CPL<-readRDS('cpl_leadrevenue_Augpred8.RDS')%>%rename('forcast_lower'='forcast_upper')

# Check original and run Retrocheck

CPL<-CPL%>%select(cap_id,forcast,forcast_curr,`Monthly Cap`,`Allowable Cap`,revised_revenue,billable_revenue,acceptable_revenue,n)
CPL$`Monthly Cap`<-as.numeric(CPL$`Monthly Cap`)

mnth_data2<-month_data_check%>%filter(month==5 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast_tm2=cpl_leads)
mnth_data1<-month_data_check%>%filter(month==6 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast_tm1=cpl_leads)
mnth_data0<-month_data_check%>%filter(month==7 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast_t=cpl_leads)

CPL_mnth<-left_join(CPL,mnth_data0,by='cap_id')
CPL_mnth<-left_join(CPL_mnth,mnth_data1,by='cap_id')
CPL_mnth<-left_join(CPL_mnth,mnth_data2,by='cap_id')

CPL_mnth[is.na(CPL_mnth)] <- 0

# Positive Values check
PositiveCPL<-CPL_mnth%>%filter(forcast > 0)%>%select(-c(forcast_lower,forcast_curr,forcast_revised_upper,forcast_revised,
                                                     forcast_revised_upper))

PositiveCPL$finalforcast<-if_else((PositiveCPL$forcast<1),ceiling(PositiveCPL$forcast),PositiveCPL$forcast)

# dt1<-data.table(Positive$leadspast_tm1,Positive$leadspast_t)
# Positive$max1<-do.call(`pmax`, dt1)
# Positive<-Positive%>%mutate(delta=abs(max1-forcast))
# 
# # For small samples n() , delta should be small 
# Positive$finalforcast<-if_else(((Positive$n < 20 & Positive$delta <=5)|(Positive$n > 20 & Positive$n <50 & Positive$delta <=10 )| 
#                                   
#                                   
#                                   (Positive$n > 50 & Positive$n <80 & Positive$delta <=10 )  | (Positive$n > 80)),Positive$forcast,0)
# 
# 
# Positive$bill<-Positive$finalforcast*Positive$revised_revenue
# 
# 
# 
# saveRDS(Positive,'CPL_AugPred4.RDS')


CPC<-readRDS('cpc_leadrevenue_Augpred8.RDS')%>%rename('forcast_lower'='forcast_upper','forcast_revised_lower'='forcast_revised_upper')

# Check original

CPC<-CPC%>%select(cap_id,forcast,forcast_curr,`Monthly Cap`,`Allowable Cap`,revised_revenue,billable_revenue,acceptable_revenue,n)
CPC$`Monthly Cap`<-as.numeric(CPC$`Monthly Cap`)

mnth_data2<-month_data_check%>%filter(month==5 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast_tm2=cpc_leads)
mnth_data1<-month_data_check%>%filter(month==6 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast_tm1=cpc_leads)
mnth_data0<-month_data_check%>%filter(month==7 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast_t=cpc_leads)
CPC_mnth<-left_join(CPC,mnth_data1,by='cap_id')

CPC_mnth<-left_join(CPC_mnth,mnth_data2,by='cap_id')
CPC_mnth<-left_join(CPC_mnth,mnth_data0,by='cap_id')


CPC_mnth[is.na(CPC_mnth)] <- 0

# Positive Values check
PositiveCPC<-CPC_mnth%>%filter(forcast > 0)%>%select(-c(forcast_lower,forcast_curr,forcast_revised,
                                                        forcast_revised_lower))


PositiveCPC$finalforcast<-if_else((PositiveCPC$forcast<1),ceiling(PositiveCPC$forcast),PositiveCPC$forcast)

# dt1<-data.table(Positive$leadspast_tm1,Positive$leadspast_t)
# Positive$max1<-do.call(`pmax`, dt1)
# Positive<-Positive%>%mutate(delta=abs(max1-forcast))
# 
# # For small samples n() , delta should be small 
# Positive$finalforcast<-if_else(((Positive$n < 20 & Positive$delta <=5)|(Positive$n > 20 & Positive$n <50 & Positive$delta <=10 )| 
#                                   
#                                   
#                                   (Positive$n > 50 & Positive$n <80 & Positive$delta <=10 )  | (Positive$n > 80)),Positive$forcast,0)
# 
# 
# Positive$bill<-Positive$finalforcast*Positive$revised_revenue
# 
# 
# saveRDS(Positive,'CPC_AugPred4.RDS')
# 
# CPC<-readRDS('CPC_AugPred4.RDS')
# CPL<-readRDS('CPL_AugPred4.RDS')

FinalAugV3<-rbind(PositiveCPC,PositiveCPL)
# Combine with school name



dt = data.table(FinalAugV3)


# check for dupes
dtindex<-dt[duplicated(cap_id), cbind(.SD[1], number = .N), by = cap_id]

finaldt<-dt%>%filter( !(cap_id%in% c('1751','1014') & leadspast_t ==0))

finaldt<-finaldt%>%mutate(finalbill=finalforcast*accepted_revenue*(1-Scrub))


# Combine with erpi data

DE_MCData<-readRDS('DE_MCData3.RDS')
finaldt1<-left_join(finaldt,DE_MCData,by=)






saveRDS(finaldt,'Finalforcast.RDS')

write.xlsx(finaldt, file = "Adj_July22.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
