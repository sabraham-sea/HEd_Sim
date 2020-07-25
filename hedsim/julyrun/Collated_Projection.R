
CPL<-readRDS('cpl_leadrevenue_Augpred4.RDS')

# Check original

CPL<-CPL%>%select(cap_id,forcast,forcast_curr,`Monthly Cap`,`Allowable Cap`,billable_revenue,n)
CPL$`Monthly Cap`<-as.numeric(CPL$`Monthly Cap`)


mnth_data1<-month_data_check%>%filter(month==6 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadspast=cpl_leads)
mnth_data2<-month_data_check%>%filter(month==7 & yr =='2020')%>%select(cap_id,cpl_leads)%>%rename(leadscurr=cpl_leads)
CPL_mnth<-left_join(CPL,mnth_data1,by='cap_id')

CPL_mnth<-left_join(CPL_mnth,mnth_data2,by='cap_id')

CPL_mnth[is.na(CPL_mnth)] <- 0




# Positive Values check
Positive<-CPL_mnth%>%filter(forcast > 0)

dt1<-data.table(Positive$leadspast,Positive$leadscurr)
Positive$max1<-do.call(`pmax`, dt1)
Positive<-Positive%>%mutate(delta=abs(max1-forcast))

# For small samples n() , delta should be small 
Positive$finalforcast<-if_else(((Positive$n < 20 & Positive$delta <=5)|(Positive$n > 20 & Positive$n <50 & Positive$delta <=10 )| 
                                  
                                  
                                  (Positive$n > 50 & Positive$n <80 & Positive$delta <=10 )  | (Positive$n > 80)),Positive$forcast,0)


Positive$bill<-Positive$finalforcast*Positive$billable_revenue







# Look at most data points)
dt1<-data.table(CPL_mnth$forcast,CPL_NN$forcast_curr)
CPL_mnth$max1<-do.call(`pmax`, dt1)

Maincases<-CPL_mnth%>%filter(n =20)


#Pull non negative CPL

# CPL_NN<-CPL_mnth%>%mutate(leadavg=(leadspast+leadscurr)/2)

CPL_NN<-CPL_mnth%>%mutate(leadavg=(leadspast+leadspast)/2)

# Check where forcast > monthly allowable
dt1<-data.table(CPL_NN$leadspast,CPL_NN$leadscurr)
CPL_NN$max1<-do.call(`pmax`, dt1)

CPL_NN$newforcast<-if_else(CPL_NN$forcast>CPL_NN$`Monthly Cap`,CPL_NN$max1,CPL_NN$forcast)

# Check negative values
CPL_NN$finforcast<-if_else(CPL_NN$newforcast < 1 ,CPL_NN$max1,CPL_NN$newforcast)

dt<-data.table(CPL_NN$forcast_curr,CPL_NN$leadavg)
CPL_NN$maxchk<-do.call(`pmax`, dt)

CPL_NN$finale<-if_else(CPL_NN$finforcast < CPL_NN$forcast_curr, CPL_NN$maxchk,CPL_NN$finforcast)
# CPL_NN<-CPL_NN%>%mutate(newbill= finforcast*billable_revenue)
CPL_NN<-CPL_NN%>%mutate(newbill= floor(finale)*billable_revenue)
sum(CPL_NN$newbill)

CPC<-readRDS('cpc_leadrevenue_Augpred4.RDS')

# Check original

CPC<-CPC%>%select(cap_id,forcast,forcast_curr,`Monthly Cap`,`Allowable Cap`,billable_revenue,n)
CPC$`Monthly Cap`<-as.numeric(CPC$`Monthly Cap`)

mnth_data1<-month_data_check%>%filter(month==6 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadspast=cpc_leads)
mnth_data2<-month_data_check%>%filter(month==7 & yr =='2020')%>%select(cap_id,cpc_leads)%>%rename(leadscurr=cpc_leads)
CPC_mnth<-left_join(CPC,mnth_data1,by='cap_id')

CPC_mnth<-left_join(CPC_mnth,mnth_data2,by='cap_id')

CPC_mnth[is.na(CPC_mnth)] <- 0

CPC_NN<-CPC_mnth%>%mutate(leadavg=(leadspast+leadspast)/2)

# Check where forcast > monthly allowable
dt1<-data.table(CPC_NN$leadspast,CPC_NN$leadscurr)
CPC_NN$max1<-do.call(`pmax`, dt1)

CPC_NN$newforcast<-if_else(CPC_NN$forcast>CPC_NN$`Monthly Cap`,CPC_NN$max1,CPC_NN$forcast)
# Check negative values
CPC_NN$finforcast<-if_else(CPC_NN$newforcast < 1 ,CPC_NN$max1,CPC_NN$newforcast)
dt<-data.table(CPC_NN$forcast_curr,CPC_NN$leadavg)
CPC_NN$maxchk<-do.call(`pmax`, dt)
CPC_NN$finale<-if_else(CPC_NN$finforcast < CPC_NN$leadavg, CPC_NN$maxchk,CPC_NN$finforcast)
# CPL_NN<-CPL_NN%>%mutate(newbill= finforcast*billable_revenue)
CPC_NN<-CPC_NN%>%mutate(newbill= floor(finale)*billable_revenue)
sum(CPC_NN$newbill)

AugProj<-rbind(CPL_NN,CPC_NN)

sum(AugProj$newbill)

# 0 cases

bb<-AugProj%>%filter(finforcast==0)

Auglist<-AugProj%>%select(cap_id,`Monthly Cap`,`Allowable Cap`,billable_revenue,orgforc=forcast,forcast=finale, leadspast,leadscurr,billable_rev=newbill)

dt<-data.table(Auglist$orgforc,Auglist$forcast)
Auglist$maxchk<-do.call(`pmax`, dt)

Auglist<-Auglist%>%rename(finalforcast=maxchk)%>%
  Auglist<-Auglist%>%mutate(newbill= finalforcast*billable_revenue)


saveRDS(Auglist,'AugForcast.RDS')

