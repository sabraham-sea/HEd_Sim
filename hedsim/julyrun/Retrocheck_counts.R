require(lubridate)

trf_daily_final<-readRDS('trf_daily_final_July23.RDS')
trf_daily_clean<-trf_daily_final
month_data_check<-trf_daily_clean%>%dplyr::select(cap_id, school_name,date,degree_name,subject_name,cpl_leads,cpl_views,cpc_clicks,cpc_leads,
                                                                        
                                                                       imptraf)%>%mutate(month=month(date),yr=year(date))%>%
  group_by(cap_id,school_name,month,yr)%>%
  summarize(cpl_leads=sum(cpl_leads),cpl_views=sum(cpl_views),cpc_clicks=sum(cpc_clicks),
            cpc_leads=sum(cpc_leads),imptrf=sum(imptraf))%>%filter(!is.na(cap_id))%>%ungroup()


# CDM traffic clean 

# month_traffic_org<-CDM_traffic_clean%>%mutate(month=month(date),yr=year(date))%>%group_by(month,yr,cap_id)%>%summarise(tottraff=sum(dcs_traffic))
# month_traffic_lat<-trf_daily_clean%>%mutate(month=month(date),yr=year(date))%>%group_by(month,yr,cap_id)%>%summarise(tottraff=sum(`daily_new$combined_daily.dcs_traffic`))