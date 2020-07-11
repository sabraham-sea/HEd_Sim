trf_daily_final<-readRDS('trf_daily_final_July.RDS')
trf_daily_clean<-trf_daily_final%>%mutate_at(c(12,14,16), ~replace(., is.na(.), 0))
month_data_check<-trf_daily_clean%>%dplyr::select(cap_id, school_name,date,degree_name,subject_name,accepted_cpl_leads,accepted_cpl_appviews,accepted_cpc_clicks,accepted_cpc_leads,
                                                                        accepted_revenue,billable_revenue,revised_revenue,
                                                                        `daily_new$combined_daily.dcs_traffic`)%>%mutate(month=month(date),yr=year(date))%>%
  group_by(cap_id,school_name,month,yr)%>%
  summarize(cpl_leads=sum(accepted_cpl_leads),cpl_views=sum(accepted_cpl_appviews),cpc_clicks=sum(accepted_cpc_clicks),
            cpc_leads=sum(accepted_cpc_leads),imptrf=sum(`daily_new$combined_daily.dcs_traffic`))%>%filter(!is.na(cap_id))%>%ungroup()