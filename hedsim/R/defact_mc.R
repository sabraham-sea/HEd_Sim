# MC Simulation
install.packages("MonteCarlo")
library(MonteCarlo)

# Get 30 days worth of data 
query_statement <- readr::read_file("/Users/sabraham/Documents/GitHub/HEd_Sim/hedsim/inst/DE_MCData.sql")
DE_MCData<- DBI::dbGetQuery(connection, query_statement)

# Get capid 1014

cap1014<-DE_MCData%>%filter(school_capid=='1014' & avg_erpi!='')


popmean<-0.423785
popsd <-0.908547240765334

mu=mean(cap1014$avg_erpi)
sd=sd(cap1014$avg_erpi)


erpi<-function(mu,sd){
  
    mu=mean(data$avg_erpi,na.rm=TRUE)
    sigma= sd(data$avg_erpi,na.rm=TRUE)
  return(list("mu"=mu,"sigma"=sigma))
}


# define parameter grid:

n_grid<-c(10,50)


# collect parameter grids in list:
param_list=list("n"=n_grid)
data<-cap1014
MC_result<-MonteCarlo(func=erpi, nrep=1000,param_list = )
df<-MakeFrame(MC_result)
head(df)

library(dplyr)
library(ggplot2)
tbl <- tbl_df(df)
ggplot(filter(tbl, loc==0, scale==1)) + geom_density(aes(x=stat, col=factor(n)))

t<-rnorm(50,0,1)
ggplot(t,aes())