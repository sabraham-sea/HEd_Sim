install.packages("MonteCarlo")
library(MonteCarlo)
Then we define the following function.

#########################################
##      Example: t-test

# Define function that generates data and applies the method of interest

ttest<-function(n){
  
  # generate sample:
  sample<-rnorm(n,0,1)
  
  # calculate test statistic:
  stat<-sqrt(n)*mean(sample)/sd(sample)
  
  # get test decision:
  decision<-abs(stat)>1.96
  
  # return result:
  return(list("decision"=decision))
}


# Example without parallization
n_grid<-c(50,100,250,500)
# loc_grid<-seq(0,1,0.2)
# scale_grid<-c(1,2)

loc=0
scale<-1

param_list=list("n"=n_grid)
erg<-MonteCarlo(func=ttest, nrep=250, param_list=param_list, ncpus=1)
summary(erg)

rows<-c("n")
cols<-c("loc","scale")
MCres<-MakeTable(output=erg, rows=rows, cols=cols, digits=2)
df<-MakeFrame(erg)



