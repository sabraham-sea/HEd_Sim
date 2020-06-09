ttest<-function(n,loc,scale){
  
  sample<-rnorm(n, loc, scale)
  stat<-sqrt(n)*mean(sample)/sd(sample)
  return(list("stat"=stat))
}


# define parameter grid:

n_grid<-c(50,100,250,500)
loc_grid<-seq(0,1,0.2)
scale_grid<-c(1,2)

# collect parameter grids in list:
param_list=list("n"=n_grid, "loc"=loc_grid, "scale"=scale_grid)
MC_result<-MonteCarlo(func=ttest, nrep=1000, param_list=param_list)
df<-MakeFrame(MC_result)
head(df)

library(dplyr)
library(ggplot2)
tbl <- tbl_df(df)
ggplot(filter(tbl, loc==0, scale==1)) + geom_density(aes(x=stat, col=factor(n)))

t<-rnorm(50,0,1)
ggplot(t,)