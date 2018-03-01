#### GENERAL SET UP ####
# rm(list=ls())
# setwd("~/Dropbox (FFRC)/NWOS/GENERAL/PRODUCTS/SCIENCE/WEIGHTING/ANALYSIS")
# library(stringr)
# library(scales)
install.packages("boot")
library(boot)

# library(nwos)
# source("nwos/R/nwos_data.R")
source("nwos_response_rate.R")
source("nwos_weights.R")
source("nwos_total.R")
# source("nwos_variance.R")
source("nwos_variance_boot_aws.R")
# source("nwos_mean.R")
# source("nwos_proportion.R")
# source("nwos_quantile.R")

stratum.area <- 35198019

#### Load Data ####
load("SAMPLE_DATA_W_WEIGHTS.RData")

#### Blank Data Frame ####
sample.stats.size <- data.frame(sample=character(0),
                           stratum=character(0),
                           domain=character(0),
                           subdomain=character(0),
                           subsubdomain=character(0),
                           value=numeric(0),
                           units=character(0),
                           stat=character(0))

#### ESTIMATES ####
sample.list <- unique(sample.data$sample)
for(i in 1:NROW(sample.list))
# for(i in 1:1)
{
  print(i)
  sample.i <- subset(sample.data, sample==sample.list[i])
  sample.i$domain <- ifelse(sample.i$owner.class=="FamilyForest", 1, 0)
  sample.i$y <- 1
  
  # Distribution by Size
  size.list <- c("1-9", "10-19", "20-49", "50-99", "100-199", "200-499", "500-999", "1000+")
  size.cat <- cut(sample.i$area, c(1,10,20,50,100,200,500,1000,Inf), right=F)
  size.cat.list <- levels(size.cat)
  for(j in 1:NROW(size.cat.list))
  {
    size.y <- ifelse(is.na(size.cat), 0, ifelse(size.cat==size.cat.list[j], 1, 0))
    sample.i.own.tot.size <- nwosTotal(sample.i$weight, sample.i$point.count, sample.i$domain,
                                       size.y,
                                       sample.i$area, units="ownerships")
    sample.i.ac.tot.size <- nwosTotal(sample.i$weight, sample.i$point.count, sample.i$domain,
                                      size.y,
                                      sample.i$area, units="acres")
    sample.stats.size <- data.frame(rbind(sample.stats.size,
                                          data.frame(sample=sample.list[i],
                                                     stratum="WI",
                                                     domain="FamilyForest",
                                                     subdomain=paste("size",size.cat.list[j],sep=""),
                                                     subsubdomain=NA,
                                                     value=c(sample.i.own.tot.size, sample.i.ac.tot.size),
                                                     units=c("ownerships", "acres"),
                                                     stat=c("total","total"))))
    
    # sample.i.own.tot.var.size <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain,
    #                                      size.y,
    #                                      sample.i$area, sample.i$response, stratum.area,
    #                                      units="ownerships", stat="total", R=2500)
    # sample.i.ac.tot.var.size <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain,
    #                                     size.y,
    #                                     sample.i$area, sample.i$response, stratum.area,
    #                                     units="acres", stat="total", R=2500)
    # sample.stats.size <- data.frame(rbind(sample.stats.size,
    #                                  data.frame(sample=sample.list[i],
    #                                             stratum="WI",
    #                                             domain="FamilyForest",
    #                                             subdomain=paste("size",size.cat.list[j],sep=""),
    #                                             subsubdomain=NA,
    #                                             value=c(sample.i.own.tot.size, sample.i.ac.tot.size,
    #                                                     sample.i.own.tot.var.size, sample.i.ac.tot.var.size),
    #                                             units=c("ownerships", "acres", "ownerships", "acres"),
    #                                             stat=c("total","total","totalVar","totalVar"))))
  }
}

save(sample.stats.size, file="sample_stats_size.RData")

