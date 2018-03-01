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
source("nwos_variance_boot.R")
source("nwos_mean.R")
source("nwos_proportion.R")
source("nwos_quantile.R")

stratum.area <- 35198019

#### Load Data ####
load("SAMPLE_DATA_W_WEIGHTS.RData")

#### Blank Data Frame ####
sample.stats <- data.frame(sample=character(0),
                           stratum=character(0),
                           domain=character(0),
                           subdomain=character(0),
                           subsubdomain=character(0),
                           value=numeric(0),
                           units=character(0),
                           stat=character(0))

#### ESTIMATES ####
sample.list <- unique(sample.data$sample)
ptm <- proc.time()
for(i in 1:NROW(sample.list))
# for(i in 1:1)
{
  print(i)
  sample.i <- subset(sample.data, sample==sample.list[i])
  sample.i$domain <- ifelse(sample.i$owner.class=="FamilyForest", 1, 0)
  sample.i$y <- 1
  
  # Totals
  print("Totals")
  sample.i.own.tot <- nwosTotal(sample.i$weight, sample.i$point.count, sample.i$domain, sample.i$y, 
                                sample.i$area, units="ownerships")
  sample.i.ac.tot <- nwosTotal(sample.i$weight, sample.i$point.count, sample.i$domain, sample.i$y, 
                               sample.i$area, units="acres")
  sample.stats <- data.frame(rbind(sample.stats,
                                   data.frame(sample=sample.list[i],
                                              stratum="WI",
                                              domain="FamilyForest",
                                              subdomain=NA,
                                              subsubdomain=NA,
                                              value=c(sample.i.own.tot, sample.i.ac.tot),
                                              units=c("ownerships", "acres"),
                                              stat="total")))
  # Totals Variance
  sample.i.own.tot.var <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain, sample.i$y, 
                                  sample.i$area, sample.i$response, stratum.area, 
                                  units="ownerships", stat="total", R=2500)
  sample.i.ac.tot.var <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain, sample.i$y, 
                                  sample.i$area, sample.i$response, stratum.area, 
                                  units="acres", stat="total", R=2500)
  sample.stats <- data.frame(rbind(sample.stats,
                                   data.frame(sample=sample.list[i],
                                              stratum="WI",
                                              domain="FamilyForest",
                                              subdomain=NA,
                                              subsubdomain=NA,
                                              value=c(sample.i.own.tot.var, sample.i.ac.tot.var),
                                              units=c("ownerships", "acres"),
                                              stat="totalVar")))
  # Means
  print("Means")
  sample.i.own.mean <- nwosMean(sample.i$weight, sample.i$point.count, sample.i$domain, y=sample.i$area, # y=sample.i$y, 
                                sample.i$area, units="ownerships")
  sample.i.ac.mean <- nwosMean(sample.i$weight, sample.i$point.count, sample.i$domain, y=sample.i$area, # sample.i$y, 
                               sample.i$area, units="area")
  sample.stats <- data.frame(rbind(sample.stats,
                                   data.frame(sample=sample.list[i],
                                              stratum="WI",
                                              domain="FamilyForest",
                                              subdomain=NA,
                                              subsubdomain=NA,
                                              value=c(sample.i.own.mean, sample.i.ac.mean),
                                              units=c("ownerships", "acres"),
                                              stat="mean")))
  # Mean variances
  sample.i.own.mean.var <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain, y=sample.i$area, # y=sample.i$y,
                                sample.i$area, sample.i$response, stratum.area, units="ownerships", stat="mean", R=2500)
  sample.i.ac.mean.var <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain,  y=sample.i$area, # y=sample.i$y,
                                  sample.i$area, sample.i$response, stratum.area, units="area", stat="mean", R=2500)
  sample.stats <- data.frame(rbind(sample.stats,
                                   data.frame(sample=sample.list[i],
                                              stratum="WI",
                                              domain="FamilyForest",
                                              subdomain=NA,
                                              subsubdomain=NA,
                                              value=c(sample.i.own.mean.var, sample.i.ac.mean.var),
                                              units=c("ownerships", "acres"),
                                              stat="meanVar")))
  # Quantiles
  print("Quantiles")
  sample.i.own.quant <- nwosQuantile(sample.i$weight, sample.i$point.count, sample.i$domain, sample.i$y, 
                                   sample.i$area, units="ownerships", prob=c(0.00, 0.25, 0.50, 0.75, 1.00), max.iter=100)
  sample.stats <- data.frame(rbind(sample.stats,
                                   data.frame(sample=sample.list[i],
                                              stratum="WI",
                                              domain="FamilyForest",
                                              subdomain=NA,
                                              subsubdomain=NA,
                                              value=sample.i.own.quant,
                                              units="ownerships",
                                              stat=paste("quantProb", 
                                                         as.numeric(names(sample.i.own.quant)), sep=""))))
  # Distribution by Size
  # size.list <- c("1-9", "10-19", "20-49", "50-99", "100-199", "200-499", "500-999", "1000+")
  # size.cat <- cut(sample.i$area, c(1,10,20,50,100,200,500,1000,Inf), right=F)
  # size.cat.list <- levels(size.cat)
  # for(j in 1:NROW(size.cat.list))
  # {
  #   size.y <- ifelse(is.na(size.cat), 0, ifelse(size.cat==size.cat.list[j], 1, 0))
  #   sample.i.own.tot.size <- nwosTotal(sample.i$weight, sample.i$point.count, sample.i$domain, 
  #                                      size.y,
  #                                      sample.i$area, units="ownerships")
  #   sample.i.ac.tot.size <- nwosTotal(sample.i$weight, sample.i$point.count, sample.i$domain, 
  #                                     size.y, 
  #                                     sample.i$area, units="acres")
  #   sample.i.own.tot.var.size <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain, 
  #                                        size.y,
  #                                        sample.i$area, sample.i$response, stratum.area, 
  #                                        units="ownerships", stat="total", R=2500)
  #   sample.i.ac.tot.var.size <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain, 
  #                                       size.y, 
  #                                       sample.i$area, sample.i$response, stratum.area, 
  #                                       units="acres", stat="total", R=2500)
  #   sample.stats <- data.frame(rbind(sample.stats,
  #                                    data.frame(sample=sample.list[i],
  #                                               stratum="WI",
  #                                               domain="FamilyForest",
  #                                               subdomain=paste("size",size.cat.list[j],sep=""),
  #                                               subsubdomain=NA,
  #                                               value=c(sample.i.own.tot.size, sample.i.ac.tot.size,
  #                                                       sample.i.own.tot.var.size, sample.i.ac.tot.var.size),
  #                                               units=c("ownerships", "acres", "ownerships", "acres"),
  #                                               stat=c("total","total","totalVar","totalVar"))))
  # }
  # y_1
  print("y_1")
  sample.i.own.prop.y1 <- nwosProportion(sample.i$weight, sample.i$point.count, sample.i$domain, 
                 as.numeric(as.character(sample.i$y_1)), sample.i$area, units="ownerships")
  sample.i.ac.prop.y1 <- nwosProportion(sample.i$weight, sample.i$point.count, sample.i$domain, 
                 as.numeric(as.character(sample.i$y_1)), sample.i$area, units="area")
  sample.i.own.prop.y1.var <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain, 
                                         as.numeric(as.character(sample.i$y_1)), 
                                         sample.i$area, sample.i$response, stratum.area, 
                                         units="ownerships", stat="proportion", R=2500)
  sample.i.ac.prop.y1.var <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain, 
                                     as.numeric(as.character(sample.i$y_1)), 
                                     sample.i$area, sample.i$response, stratum.area, 
                                     units="area", stat="proportion", R=2500)
  sample.stats <- data.frame(rbind(sample.stats,
                                   data.frame(sample=sample.list[i],
                                              stratum="WI",
                                              domain="FamilyForest",
                                              subdomain="y_1",
                                              subsubdomain=NA,
                                              value=c(sample.i.own.prop.y1, sample.i.ac.prop.y1,
                                                      sample.i.own.prop.y1.var, sample.i.ac.prop.y1.var),
                                              units=c("ownerships", "acres"),
                                              stat=c("proportion","proportion",
                                                     "proportionVar","proportionVar"))))
  # y_2
  print("y_2")
  sample.i.own.prop.y2 <- nwosProportion(sample.i$weight, sample.i$point.count, sample.i$domain, 
                                         as.numeric(as.character(sample.i$y_2)), sample.i$area, units="ownerships")
  sample.i.ac.prop.y2 <- nwosProportion(sample.i$weight, sample.i$point.count, sample.i$domain, 
                                        as.numeric(as.character(sample.i$y_2)), sample.i$area, units="area")
  sample.i.own.prop.y2.var <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain, 
                                      as.numeric(as.character(sample.i$y_2)), 
                                      sample.i$area, sample.i$response, stratum.area, 
                                      units="ownerships", stat="proportion", R=2500)
  sample.i.ac.prop.y2.var <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain, 
                                     as.numeric(as.character(sample.i$y_2)), 
                                     sample.i$area, sample.i$response, stratum.area, 
                                     units="area", stat="proportion", R=2500)
  sample.stats <- data.frame(rbind(sample.stats,
                                   data.frame(sample=sample.list[i],
                                              stratum="WI",
                                              domain="FamilyForest",
                                              subdomain="y_2",
                                              subsubdomain=NA,
                                              value=c(sample.i.own.prop.y2, sample.i.ac.prop.y2,
                                                      sample.i.own.prop.y2.var, sample.i.ac.prop.y2.var),
                                              units=c("ownerships", "acres"),
                                              stat=c("proportion","proportion",
                                                     "proportionVar","proportionVar"))))
  # 
  # y_3
  print("y_3")
  sample.i.own.prop.y3 <- nwosMean(sample.i$weight, sample.i$point.count, sample.i$domain, 
                                         as.numeric(as.character(sample.i$y_3)), sample.i$area, units="ownerships")
  sample.i.ac.prop.y3 <- nwosMean(sample.i$weight, sample.i$point.count, sample.i$domain, 
                                        as.numeric(as.character(sample.i$y_3)), sample.i$area, units="area")
  sample.i.own.prop.y3.var <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain, 
                                      as.numeric(as.character(sample.i$y_3)), 
                                      sample.i$area, sample.i$response, stratum.area, 
                                      units="ownerships", stat="mean", R=2500)
  sample.i.ac.prop.y3.var <- nwosVarBoot(sample.i$weight, sample.i$point.count, sample.i$domain, 
                                     as.numeric(as.character(sample.i$y_3)), 
                                     sample.i$area, sample.i$response, stratum.area, 
                                     units="area", stat="mean", R=2500)
  sample.stats <- data.frame(rbind(sample.stats,
                                   data.frame(sample=sample.list[i],
                                              stratum="WI",
                                              domain="FamilyForest",
                                              subdomain="y_3",
                                              subsubdomain=NA,
                                              value=c(sample.i.own.prop.y3, sample.i.ac.prop.y3,
                                                      sample.i.own.prop.y3.var, sample.i.ac.prop.y3.var),
                                              units=c("ownerships", "acres"),
                                              stat=c("mean","mean",
                                                     "meanVar","meanVar"))))
}
proc.time() - ptm
# sample.stats

save(sample.stats, file="sample_stats.RData")

