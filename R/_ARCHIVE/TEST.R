nwosTotal(w,
                     stratum=stratum,
                     domain=domain,
                     subdomain=nwos.data.st.r$subdomain,
                     subdomain.name=subdomain.name,
                     subsubdomain=nwos.data.st.r$subsubdomain,
                     subsubdomain.name=subsubdomain.name,
                     units=units)

nwosTotal(nwos.data,
          stratum=stratum,
          domain=domain,
          subdomain=nwos.data.st.r$subdomain,
          subdomain.name=subdomain.name,
          subsubdomain=nwos.data.st.r$subsubdomain,
          subsubdomain.name=subsubdomain.name,
          units=units)

summary(w)
summary(nwos.data)
summary(subset(w,domain=="FamilyForest"))
summary(subset(nwos.data,domain=="FamilyForest"))



> sum(nwos.data.sample$point.count[nwos.data.sample$sample==1])
s1 <- nwos.data.sample[nwos.data.sample$sample==7,]

A<-35198019
n.d <- tapply(s1$point.count, s1$domain, sum)
A * (n.d / (sum(n.d))) # Nonforest and UnknownForest are high - Issue with nwosData (?)
sum(A * (n.d / (sum(n.d))))
# PARCEL 11317501

sum(OWNER.MERGED$a)
sum(OWNER$ACRES_TOTAL)
sum(OWNER$ACRES_FOREST)
tapply(OWNER$ACRES_FOREST, OWNER$OWNCD, sum)
sum(OWNER$ACRES_NONFOREST) # 14033720
sum(OWNER$ACRES_FOREST[OWNER$OWNCD=="FAMILY"])
sum(OWNER$ACRES_FOREST[OWNER$OWNCD=="FAMILY" & OWNER$ACRES_FOREST>=1])
sum(OWNER.MERGED$a[OWNER.MERGED$o==1])

NROW(s1)

11317501 / 2108


View(s1[s1$point.count>1 & s1$domain=="FamilyForest",])


table(s1$point.count[s1$domain=="FamilyForest"])
s1$area[s1$point.count>1 & s1$domain=="FamilyForest"]
summary(s1$area[s1$domain=="FamilyForest"])
summary(s1$weight[s1$domain=="FamilyForest"])
summary(s1$area[s1$domain=="FamilyForest"])

n <- sum(s1$point.count) # 6889
A/n
n.ffo <- sum(s1$point.count[s1$domain=="FamilyForest"]) # 2159


11317501-11159010

158491/11317501

2159/6889 # 0.3133982

0.3133982 * 35198019 # 11030996

# Estimated Total = 11000338.9

11030996 - 11000338.9

30657.1 / 11030996

summary(df.s)











#### GENERAL SET UP ####
rm(list=ls())
setwd("~/Dropbox (FFRC)/NWOS/GENERAL/PRODUCTS/SCIENCE/WEIGHTING")

#### Build package ####
# library("devtools")
# library("roxygen2")
# document()

#### POPULATION STATS ####
load("DATA/OWNER/OWNER_20180106.RData")
# load("DATA/OWNER/OWNER_MERGED_20180106.RData")
load("DATA/OWNER/OWNER_MERGED_W_SIMULATED_DATA_20180106.RData")
# View(OWNER.MERGED[OWNER.MERGED$a>10000 & OWNER.MERGED$o==1,])
# View(OWNER[OWNER$ID%in%OWNER.MERGED$i[OWNER.MERGED$a>2500 & OWNER.MERGED$o==1],])

# Total Land Area
sum(OWNER$ACRES_TOTAL) # 35198019
sum(OWNER$ACRES_FOREST)
sum(OWNER$ACRES_FOREST) / sum(OWNER$ACRES_TOTAL)
sum(OWNER$ACRES_NONFOREST)
tapply(OWNER$ACRES_FOREST, OWNER$OWNCD, sum)

# Forest Land Area
sum(OWNER.MERGED$a)
tapply(OWNER.MERGED$a, OWNER.MERGED$o, sum)
summary(OWNER.MERGED$a[OWNER.MERGED$o==1])
summary(OWNER.MERGED$a[OWNER.MERGED$o==1 & OWNER.MERGED$a>=1])
summary(OWNER.MERGED$a[OWNER.MERGED$o==1 & OWNER.MERGED$a>=10])

# Number of Ownerships
summary(OWNER.MERGED$a)
summary(OWNER.MERGED$o)
NROW(OWNER.MERGED$i[OWNER.MERGED$o==1 & OWNER.MERGED$a>=1]) # 524551
NROW(OWNER.MERGED$i[OWNER.MERGED$o==1 & OWNER.MERGED$a>=10]) # 208340


#### WEIGHTS ####
load("ANALYSIS/R/nwos/data/nwos_data_20180107.RData")
nwos.response.rate <- nwosResponseRates(nwos.data)
nwos.response.rate
nwos.data.weights <- nwosWeights(nwos.data,
                                 stratum.area=data.frame(stratum="WI", area=35198019),
                                 response.rate=nwos.response.rate)

#### Import test data ####
load("DATA/SAMPLE/SAMPLE_0001.RData")




#### Load nwos.data ####
library(stringr)
sample.list <- list.files("DATA/SAMPLE/", "RData", full.names=T)
for(i in 1:NROW(sample.list))
{
  print(i)
  load(sample.list[i])
  sample.data <- df.s
  rm(df.s)
  sample.data$st <- "WI"
  if(i==1)
    nwos.data.sample <- nwosData(sample.data)
  else
    nwos.data.sample <- data.frame(rbind(nwos.data.sample,
                                         nwosData(sample.data)))
  # assign(paste("nwos_data_", str_pad(i,4,"left",pad="0"), sep=""), nwos.data)
  # save(list=paste("nwos_data_", str_pad(i,4,"left",pad="0"), sep=""),
  #      file=paste("ANALYSIS/R/nwos/data/nwos_data_", str_pad(i,4,"left",pad="0"), ".RData", sep=""))
}

#' nwos.response.rate <- nwosResponseRates(nwos.data)
#' nwos.data.weights <-
#'     nwosWeights(nwos.data,
#'     stratum.area=data.frame(stratum="WI", area=41918720),
#'     response.rate=nwos.response.rate)

rm(list=ls())
load("ANALYSIS/R/nwos/data/nwos_data_20180106.RData")

summary(nwos.data)

#### Response Rate ####
source("ANALYSIS/R/nwos/R/nwos_response_rate.R")
nwos.rr <- nwosResponseRates(response=nwos.data$response, stratum=nwos.data$stratum, domain=nwos.data$domain)
nwos.rr

#### Weights ####
source("ANALYSIS/R/nwos/R/nwos_weights.R")
nwos.df <- nwos.data
rm(nwos.data)
names(nwos.data)
id=nwos.data$id
area=nwos.data$area
point.count=nwos.data$point.count
response=nwos.data$response
stratum=nwos.data$stratum
domain=nwos.data$domain
stratum.area=data.frame(stratum="WI", area=41918720)
response.rate=nwos.rr

nwos.weights <- nwosWeights(nwos.data=nwos.df,
                            stratum.area=data.frame(stratum="WI", area=41918720),
                            response.rate=nwos.rr)
summary(nwos.weights)
tapply(sum(nwos.weights$weight)
pc.sum <- tapply(nwos.data$point.count, nwos.data$domain, sum)
pc.prop <- pc.sum / sum(pc.sum, na.rm=T)
pc.prop * 41918720

# Area nonforest: 17,742,342 ac
# Area forest: 24,176,379 ac # Too high by 7 MM

pc.sum <- tapply(nwos.data$point.count[nwos.data$domain!="Nonforest"], nwos.data$domain[nwos.data$domain!="Nonforest"], sum)
pc.prop <- pc.sum / sum(pc.sum, na.rm=T)
pc.prop


load("~/Dropbox (FFRC)/NWOS/GENERAL/PRODUCTS/SCIENCE/WEIGHTING/DATA/OWNER/OWNER_FFO.RData")
names(OWNER.FFO)

load("~/Dropbox (FFRC)/NWOS/GENERAL/PRODUCTS/SCIENCE/WEIGHTING/DATA/OWNER/OWNER_20181219.RData")
names(OWNER)
summary(factor(OWNER$OWNCD))
OWNER.FFO.AC <- (OWNER$ACRES_FOREST[OWNER$OWNCD=="FAMILY"])
summary(OWNER.FFO.AC)

load("~/Dropbox (FFRC)/NWOS/GENERAL/PRODUCTS/SCIENCE/WEIGHTING/DATA/OWNER/OWNER_MERGED_20181223.RData")
names(OWNER.MERGED)
OWNER.MERGED.FFO.AC <- (OWNER.MERGED$a[OWNER.MERGED$o==1])
summary(OWNER.MERGED.FFO.AC[OWNER.MERGED.FFO.AC<689963])

large.owners <- OWNER.MERGED$i[OWNER.MERGED$a>=40000]
large.ffo <- OWNER.MERGED$i[OWNER.MERGED$a>=1000 & OWNER.MERGED$o==1]

View(OWNER[OWNER$ID%in%large.owners,])
View(OWNER[OWNER$ID%in%large.ffo,])
OWNER$NAME[OWNER$ID%in%large.ffo]

View(OWNER[OWNER$ID==1422290,])
View(OWNER[OWNER$ID==1950655,])
View(OWNER[OWNER$ID==2123773,])
View(OWNER[OWNER$ID==2127066,])
View(OWNER[OWNER$ID==,])

