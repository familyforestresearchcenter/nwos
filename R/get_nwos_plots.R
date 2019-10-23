#' get_nwos_plots
#'
#' Extracts the full plot list for a given NWOS cycle/study, classed to STRATA
#'
#' @details
#' This function must be run on a machine with an ODBC connection to the USFS FIA production database through a user with read permissions.
#'
#' @param cycle is a string containing the NWOS cycle desired.
#' @param study is a string containing the NWOS study desired.
#'
#' @return an nwos.plots object
#'
#' @examples
#' get_nwos_plots(cycle='2018',study='base')
#'
#' @export

get_nwos_plots <- function(cycle = "2018",study='base'){
  
  if (study!='base'){
	  stop("get_nwos_plots() currently does not include functionality for studies other than 'base'")
  }
  
  #changing global settings
  options(stringsAsFactors = FALSE)
  options(scipen=999)
  
  #accessing DB and downloading records
  q <- "SELECT p.CN PLOT_OWNER_CN,
  p.LATITUDE,
  p.LONGITUDE,
  o.CN OWNER_CN,
  p.COND_STATUS_CD,
  CASE
  WHEN o.OWNCD_NWOS IS NOT NULL THEN o.OWNCD_NWOS
  ELSE p.OWNCD_NWOS
  END OWNCD_NWOS,
  p.STATECD_NWOS,
  o.INDUSTRIALCD_NWOS
  FROM FS_NWOS.PLOT_OWNER p
  LEFT JOIN FS_NWOS.OWNER o
  ON p.OWNER_CN = o.CN
  WHERE p.NWOS_CYCLE = '<CYTAG>'
  AND (p.ORIGIN = 'P2' OR p.ORIGIN_OTHER_REASON = 'Augmentation')"
  q <- gsub("<CYTAG>",cycle,q)
  PO <- sqlQuery64(q)
  
  PO$STATECD_NWOS <- as.character(PO$STATECD_NWOS) #statecd to character
  
  NS <- read.csv("T:/FS/RD/FIA/NWOS/DB/OFFLINE_TABLES/_REF_NONSAMPLED.csv") #import lookup table for nonsampled points
  NS <- NS[NS$NWOS_CYCLE==cycle,] #subset to this cycle
  
  #update OWNCD_NWOS and COND_STATUS_CD from NS
  IN.NS <- PO$PLOT_OWNER_CN %in% NS$CN #index of those with updates
  PO$COND_STATUS_CD[IN.NS] <- NS$COND_STATUS_CD_NWOS[match(PO$PLOT_OWNER_CN[IN.NS],NS$CN)]
  PO$OWNCD_NWOS[IN.NS] <- NS$OWNCD_NWOS[match(PO$PLOT_OWNER_CN[IN.NS],NS$CN)]
  
  OA <- read.csv("T:/FS/RD/FIA/NWOS/DB/OFFLINE_TABLES/_REF_OWNCD_ASSIGNED.csv") #import lookup table for randomly assigned owncds
  OA <- OA[OA$NWOS_CYCLE==cycle,] #subset to this cycle
  
  #update OWNCD_NWOS from OA
  IN.OA <- PO$PLOT_OWNER_CN %in% OA$CN #index of those with updates
  PO$OWNCD_NWOS[IN.OA] <- OA$OWNCD_ASSIGNED[match(PO$PLOT_OWNER_CN[IN.OA],OA$CN)]
  
  PO <- subset(PO,COND_STATUS_CD != '4') #drop census water from sample
  
  #determine strata
  #non-private, non-forested
  PR <- c('41','42','43','44','45') #private
  NP <- na.omit(unique(PO$OWNCD_NWOS)[!unique(PO$OWNCD_NWOS)%in%PR]) #nonprivate
  NF <- na.omit(unique(PO$COND_STATUS_CD)[!unique(PO$COND_STATUS_CD)=='1']) #nonforest
  PO$STRATA <- ifelse(PO$OWNCD_NWOS %in% NP | 
                        PO$COND_STATUS_CD %in% NF,"NPNF",NA)
  #large corporate
  PO$STRATA <- ifelse(PO$INDUSTRIALCD_NWOS=='1' & 
                        PO$COND_STATUS_CD=='1' &
                        is.na(PO$STRATA),"CORP_LARGE",PO$STRATA)
  # Other Corp
  # PO$STRATA <- ifelse(PO$STRATA == '41', "CORP_OTHER", PO$STRATA)
  #private, forested, other (by OWNCD_NWOS)
  PO$STRATA <- ifelse(PO$OWNCD_NWOS %in% PR &
                        PO$INDUSTRIALCD_NWOS %in% c('0',NA) &
                        PO$COND_STATUS_CD=='1'&
                        is.na(PO$STRATA), PO$OWNCD_NWOS,PO$STRATA)
  
  PO$OWNER_CN <- as.character(PO$OWNER_CN)
  NO <- which(is.na(PO$OWNER_CN)) #which have null owner cns?
  PO$OWNER_CN[NO] <- paste("UNKNOWN_OWNER",1:length(NO),sep='')
  
  #accessing DB and downloading records, SAMPLE AND RESPONSE
  #con <- odbcConnect("FIA01P")
  q <- "SELECT s.CN SAMPLE_CN,
  s.STATECD_NWOS,
  s.OWNER_CN,
  r.CN RESPONSE_CN,
  r.RESPONSE_CD,
  r.NONRESPONSE_REASON
  FROM FS_NWOS.SAMPLE s,
  FS_NWOS.RESPONSE r
  WHERE s.CN = r.SAMPLE_CN
  AND s.NWOS_CYCLE = '<CYTAG>'
  AND s.NWOS_STUDY = '<STTAG>'
  AND (r.NONRESPONSE_REASON <> '9' OR r.NONRESPONSE_REASON IS NULL)"
  q <- gsub("<CYTAG>",cycle,q)
  q <- gsub("<STTAG>",study,q)
  SR <- sqlQuery64(q)
  
  #combine RESPONSE_CD and NONRESPONSE_REASON in single column
  SR$RESPONSE <- ifelse(SR$RESPONSE_CD=='1','R',SR$NONRESPONSE_REASON)
  
  #aggregate SR by concatenated response code
  SRagg <- aggregate(RESPONSE~SAMPLE_CN,SR,FUN=function(x){paste(x,collapse=',')})
  names(SRagg)[2] <- 'RESPONSES'
  
  #function for determing if a sample is RESPONSE, NONRESPONSE, or EXCUSED NONRESPONSE
  coop.code <- function(x){
    if ('R' %in% x){
      y <- 'I'
    } else if (any(x %in% c(3,13))){
      y <- 'P' 
    } else if (any(x %in% c(2:5,7:8,14))){
      y <- 'NC'
	} else if (any(x %in% c(10,11))){
      y <- 'UN'
	} else if (any(x %in% c(1,6))){
      y <- 'R'
    }
    return(y)
  }
  #aggregate SR by coop.code
  SRagg2 <- aggregate(RESPONSE~SAMPLE_CN,SR,FUN=coop.code)
  names(SRagg2)[2] <- 'RESPONSE_CAT'
  
  SR2 <- unique(SR[,1:3]) #unique samples
  SR2 <- merge(SR2,SRagg,by='SAMPLE_CN') #merge on concatenated responses
  SR2 <- merge(SR2,SRagg2,by='SAMPLE_CN') #merge on aggregated response
  
  #create sample keys for joining
  PO$SKEY <- paste(PO$OWNER_CN,PO$STATECD_NWOS,sep="_")
  SR2$SKEY <- paste(SR2$OWNER_CN,SR2$STATECD_NWOS,sep="_")
  
  PO2 <- PO[PO$STRATA %in% 41:45,] #copy of PO
  PO2$SAMPLE_CN <- SR2$SAMPLE_CN[match(PO2$SKEY,SR2$SKEY)] #join SAMPLE_CN
  PO2$RESPONSES <- SR2$RESPONSES[match(PO2$SKEY,SR2$SKEY)] #join RESPONSES
  PO2$RESPONSE_CAT <- SR2$RESPONSE_CAT[match(PO2$SKEY,SR2$SKEY)] #join RESPONSE_CAT
  PO2$RESPONSES[is.na(PO2$RESPONSES)] <- '11' #if no RESPONSES, then 11
  PO2$RESPONSE_CAT[is.na(PO2$RESPONSE_CAT)] <- 'UN' #if no RESPONSE_CAT, then UN
  
  #full sample, one record per response
  fs <- unique(PO2[,c("SKEY","SAMPLE_CN","STATECD_NWOS","OWNCD_NWOS","STRATA",
                     "RESPONSES","RESPONSE_CAT")])
  R <- SR$RESPONSE_CD=='1' #add CN of valid response
  fs$RESPONSE_CN <- SR$RESPONSE_CN[R][match(fs$SAMPLE_CN,SR$SAMPLE_CN[R])]
  
  PO2 <- aggregate(PLOT_OWNER_CN~SAMPLE_CN,PO2,FUN='length') #aggregate PO2 by SAMPLE_CN
  fs$POINT_COUNT <- PO2$PLOT_OWNER_CN[match(fs$SAMPLE_CN,PO2$SAMPLE_CN)] #add point_count
  
  ls <- list(plots=PO,response_codes=fs)
  ls <- nwos_plots_object(ls)
  ls <- new("nwos.plots.object",plots=ls[[1]],response_codes=ls[[2]])
  
  return(ls)
  
}