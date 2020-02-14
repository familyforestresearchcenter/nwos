#' nwos_full_metadata
#'
#' Creates metadata table for full (i.e. all plots and responses) NWOS dataset
#'
#' @param x is a nwos.object
#' 
#' @return a data.frame
#'
#' @examples
#' nwos_full_metadata(foo)
#'
#' @export

nwos_full_metadata <- function(x){
  
  if(!is(x,'nwos.object')){
    stop("'x' requires nwos.object as input")
  }
  
  #metadata from plots
  p <- data.frame(COLUMN=c('STATECD_NWOS','COND_STATUS_CD','OWNCD_NWOS'),
                   DESCRIPTION=c('State code, NWOS adjusted',
                                 'Condition status code',
								 'Owner code, NWOS adjusted'),
                   DATA_TYPE=c('VARCHAR2(255)','NUMBER(22)','NUMBER(22)'),
                   UNITS_FACTORS=c(NA,
									'-1=Unknown, 1=Forest, 2=Non-forest, 3=Water(census)',
                                   '-1=Unknown, 0=Undifferentiated public, 11=National Forest System, 12=National Grassland, 13=Other Forest Service, 21=National Park Service, 22=Bureau of Land Management, 23=Fish and Wildlife Service, 24=Department of Defense/Energy, 25=Other federal, 31=State, 32=Local (County, Municipal, etc.), 33=Other non-federal public, 41=Corporate, 42=Non-governmental conservation/natural resources organization, 43=Unincorporated local partnership/association/club, 44=Native American (Indian), 45=Individual, family'),
                   ITEM_TYPE=NA,
                   CHECK_GROUP=NA)
  
  #metadata from quest
  q <- nwos_wide_metadata(x)
  
  pq <- rbind(p,q[!q$COLUMN %in% c('STATECD_NWOS','OWNCD_NWOS'),])
  
  return(pq)
}
