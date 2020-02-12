#' nwos_plots_metadata
#'
#' Creates metadata table for NWOS plots data
#'
#' @param x is a nwos.plots.object
#' 
#' @return a data.frame
#'
#' @examples
#' nwos_plots_metadata(foo)
#'
#' @export

nwos_plots_metadata <- function(x){
  
  if(!is(x,'nwos.plots.object')){
    stop("'plots' requires nwos.plots.object as input")
  }
  
  pc <- nwos_plots_complete(x)
  
  md <- data.frame(COLUMN=names(pc),
                   DESCRIPTION=c('PLOT sequence number',
                                 'Actual latitude, NAD83 (ESPG:4269)',
                                 'Actual longitude, NAD83 (ESPG:4269)',
                                 'OWNER sequence number',
                                 'Condition status code',
                                 'Owner code, NWOS adjusted',
                                 'State code, NWOS adjusted',
                                 'Industrial private forest owner code, NWOS adjusted',
                                 'NWOS statistical strata',
                                 'SAMPLE sequence number',
                                 'RESPONSE sequence number',
                                 'Response category (equivalent to AAPOR COOP1)'),
                   DATA_TYPE=c('VARCHAR2(255)',
                               'BINARY_DOUBLE(8)',
                               'BINARY_DOUBLE(8)',
                               'VARCHAR2(255)',
                               'NUMBER(22)',
                               'NUMBER(22)',
                               'VARCHAR2(255)',
                               'NUMBER(22)',
                               'VARCHAR2(255)',
                               'VARCHAR2(255)',
                               'VARCHAR2(255)',
                               'VARCHAR2(255)'),
                   UNITS_FACTORS=c(NA,'decimal degrees','decimal degrees',
                                   NA,
                                   '1=Forest, 2=Non-forest, 3=Water(census)',
                                   '0=Undifferentiated public, 11=National Forest System, 12=National Grassland, 13=Other Forest Service, 21=National Park Service, 22=Bureau of Land Management, 23=Fish and Wildlife Service, 24=Department of Defense/Energy, 25=Other federal, 31=State, 32=Local (County, Municipal, etc.), 33=Other non-federal public, 41=Corporate, 42=Non-governmental conservation/natural resources organization, 43=Unincorporated local partnership/association/club, 44=Native American (Indian), 45=Individual, family',
                                   NA,
                                   '0=non-industrial or small corporate, 1=large corporate',
                                   '41=Corporate, 42=Non-governmental conservation/natural resources organization, 43=Unincorporated local partnership/association/club, 44=Native American (Indian), 45=Individual, family, CORP_LARGE=Large corporate, NPNF=Non-private and/or non-forest',
                                   NA,NA,
                                   'I=Response, P=Partial response, NC=Not contacted, R=Refused, UN=Unknown'),
                   ITEM_TYPE=NA,
                   CHECK_GROUP=NA)
  
  return(md)
}
