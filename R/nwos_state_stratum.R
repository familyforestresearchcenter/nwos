#' NWOS State Stratum
#'
#' This function provides a means to iterate nwos functions by state and stratum.
#' @usage nwosResponseRate(point.count, response)
#' @param data data frame of survey data. See details below for required variables.
#' @details
#' data must have the following variables:
#' STATE state;
#' STRATUM list of strata;
#' POINT_COUNT number of sample points per record. Needs to sum to total number of sample points across all land uses and ownership classes.
#' @return Dataframe with response rate by state and stratum
#' @keywords nwos
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data_sample.RData")
#' data <- NWOS_DATA_SAMPLE[NWOS_DATA_SAMPLE$SAMPLE==1,]
#' nwosResponseRate(data)

# nwosStateStratum <- function(data)
# {
#   rr <- data.frame(STATE=numeric(0),
#                    STRATUM=numeric(0),
#                    RESPONSE_RATE=numeric(0)) # Create empty data frame
#   state <- unique(data$STATE) # Vector of unique states in data
#   for(i in state)
#   {
#     stratum <- unique(data$STRATUM[data$STATE%in%i])  # Vector of unique strata in state i
#     for(j in stratum)
#     {
#       data.i.j <- data[data$STATE%in%i & data$STRATUM%in%j, ] # Data for stratum j in state i
#       rr.i.j <- sum(data.i.j$POINT_COUNT[data.i.j$RESPONSE%in%c(1)],na.rm=T) /
#         sum(data.i.j$POINT_COUNT[data.i.j$RESPONSE%in%c(1,0)],na.rm=T) # Response rate for stratum j in state i
#       rr <- rbind(rr,
#                   data.frame(STATE=i,
#                              STRATUM=j,
#                              RESPONSE_RATE=rr.i.j)) # Append response rate data to data frame
#     }
#   }
#   return(rr) # Return data from of response rates by state and stratum
# }
