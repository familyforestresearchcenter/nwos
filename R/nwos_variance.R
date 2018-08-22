#' NWOS Variance
#'
#' This function calculates variances for NWOS statistics using a bootstrapping approach.
#' @usage nwosVar(point.count, domain, y=1, area, response=1,
#' stratum.area, units="ownerships", stat="total", R=100)
#' @param data data frame of survey data. See details below for required variables.
#' @param units units of analysis. Permissable values are "ownerships" or "area". Default is "ownerships".
#' @param stat statistic of interest. Permissable values are "total", "mean" or "proportion". Default is "total".
#' @param R number of replicates or bootstraps. Default is 100.
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @keywords nwos
#' @export
#' @examples
#' x <- nwos_variances(df=sample.i.j, state.list=55, state.area=LAND_AREA, R=10)
#' x$VARIANCES

nwos_variance <- function(state.area,
                          point.count, response, owner.area,
                          stratum = 1, domain = 1, variable = 1,
                          replicates, index,
                          stat = "TOTAL", units = "OWNERSHIPS") {
  var(sapply(1:NCOL(replicates), nwos_estimate_replicates,
             state.area = state.area,
             point.count = point.count, response = response, owner.area = owner.area,
             stratum = stratum, domain = domain,
             replicates = replicates, index = index,
             stat = stat, units = units))
}


# nwos_variance <- function(df,
#                           state.list,
#                           state.area,
#                           stratum.area=NA,
#                           land.use.list=1,
#                           own.cd.list=45,
#                           domain.list=NA,
#                           variable.list=NA,
#                           replicates = NA, # Use a pre-defined data frame of replicates
#                           R=10) { # Number of replicates to use
#   #### General Set up ####
#   # df$OWN_CD[is.na(df$OWN_CD)] <- -1
#   # df$OWN_ID <- as.character(df$OWN_ID)
#   # df$OWN_ID[is.na(df$OWN_ID)] <- -1
#   if(is.na(domain.list)) {
#     df$ONE_PLUS <- 1
#     domain.list <- "ONE_PLUS"
#   }
#   if(is.na(variable.list)) {
#     df$NONE <- 1
#     variable.list <- "NONE"
#   }
#
#   #### Empty data frames ####
#   VARIANCES <- data.frame(STATE_CD = numeric(0),
#                           LAND_USE = numeric(0),
#                           OWN_CD = numeric(0),
#                           DOMAIN = character(0),
#                           VARIABLE = character(0),
#                           VARIANCE = numeric(0),
#                           UNITS = character(0),
#                           STAT = character(0),
#                           REPLICATES = numeric(0),
#                           RUN_DATE_VARIANCE = character(0))
#
#   #### Append replicates ####
#   # Drop POINT_COUNT from df
#   replicates <- replicates[,!names(replicates) %in% c("RUN_DATE")]
#   df <- df[ , !names(df) %in% c("POINT_COUNT")]
#   # merge
#   df2 <- merge(replicates, df)
#
#   #### Estimates ####
#   for(r in 1:R) {
#     df.r <- df2[df2$REPLICATE == r,]
#     if(r == 1) {
#       ESTIMATES_R <- data.frame(nwos_estimates(df.r,
#                                                state.list=55,
#                                                state.area=LAND_AREA,
#                                                stratum.area=NA,
#                                                land.use.list=1,
#                                                own.cd.list=45,
#                                                domain.list=domain.list,
#                                                variable.list=NA,
#                                                stat.list=c("TOTAL"))$ESTIMATES,
#                                 REPLICATE = r)
#     }
#     else {
#       ESTIMATES_R <- rbind(ESTIMATES_R,
#                            data.frame(nwos_estimates(df.r,
#                                                      state.list=55,
#                                                      state.area=LAND_AREA,
#                                                      stratum.area=NA,
#                                                      land.use.list=1,
#                                                      own.cd.list=45,
#                                                      domain.list=domain.list,
#                                                      variable.list=NA,
#                                                      stat.list=c("TOTAL"))$ESTIMATES,
#                                       REPLICATE = r))
#     }
#   }
#
#   #### Variances ####
#   for(s in unique(ESTIMATES_R$STATE_CD)) {
#     for(l in unique(ESTIMATES_R$LAND_USE)) {
#       for(o in unique(ESTIMATES_R$OWN_CD)) {
#         for(d in unique(ESTIMATES_R$DOMAIN)) {
#           for(v in unique(ESTIMATES_R$VARIABLE)) {
#             for(u in unique(ESTIMATES_R$UNITS)) {
#               for(st in unique(ESTIMATES_R$STAT)) {
#                 VAR_EST <- var(ESTIMATES_R$ESTIMATE[ESTIMATES_R$STATE_CD == s &
#                                                       ESTIMATES_R$LAND_USE == l &
#                                                       ESTIMATES_R$OWN_CD == o &
#                                                       ESTIMATES_R$DOMAIN == d &
#                                                       ESTIMATES_R$VARIABLE == v &
#                                                       ESTIMATES_R$UNITS == u &
#                                                       ESTIMATES_R$STAT == st])
#                 VARIANCES <- rbind(VARIANCES,
#                                    data.frame(STATE_CD = s,
#                                               LAND_USE = l,
#                                               OWN_CD = o,
#                                               DOMAIN = d,
#                                               VARIABLE = v,
#                                               VARIANCE = VAR_EST,
#                                               UNITS = u,
#                                               STAT = st,
#                                               REPLICATES = NROW(R),
#                                               RUN_DATE_VARIANCE = Sys.time()))
#               }
#             }
#           }
#         }
#       }
#     }
#   }
#   return(list(ESTIMATES = ESTIMATES_R, VARIANCES = VARIANCES))
# }
