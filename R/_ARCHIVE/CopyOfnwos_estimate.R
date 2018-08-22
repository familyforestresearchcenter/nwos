#' NWOS State LandUse Ownership Domain Variable Wrapepr function
#'
#' This function estimates the stratum area.
#' @usage nwos_stratum_area(stratum, point.count, state.area)
#' @param df data frame containing, at a minimum, the following variables: STATE_CD, LAND_USE, OWN_CD, and POINT_COUNT
#' @param state.list vector of state codes to be evaluated.
#' @param state.area vector of total land area in state. It needs to have the same length as state.list and be in the
#' same order.
#' @param land.use.list vector of land uses to be evaluated. The defaualt value is 1 (forest).
#' @param own.cd.list vector of ownership codes to be evaluated. The default value if 45 (family).
#' @details
#' @return data frame of areas in state/land use/ownership strata
#' @keywords nwos
#' @export
#' @references
#' Butler, B.J. In review. Weighting for the US Forest Service, National Woodland Owner Survey. U.S. Department of Agriculture, Forest Service, Northern Research Station. Newotwn Square, PA.
#' @examples
#' load("data/nwos_data_sample.RData")
#' data <- NWOS_DATA_SAMPLE[NWOS_DATA_SAMPLE$SAMPLE==1,]
#' nwos_stratum_area()
#'
#' df=sample.i.j
#' state.list=55
#' state.area=LAND_AREA
#' x <- nwos_estimates(df=sample.i.j, state.list=55, state.area=LAND_AREA)
#' x

nwos_estimates <- function(df,
                           state.list,
                           state.area,
                           stratum.area=NA,
                           land.use.list=1,
                           own.cd.list=45,
                           domain.list=NA,
                           variable.list=NA,
                           stat.list=c("TOTAL")) {  # Generic or by variable
  #### Data Checks ####
  if(any(!c("STATE_CD", "LAND_USE", "OWN_CD", "POINT_COUNT") %in% names(df)))
    stop("Variables missing from df")
  if(length(state.list) != length(state.area))
    stop("Lengths of state.list and state.area do not match")

  #### Set defaults for domain and variable ####
  if(is.na(domain.list)) {
    df$ONE_PLUS <- 1
    domain.list <- "ONE_PLUS"
  }
  if(is.na(variable.list)) {
    df$NONE <- 1
    variable.list <- "NONE"
  }

  #### Empty data frames for data capture ####
  STRATUM_AREAS <- data.frame(STATE_CD=numeric(0),
                              LAND_USE=numeric(0),
                              OWN_CD=numeric(0),
                              AREA=numeric(0),
                              UNITS=character(0),
                              POINTS_STATE=numeric(0),
                              POINTS_STRATUM=numeric(0),
                              RUN_DATE=numeric(0))
  RESPONSE_RATES <- data.frame(STATE_CD=numeric(0),
                               LAND_USE=numeric(0),
                               OWN_CD=numeric(0),
                               RESPONSE_RATES=numeric(0),
                               POINTS_STRATUM=numeric(0),
                               RUN_DATE=character(0))
  WEIGHTS <- data.frame(STATE_CD = numeric(0),
                        LAND_USE = numeric(0),
                        OWN_CD = numeric(0),
                        OWN_ID = numeric(0),
                        WEIGHT = numeric(0),
                        RUN_DATE = character(0))
  ESTIMATES <- data.frame(STATE_CD = numeric(0),
                          LAND_USE = numeric(0),
                          OWN_CD = numeric(0),
                          DOMAIN = character(0),
                          Y = character(0),
                          ESTIMATE = numeric(0),
                          UNITS = character(0),
                          STAT = character(0),
                          RESPONDENTS_STRATUM = numeric(0),
                          RESPONDENTS_DOMAIN = numeric(0),
                          POINTS_STRATUM = numeric(0),
                          POINTS_DOMAIN = numeric(0),
                          RUN_DATE = character(0))

  #### Estimates ####
  for(s in state.list) { # By state
    df.s <- df[df$STATE_CD %in% s,] # Subset for state
    for(l in land.use.list) { # By land use
      df.s.l <- df.s[df.s$LAND_USE %in% l,] # Subset for land use
      for(o in own.cd.list) { # By ownership group
        df.s$STRATUM <- ifelse(df.s$STATE_CD==s & df.s$LAND_USE==l & df.s$OWN_CD==o, 1, 0)
        df.s.l.o <- df.s.l[df.s.l$OWN_CD %in% o,] # Subset for owner group
        a.s.l.o <- nwos_stratum_area(state.area=state.area, # Stratum area
                                     stratum=df.s$STRATUM,
                                     point.count=df.s$POINT_COUNT)
        STRATUM_AREAS <- rbind(STRATUM_AREAS, # Append stratum areas
                               data.frame(STATE_CD = s,
                                          LAND_USE = l,
                                          OWN_CD = o,
                                          AREA = a.s.l.o,
                                          UNITS = "ACRES",
                                          POINTS_STATE = sum(df.s$POINT_COUNT),
                                          POINTS_STRATUM = sum(df.s.l.o$POINT_COUNT),
                                          RUN_DATE = Sys.Date()))
        rr.s.l.o <- nwos_response_rate(point.count=df.s.l.o$POINT_COUNT, # Response rates
                                       response=df.s.l.o$RESPONSE)
        RESPONSE_RATES <- rbind(RESPONSE_RATES,
                                data.frame(STATE_CD = s, # Append response rates
                                           LAND_USE = l,
                                           OWN_CD = o,
                                           RESPONSE_RATES = rr.s.l.o,
                                           POINTS_STRATUM = sum(df.s.l.o$POINT_COUNT),
                                           RUN_DATE = Sys.Date()))
        df.s.l.o$WEIGHT <- nwos_weights(stratum.area=a.s.l.o, # Weights
                                        response.rate=rr.s.l.o,
                                        point.count=df.s.l.o$POINT_COUNT,
                                        owner.area=df.s.l.o$ACRES_FOREST)
        WEIGHTS <- rbind(WEIGHTS,
                         data.frame(STATE_CD = df.s.l.o$STATE_CD, # Append weights
                              LAND_USE = df.s.l.o$LAND_USE,
                              OWN_CD = df.s.l.o$OWN_CD,
                              OWN_ID = df.s.l.o$OWN_ID,
                              WEIGHT = df.s.l.o$WEIGHT,
                              RUN_DATE = Sys.Date()))
        # if(is.na(domain.list)) {
        #   df.s.l.o$DOMAIN <- -1
        #   domain.list <- -1
        # }
        for(d in domain.list) {
          # convert domain to dummy
          domain <- as.numeric(eval(parse(text=paste0("df.s.l.o$",d))))
          # if(is.na(variable.list)){
          #   df.s.l.o$VARIABLE <- 1
          #   variable.list <- 1
          # }
          for(v in variable.list) { # By variable
            variable <- as.numeric(eval(parse(text=paste0("df.s.l.o$", v))))
            if("TOTAL" %in% stat.list) { # Totals
              own.tot.s.l.o.d <- nwos_total(weight=df.s.l.o$WEIGHT,
                                            area=df.s.l.o$ACRES_FOREST,
                                            domain = domain,
                                            variable = variable,
                                            units="OWNERSHIPS")
              ac.tot.s.l.o.d <- nwos_total(weight=df.s.l.o$WEIGHT,
                                           area=df.s.l.o$ACRES_FOREST,
                                           domain = domain,
                                           variable = variable,
                                           units="AREA")
              ESTIMATES <- rbind(ESTIMATES,
                                 data.frame(STATE_CD = rep(s, 2),
                                            LAND_USE = rep(l, 2),
                                            OWN_CD = rep(o, 2),
                                            DOMAIN = rep(d, 2),
                                            VARIABLE = rep(v, 2),
                                            ESTIMATE = c(own.tot.s.l.o.d,
                                                         ac.tot.s.l.o.d),
                                            UNITS = c("OWNERSHIPS", "ACRES"),
                                            STAT = rep("TOTAL", 2),
                                            RESPONDENTS_STRATUM = rep(NROW(unique(df.s.l.o$OWN_ID[df.s.l.o$RESPONSE %in% 1])), 2),
                                            RESPONDENTS_DOMAIN = rep(NROW(unique(df.s.l.o$OWN_ID[df.s.l.o$RESPONSE %in% 1 &
                                                                                                   domain %in% 1])), 2),
                                            POINTS_STRATUM = rep(sum(df.s.l.o$POINT_COUNT[df.s.l.o$RESPONSE %in% 1]), 2),
                                            POINTS_DOMAIN = rep(sum(df.s.l.o$POINT_COUNT[df.s.l.o$RESPONSE %in% 1 &
                                                                                           domain %in% 1]), 2),
                                            RUN_DATE = Sys.Date()))
            }
            # Means {}
            # Proportions
            # Quantile
          }
        }
      }
    }
  }
  return(list(STRATUM_AREAS = STRATUM_AREAS,
              RESPONSE_RATES = RESPONSE_RATES,
              WEIGHTS = WEIGHTS,
              ESTIMATES = ESTIMATES))
}
