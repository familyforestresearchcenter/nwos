#' NWOS Data
#'
#' This function is used to take a dataset generated from the empirical sampling dataset and convert it to the nwos.data format.
#' @param sample.data Data frame typically generated from the empirical sampling analysis.
#' @return
#' Data frame with the following variables:
#'     id
#'     stratum
#'     owner.class
#'     point.count
#'     area
#'     response
#'     y_1
#'     y_2
#'     y_3
#' @details
#' sample.data needs to have the following variables:
#'     l    Land cover
#'     st    Stratum
#'     i    ID
#'     o    Owner code
#'     rp    Response propensity
#'     a    Area
#'     y_1    y_1
#'     y_2    y_2
#'     y_3    y_3
#' @keywords nwos
#' @export
#' @examples
#' load("DATA/SAMPLE/SAMPLE_0001.RData")
#' sample.data <- df.s
#' rm(df.s)
#' sample.data$st <- "WI"
#' nwos.data <- nwosData(sample.data)
#' save(nwos.data,file="ANALYSIS/R/nwos/data/nwos_data_20180107.RData")

nwosData <- function(sample.data, long.form=F)
{
  # OWNER.MERGED$a[round(OWNER.MERGED$a,0)<1] <- 0
  # sample.data$a[round(sample.data$a,0)<1] <- 0
  sample.data$a[sample.data$a<1] <- 0
  # sample.data$a <- round(sample.data$a,0)
  sample.data$i <- as.character(sample.data$i)
  sample.data$i[sample.data$l==0] <- -9 # Assign -9 to i for non-forest points
  sample.data$i[sample.data$a<1] <- -9
  # sample.data$i[round(sample.data$a,0)<1] <- -9
  sample.data$i[sample.data$o==4] <- -4
  sample.data$i[sample.data$o==5] <- -5
  sample.data$i[sample.data$o==6] <- -6
  sample.data$i[sample.data$o==7] <- -7
  sample.data$o <- as.character(sample.data$o)
  # sample.data$o <- ifelse(sample.data$a<1, NA, sample.data$o)
  # sample.data$o <- ifelse(round(sample.data$a,0)<1, 9, sample.data$o)
  sample.data$o <- ifelse(sample.data$a<1, 9, sample.data$o)
  # sample.data$o <- ifelse(is.na(sample.data$o), 9, sample.data$o)
  sample.data$o <- ifelse(sample.data$l==0, 9, sample.data$o)
  sample.data$rp <- ifelse(is.na(sample.data$rp), 1, sample.data$rp)
  sample.data$response <- ifelse(sample.data$rp>0.5, 1, 0)
  # sample.data$response <- 1
  # summary(sample.data)

  if(long.form)
  {
    nwos.data <- data.frame(id=sample.data$i,
                            stratum=rep("WI", NROW(sample.data)),
                            owner.class=sample.data$o,
                            point.count=rep(1, NROW(sample.data)),
                            area=sample.data$a,
                            response=sample.data$response,
                            y_1=sample.data$y_1,
                            y_2=sample.data$y_2,
                            y_3=sample.data$y_3)

    # Recode owner.class
    nwos.data$owner.class <- as.character(nwos.data$owner.class)
    owner.class.table <- data.frame(OWNCD=c(1,2,3,4,5,6,7,0,9),
                               OWNCD.NAME=c("FamilyForest", "CorporateForest", "OtherPrivateForest",
                                            "TribalForest",
                                            "FederalForest", "StateForest", "LocalForest",
                                            "UnknownForest", "Nonforest"))
    nwos.data$owner.class<- as.character(owner.class.table$OWNCD.NAME[match(nwos.data$owner.class,owner.class.table$OWNCD)])
    nwos.data$owner.class <- factor(nwos.data$owner.class, levels=c("FamilyForest", "CorporateForest", "OtherPrivateForest",
                                                          "TribalForest",
                                                          "FederalForest", "StateForest", "LocalForest",
                                                          "UnknownForest", "Nonforest"))

    # Recode Non-private Forest
    private.forest <- c("FamilyForest", "CorporateForest", "OtherPrivateForest")
    nwos.data$response <- factor(ifelse(nwos.data$owner.class%in%private.forest,
                                        nwos.data$response, NA))
    nwos.data$response <- factor(nwos.data$response)

    nwos.data$area <- ifelse(nwos.data$owner.class%in%private.forest & nwos.data$response==1,
                             nwos.data$area, NA)

    # Auxilarry Variables
    nwos.data$y_1 <- factor(ifelse(nwos.data$owner.class%in%private.forest & nwos.data$response==1,
                                   nwos.data$y_1, NA))
    nwos.data$y_2 <- factor(ifelse(nwos.data$owner.class%in%private.forest & nwos.data$response==1,
                                   nwos.data$y_2, NA))
    nwos.data$y_3 <- round(ifelse(nwos.data$owner.class=="FamilyForest" & nwos.data$response==1,
                                  nwos.data$y_3, NA),0)
  }
  else
  {
    # ID and Point Count
    id.pc <- data.frame(table(as.character(sample.data$i)))
    names(id.pc) <- c("id", "point.count")

    # Build nwos.data
    nwos.data <- data.frame(id=id.pc$id,
                            stratum=rep(NA, NROW(id.pc$id)),
                            owner.class=rep(NA, NROW(id.pc$id)),
                            point.count=id.pc$point.count,
                            area=rep(NA, NROW(id.pc$id)),
                            response=rep(NA, NROW(id.pc$id)),
                            y_1=rep(NA, NROW(id.pc$id)),
                            y_2=rep(NA, NROW(id.pc$id)),
                            y_3=rep(NA, NROW(id.pc$id)))
    # Stratum
    nwos.data$stratum <- as.factor(sample.data$st[match(nwos.data$id, sample.data$i)])

    # owner.class
    sample.data$d <- as.character(sample.data$o)
    owner.class.table <- data.frame(OWNCD=c(1,2,3,4,5,6,7,0,9),
                               OWNCD.NAME=c("FamilyForest", "CorporateForest", "OtherPrivateForest",
                                            "TribalForest",
                                            "FederalForest", "StateForest", "LocalForest",
                                            "UnknownForest", "Nonforest"))
    sample.data$d <- as.character(owner.class.table$OWNCD.NAME[match(sample.data$d,owner.class.table$OWNCD)])
    sample.data$d <- factor(sample.data$d, levels=c("FamilyForest", "CorporateForest", "OtherPrivateForest",
                                                    "TribalForest",
                                                    "FederalForest", "StateForest", "LocalForest",
                                                    "UnknownForest", "Nonforest"))
    nwos.data$owner.class <- as.factor(sample.data$d[match(nwos.data$id, sample.data$i)])

    # Reponse
    private.forest <- c("FamilyForest", "CorporateForest", "OtherPrivateForest")
    nwos.data$response <- factor(ifelse(nwos.data$owner.class%in%private.forest,
                                        sample.data$response[match(nwos.data$id, sample.data$i)], NA))
    nwos.data$response <- factor(nwos.data$response)

    # Area
    # nwos.data$area <- round(ifelse(nwos.data$owner.class%in%private.forest & nwos.data$response==1,
    #                                sample.data$a[match(nwos.data$id, sample.data$i)], NA),0)
    nwos.data$area <- ifelse(nwos.data$owner.class%in%private.forest & nwos.data$response==1,
                             sample.data$a[match(nwos.data$id, sample.data$i)], NA)

    # Auxilarry Variables
    nwos.data$y_1 <- factor(ifelse(nwos.data$owner.class%in%private.forest & nwos.data$response==1,
                                   sample.data$y_1[match(nwos.data$id, sample.data$i)], NA))
    nwos.data$y_2 <- factor(ifelse(nwos.data$owner.class%in%private.forest & nwos.data$response==1,
                                   sample.data$y_2[match(nwos.data$id, sample.data$i)], NA))
    nwos.data$y_3 <- round(ifelse(nwos.data$owner.class=="FamilyForest" & nwos.data$response==1,
                                  sample.data$y_3[match(nwos.data$id, sample.data$i)], NA),0)
  }

  # Return nwos.data
  # summary(nwos.data)
  return(nwos.data)
}
