#' get_nwos
#'
#' Extracts the data for a given NWOS cycle/study from the NWOS and returns it as an nwos.object, including metadata, weights, imputations, and plot-level data.
#'
#' @details
#' This function must be run on a machine with an ODBC connection to the USFS FIA production database through a user with read permissions.
#'
#' @param cycle is a string containing the NWOS cycle desired.
#' @param study is a string containing the NWOS study desired.
#' @param states is a character vector containing one or more states to extract.
#' @param questions is a character vector containing the names of one or more NWOS questions to extract.
#' @param response_cd is a numeric vector indicating which responses to include (1=response (default), 0=nonresponse, -2=invalid respons).
#'
#' @return a nwos.object
#'
#' @examples
#' get_nwos(cycle='2018',study='base',states='44',questions='AC_LAND')
#'
#' @export

get_nwos <- function(cycle='2018',study='base',states=NA,questions=NA,response_cd=1){

  #changing global settings
  options(stringsAsFactors = FALSE)
  options(scipen=999)

  #query for downloading QUEST table
  q <- "SELECT r.CN,
	  q.METADATA_CN,
    q.RESPONSE_VALUE
    FROM FS_NWOS.SAMPLE s, FS_NWOS.RESPONSE r, FS_NWOS.QUEST q, FS_NWOS.QUEST_METADATA m
    WHERE s.CN = r.SAMPLE_CN AND r.CN = q.RESPONSE_CN AND q.METADATA_CN = m.CN
    AND s.NWOS_CYCLE = '<CYTAG>'
    AND s.NWOS_STUDY = '<STTAG>'
    AND s.STATECD_NWOS IN ('<STAG>')
    AND m.QUESTION_NAME IN ('<QTAG>')
    AND r.RESPONSE_CD IN ('<RCTAG>')
    ORDER BY r.CN,
    CAST(SUBSTR(q.METADATA_CN,4) AS NUMBER)"
   #query for downloading SAMPLE table
  q2 <- "SELECT r.CN,
    s.STATECD_NWOS,
    s.NWOS_CYCLE,
    s.NWOSYR,
    o.OWNCD_NWOS,
    o.INDUSTRIALCD_NWOS
    FROM FS_NWOS.SAMPLE s, FS_NWOS.RESPONSE r, FS_NWOS.OWNER o
    WHERE s.CN = r.SAMPLE_CN
    AND o.CN = s.OWNER_CN
    AND s.NWOS_CYCLE = '<CYTAG>'
    AND s.NWOS_STUDY = '<STTAG>'
    AND s.STATECD_NWOS IN ('<STAG>')
    AND r.RESPONSE_CD  IN ('<RCTAG>')
    ORDER BY r.CN"

  #edit queries based on function parameters
  q <- gsub("<CYTAG>",cycle,q) #insert cycle
  q2 <- gsub("<CYTAG>",cycle,q2) #insert cycle
  if (study=='base intensified'){ #insert study
    q <- gsub("= '<STTAG>'","IN ('base','base intensified')", q)
    q2 <- gsub("= '<STTAG>'","IN ('base','base intensified')", q2)
  } else {
    q <- gsub("<STTAG>",study,q)
    q2 <- gsub("<STTAG>",study,q2)
  }
  if (!is.na(states[1])){ #insert states (if listed)
    q <- gsub("<STAG>",paste(states,collapse="','"),q)
	  q2 <- gsub("<STAG>",paste(states,collapse="','"),q2)
  } else {
    q <- gsub("IN ('<STAG>')","IS NOT NULL",q,fixed=T) #else, change to null filter
	  q2 <- gsub("IN ('<STAG>')","IS NOT NULL",q2,fixed=T) #else, change to null filter
  }
  if (!is.na(questions[1])){ #insert questions (if listed)
    q <- gsub("<QTAG>",paste(questions,collapse="','"),q)
  } else {
    q <- gsub("IN ('<QTAG>')","IS NOT NULL",q,fixed=T) #else, change to null filter
  }
  q <- gsub("<RCTAG>", paste(response_cd, collapse = "','"), q)
  q2 <- gsub("<RCTAG>", paste(response_cd, collapse = "','"), q2)

  quest <- sqlQuery64(q) #send query to database, quest

  if (nrow(quest)==0){
    stop('This combination of cycle/study/states/questions is invalid')
  }

  #add records for unasked questions
  rcn <- unique(quest$CN)
  qus <- unique(quest$METADATA_CN)
  una <- data.frame(CN=rep(rcn,each=length(qus)),
                    METADATA_CN=rep(qus,length(rcn)),
                    RESPONSE_VALUE='-3',
                    ASKED=NA) #dataframe for recording whether asked
  UK <- paste(una$CN,una$METADATA_CN) #UK, una
  UK2 <- paste(quest$CN,quest$METADATA_CN) #UK, quest
  una$ASKED <- UK %in% UK2
  una <- una[!una$ASKED,1:3]
  quest <- rbind(quest,una) #add unasked questions to quest
  quest <- quest[order(quest$CN,ncn(quest$METADATA_CN)),] #reorder

  sample <- sqlQuery64(q2) #send query to database, sample

  #get metadata
  q <- "SELECT CN,QUESTION_NAME,QUESTION_TEXT,
    DATA_TYPE,RANGE,UNITS_FACTORS,
    ITEM_TYPE,CHECK_GROUP,SKIP_ROOT,
    SKIP_TRIGGER
    FROM FS_NWOS.<T> WHERE <F> IN ('<I>')
    ORDER BY CAST(SUBSTR(CN,4) AS NUMBER)"
  metadata <- BATsqlQuery64(ids=quest$METADATA_CN,table='QUEST_METADATA',query=q)

  #get fields metadata
  q <- "SELECT f.FIELD_NAME,
      f.DESCRIPTION,
      f.DATA_TYPE,
      c.CODE||'='||c.MEANING UNITS_FACTORS
      FROM FS_NWOS.FIELDS f
      LEFT JOIN FS_NWOS.CODES c
      ON f.CN = c.FIELD_CN
      WHERE f.TABLE_NAME||f.FIELD_NAME IN
        ('RESPONSECN','SAMPLESTATECD_NWOS',
        'SAMPLENWOS_CYCLE','SAMPLENWOSYR',
        'OWNEROWNCD_NWOS',
        'OWNERINDUSTRIALCD_NWOS',
        'QUESTMETADATA_CN','QUESTRESPONSE_VALUE')"
  fields <- sqlQuery64(q)
  #reformat UNITS_FACTORS
  fields <- aggregate(UNITS_FACTORS~FIELD_NAME+DESCRIPTION+DATA_TYPE,
                                    fields,FUN=
                        function(x){ifelse(all(x!='='),paste(x,collapse=', '),NA)})
  #reorder
  so <- match(c(names(sample),names(quest)[2:3]),fields$FIELD_NAME)
  fields <- fields[so,]

  #get weights
  weights <- read.csv("T:/FS/RD/FIA/NWOS/DB/OFFLINE_TABLES/_REF_WEIGHTS.csv")
  weights <- weights[weights$NWOS_STUDY==study,]
  weights <- weights[weights$RESPONSE_CN %in% sample$CN
                       ,c('RESPONSE_CN','FINAL_WEIGHTS','PLOT_COUNT')]

  #get imputations
  imps <- read.csv("T:/FS/RD/FIA/NWOS/DB/OFFLINE_TABLES/_REF_QUEST_IMPUTED.csv")
  imps <- imps[imps$RESPONSE_CN %in% sample$CN
                       ,c('RESPONSE_CN','RESPONSE_VALUE','IMPUTATION','METADATA_CN')]

  ls <- list(quest,sample,metadata,fields,weights,imps)
  ls <- new("nwos.object",quest=ls[[1]],
	sample=ls[[2]],
	metadata=ls[[3]],
	fields=ls[[4]],
	weights=ls[[5]],
	imputations=ls[[6]])

  return(ls)

}
