#' nwos_long
#'
#' Formats NWOS data into a long format for analysis
#'
#' @param x is a nwos.object
#' @param imputations is string containing which imputation set is to be used in place of non-response, if any. Valid responses are "none","random","1","2","3","4","5"
#'
#' @return a data.frame
#'
#' @examples
#' nwos_long(foo,imputations="1")
#'
#' @export

nwos_long <- function(x, imputations="none"){

  if(!is(x,'nwos.object')){
    stop("nwos_long() requires nwos.object as input")
  }

  if(!imputations %in% c('none','random',1:5)){
    stop("'imputations' only excepts values of 'none','random', or integer")
  }

  st <- x@quest #extract quest
  if (imputations!='none'){
    if (imputations=='random'){ #insert imputations, if wanted
      rep <- sample(x@imputations$IMPUTATION,1) #choose random imputation
    } else {
      rep <- imputations #choose selected imputation
    }
    imp <- x@imputations[x@imputations$IMPUTATION==rep,] #subset imputation
    st.UK <- paste(st$CN,st$METADATA_CN,sep='_') #create keys
    imp.UK <- paste(imp$RESPONSE_CN,imp$METADATA_CN,sep='_')
    TR <- st.UK %in% imp.UK #to replace, those records with imputed values
    st$RESPONSE_VALUE[TR] <- imp$RESPONSE_VALUE[match(st.UK[TR],imp.UK)] #insert value
  }
  l <- merge(x@sample,st,by="CN")
  #reformat l
  for (i in 1:length(l)){ #cast columns as correct datatype
    t <- x@fields$DATA_TYPE[match(names(l)[i],x@fields$FIELD_NAME)]
    if (t=='CHAR255'){
      l[,i] <- as.character(l[,i])
    } else if (t=='INT'){
      l[,i] <- as.integer(l[,i])
    }
  }
  l$PLOT_COUNT <- x@weights$PLOT_COUNT[match(l$CN,x@weights$RESPONSE_CN)]
  l$FINAL_WEIGHT <- x@weights$FINAL_WEIGHT[match(l$CN,x@weights$RESPONSE_CN)]
  return(l)
}
