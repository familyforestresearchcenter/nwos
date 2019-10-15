#' nwos_wide
#'
#' Formats NWOS data into a wide format for analysis
#'
#' @param x is a nwos.object
#'
#' @return a data.frame
#'
#' @examples
#' nwos_wide(foo)
#'
#' @export

nwos_wide <- function(x,imputations='none'){
  
  if(!is(x,'nwos.object')){
    stop("nwos_wide_raw() requires nwos.object as input")
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
  st <- unstack(st[,c('RESPONSE_VALUE','METADATA_CN')]) #unstack questions
  st <- st[,order(ncn(names(st)))] #reorder
  names(st) <- x@metadata$QUESTION_NAME[match(names(st),x@metadata$CN)]
  #format st
  for (i in 1:length(st)){ #cast columns as correct datatype
    st[,i] <- as.character(st[,i])
  }
  
  s <- x@sample #sample columns
  #format s
  for (i in 1:length(s)){ #cast columns as correct datatype
    t <- x@fields$DATA_TYPE[match(names(s)[i],x@fields$FIELD_NAME)]
    if (t=='CHAR255'){
      s[,i] <- as.character(s[,i])
    } else if (t=='INT'){
      s[,i] <- as.integer(s[,i])
    }
  }
  s$PLOT_COUNT <- x@weights$PLOT_COUNT[match(s$CN,x@weights$RESPONSE_CN)]
  s$FINAL_WEIGHT <- x@weights$FINAL_WEIGHT[match(s$CN,x@weights$RESPONSE_CN)]
  
  wide <- cbind(s,st)	
  return(wide)
}
