#' Merge UDB-Data
#'
#' Merges the data from the list object into one big data.table.
#' Also automatically renames some variables
#'
#' @param dat The list element created from readUDB().
#' @param merge.by Default values are c("B010","B020","B030"). No change recommended! Changes will probably crash the function.
#' @param prettyNames Default = TRUE. No change recommended! Renames the varaibles to "SurveyYear", "Country", "PID", "HHID". These names will be used by later functions!
#'
#' @return
#' @export
#'
#' @examples
#' # Standard call:
#' SILC_UDB_M <- mergeUDB(copy(SILC_UDB_2016_3))
#' # copy() is not mandatory

mergeUDB <- function(dat,merge.by=c("B010","B020","B030"),
                     prettyNames=TRUE,keep.all=TRUE,
                     keep.D=TRUE,keep.H=TRUE,
                     keep.R=TRUE,keep.P=TRUE){


  if(keep.all){
    keep.D <- keep.H <- keep.R <- keep.P <- TRUE
  }

  DHFiles <- doMerge(dat$D.Files,dat$H.Files,merge.by,keep.x=keep.D,keep.y=keep.H)
  RPFiles <- doMerge(dat$R.Files,dat$P.Files,merge.by,keep.x=keep.R,keep.y=keep.P)

  keep.x <- any(c(keep.R,keep.P))
  keep.y <- any(c(keep.D,keep.H))

  if(prettyNames){
    DHNames <- getNames(colnames(DHFiles),merge.by)
    RPNames <- getNames(colnames(RPFiles),c(merge.by,"B040"))
    setnames(DHFiles,DHNames,c("SurveyYear","Country","HHID"))
    setnames(RPFiles,RPNames,c("SurveyYear","Country","PID","HHID"))

    allmerge.by <- c("SurveyYear","Country","HHID")
    allFiles <- merge(DHFiles,RPFiles,by=allmerge.by,keep.x=keep.x,keep.y=keep.y)
  }else{
    by.x <- getNames(colnames(DHFiles),c("B010","B020","B030"))
    by.y <- getNames(colnames(RPFiles),c("B010","B020","B040"))
    allFiles <- merge(DHFiles,RPFiles,by.x=by.x,by.y=by.y,keep.x=keep.x,keep.y=keep.y)
  }

  return(allFiles)
}

# help-function to merge D and H files
doMerge <- function(x,y,merge.by,keep.x,keep.y){

  by.x <- getNames(colnames(x),merge.by)
  by.y <- getNames(colnames(y),merge.by)

  mergedData <- merge(x,y,by.x=by.x,by.y=by.y,all.x=keep.x,all.y=keep.y)
  return(mergedData)
}

# help-function to get column names
getNames <- function(x,contains){
  contains <- paste(contains,collapse="|")
  x[grepl(contains,x)]
}
