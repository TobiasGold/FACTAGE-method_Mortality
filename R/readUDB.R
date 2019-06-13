#' Read UDB-Data
#'
#' Specify a directory from which you want to read in .csv files containing SILC UDB-Data.
#' Furthermore, specify the year span, the countries and the analytical variables for the extraction.
#' Since data will overlap, only selects the most recent observations for each respondent.
#' Creates a big list element which then will be used by mergeUDB().
#'
#' @param path Specify the path to the folder where the longitudinal SILC UDB data is stored.
#' @param year_from Default value = NULL. Specify the first release-year of data extraction.
#' @param year_to Default value = NULL. Specify the last release-year of data extraction.
#' @param countries Default value = NULL. Specify which countries to extract in ISO 3166-2 (two letter abbreviations), if NULL then all available countries are extracted.
#' @param vars Default value = NULL. Specify which variables to extract (for a complete list look at the DOCSILC065 pdf), if NULL no additional variables are extracted.
#' @param verbose Default value = TRUE. Be chatty?
#' @param essential.vars The variables needed to run the code successfully and produce a useable output file.
#'
#'
#' @return A list element with D.Files, H.Files, P.Files and R.Files in it.
#' @author Johannes Gussenbauer, Tobias Göllner
#' @export
#'
#' @examples
#' # Set all parameters:
#' path <- "/home/SILC-UDB/2016CL/2016-3/"
#' year_from <- 2010
#' year_to <- 2016
#' countries <- c("AT", "BE", "FR")
#' vars <- c("PH010","PH030", "HY020")
#' # Call the function:
#' SILC_UDB_2016_3 <- readUDB(path,year_from,year_to,countries,vars)
#' # Same call but inside the function:
#' SILC_UDB_2016_3 <- readUDB(path="/home/SILC-UDB/2016CL/2016-3/",year_from=2010,year_to=2016,countries=c("AT", "BE", "FR"),vars=c("PH010","PH030", "HY020"))
#'

readUDB <- function(path,year_from=NULL,year_to=NULL,countries=NULL,vars=NULL,verbose=TRUE,
                    essential.vars = list(D.vars = c("DB010", "DB020", "DB030", "DB110"),
                    H.vars = c("HB010", "HB020", "HB030", "HB050", "HB060"),
                    P.vars = c("PB010", "PB020", "PB030", "PB100", "PB110"),
                    R.vars = c("RB010", "RB020", "RB030", "RB040", "RB070", "RB080", "RB090", "RB110", "RB140", "RB150", "RX010"))){


  if(is.null(year_from)){
    year_from <- 1990
  }
  if(is.null(year_to)){
    year_to <- 2100
  }

  # build file paths to .csv files
  filePath <- buildFilePath(path,countries,year_from,year_to)

  # loop over different file endings (D-file, H-file,...)
  # and read in data seperately
  readData <- list()

  for(i in names(essential.vars)){
    fileType <- substr(i,1,1)
    select.vars <- c(essential.vars[[i]],vars[grepl(paste0("^",fileType),vars)])
    if(verbose){
      cat("Reading data for",fileType,"-Files\n")
      cat("Selecting the following variables:\n",select.vars,"\n\n")
    }
    datafiles <- readMultipleFiles(filePath,ending=paste0(fileType,".csv"),select=select.vars)

    # remove countries
    country.col <- paste0(fileType,"B020")
    if(!is.null(countries)){
      datafiles <- datafiles[get(country.col)%in%countries]
    }
    if(any(countries%in%c("GR","EL"))|is.null(countries)){
      datafiles[get(country.col)%in%c("GR","EL"),c(paste0(fileType,"B020")):="GR"]
    }

    # remove years
    # and get for each valid year the latest
    # data delivery
    year.col <- paste0(fileType,"B010")
    datafiles <- datafiles[get(year.col)%between%c(year_from,year_to)]
    # latestDelivery <- datafiles[get(year.col)%between%c(year_from,year_to),.(.id=max(.id)),by=c(year.col)]
    # datafiles <- datafiles[latestDelivery,,on=c(".id",year.col)]
    #
    # datafiles[RB020=="AT"&RB010==2006,uniqueN(.id),by=RB030][V1>1]
    # dcast(datafiles[,.N,by=c(country.col,year.col,".id")],RB020+RB010~.id)
    # datafiles[,.N,by=c(country.col,year.col,".id","RB040")][,uniqueN(N),by=c(country.col,year.col,"RB040")][V1>1]

    # get unique IDs
    id <- paste0(fileType,"B030")
    id.col <- c(id,country.col,year.col)
    datafiles[,selectRecords:=.id==max(.id),by=c(id.col)]

    if(fileType=="R"){
      originalOrder <- copy(colnames(datafiles))

      setorderv(datafiles,c(id.col))
      datafiles[,helpRecords:=sum(selectRecords)>1,by=c(id.col)]
      datafiles[datafiles[helpRecords==TRUE,mget(c(id,country.col))],reoccur:=c(diff(get(year.col))<=1,FALSE),by=c(id,country.col),on=c(id,country.col)]
      datafiles[selectRecords==TRUE&helpRecords==TRUE,selectRecords:=helpSelect(RB110,reoccur[1]),by=c(id.col)]
      datafiles[,c("helpRecords","reoccur"):=NULL]

      setcolorder(datafiles,originalOrder)
    }

    datafiles[,c("dropComplete","multipleIDs"):=.(all(!selectRecords),
                                                  sum(selectRecords)>1),by=c(id.col)]
    if(nrow(datafiles[dropComplete==TRUE|multipleIDs==TRUE])>1){
      message("The following records appear multiple times in a single file and should be checked manually before proceeding:")
      print(datafiles[dropComplete==TRUE|multipleIDs==TRUE,mget(id.col)])

      datafiles[dropComplete==TRUE,selectRecords:=dropComplete]
    }

    datafiles <- datafiles[selectRecords==TRUE]
    datafiles[,c("selectRecords",".id","dropComplete","multipleIDs"):=NULL]

    fileType <- paste0(fileType,".Files")
    readData[[fileType]] <- datafiles
  }

  return(readData)
}


# help function to build file paths for SILC-UDB data
buildFilePath <- function(path,countries,year_from,year_to){

  nextPath <- list.files(path,full.names = TRUE)
  pathEnding <- gsub("^.*/","",nextPath)

  if(all(grepl("^[[:alpha:]]{2}$",pathEnding))){
    if(is.null(countries)){
      countries <- pathEnding
    }
    selectPath <- which(pathEnding%in%countries)
  }else if(any(pathEnding%in%c(year_from:year_to))){
    selectPath <-  which(pathEnding>=year_from&pathEnding<=year_to)
  }else{
    selectPath <- grepl("UDB_l",pathEnding)
    path <- nextPath[selectPath]
    return(path)
  }

  path <- nextPath[selectPath]
  buildFilePath(path,countries,year_from,year_to)
}

# help-function to read in multiple files using fread()
readMultipleFiles <- function(files,ending,select=NULL){
  readFiles <- files[grepl(ending,files)]

  n <- nchar(readFiles)-nchar(ending)
  nameFiles <- substr(readFiles,n-1,n)

  readFiles <- lapply(readFiles,function(z){

    tryCatch({
      # check colnames
      checkcol <- fread(z,nrows=1)
      fread(z,select=select[select%in%colnames(checkcol)],fill=TRUE)
    },
    error=function(cond){
      message("An error occured for file:",z)
      message("Original error message:\n")
      message(cond,"\n")
      message("Proceeding with read.csv() which will be much slower. Consider fixing your files!\n")

      getInt64 <- grepl("B030|B040",select)
      class64 <- rep("integer64",sum(getInt64))
      names(class64) <- select[getInt64]

      dat <- data.table(read.csv(z, header=TRUE,stringsAsFactors=FALSE,colClasses=class64))
      country <- select[grepl("B020",select)]
      return(dat[get(country)!="",mget(select)])
    }#,
    # warning=function(cond){
    #   message("A warning occured for file:",z)
    #   message("Original warning message:")
    #   message(cond,"\n")
    #   message("Proceeding with read.csv() which will be much slower. Consider fixing your files!\n")
    #
    #   getInt64 <- grepl("B030|B040",select)
    #   class64 <- rep("integer64",sum(getInt64))
    #   names(class64) <- select[getInt64]
    #
    #   dat <- data.table(read.csv(z, header=TRUE,stringsAsFactors=FALSE,colClasses=class64))
    #   country <- select[grepl("B020",select)]
    #   return(dat[get(country)!="",mget(select)])
    # }
  )
  })
  names(readFiles) <- nameFiles
  readFiles <- rbindlist(readFiles,use.names=TRUE,fill=TRUE,idcol=TRUE)
  readFiles[,c(".id"):=as.numeric(.id)]
  return(readFiles)
}


# helpfunction to select unique rows
helpSelect <- function(RB110,reoccur=FALSE){

  if(identical(RB110,c(5,2))){
    return(RB110==2)
  }else if(identical(RB110,c(7,3))){
    return(RB110==3)
  }else{
    if(reoccur){
      return(RB110%in%c(1:4))
    }else{
      return(RB110%in%c(5:6))
    }
  }
}

# help-function to place flag if record occurs next year again
helpOccur <- function(year){
  out <- rep(TRUE,length(year))
}
