#' @title
#' Extracting sequences from SHP waves
#'
#' @description
#' Based on the structure of the 'SPSS' version of the Swiss Household Panel (SHP) data, the function seeks the data of variables specified by the user in each of the wave files and collects them as sequence data in a table. The function can also match the sequences with variables from other files such as the master files of persons (MP) and households (MH) and social origins (SO). It can also match with activity calendar data (CA). In addition it can extract user specified covariates from a specific wave.
#'
#' @details
#' SHP data are available for free from FORS (\url{https://forscenter.ch/projects/swiss-household-panel/data/}) but require the user to accept the usage contract.
#'
#' The function extracts the columns corresponding to the protonames provided from the successive wave files and collects them in a tibble table. From this table, it is then, for example, straightforward to create state sequence objects for 'TraMineR'.
#'
#' When using the \code{shpdir} argument, the \code{shpdir} path must end with the two-digits number \code{xx} of the last wave. The path \code{wavedir} is then set as \code{shpdir/SHP-Data-W1-Wxx-SPSS/} and \code{datadir} as \code{shpdir/SHP-Data-WA-SPSS/}. For example, with \code{shpdir="C:/SHP/shp23"}, \code{wavedir} will be set as \code{"C:/SHP/shp23/SHP-Data-W1-W23-SPSS/"} and \code{datadir} as \code{"C:/SHP/shp23/SHP-Data-WA-SPSS/"}.
#'
#' The list of variable names \code{pvarseq} and \code{hvarseq} must be provided as protonames with \code{$$} standing for the last two digits of the year.
#'
#' \code{maxMissing} is set by default as \code{length(waves) - 1}, which drops cases for which one of the yearly sequence defined by \code{pvarseq} and \code{hvarseq} is empty (i.e., has no valid state). Likewise, \code{maxMissingCA} is set by default as \code{length(CAvar) - 1} to exclude cases with an empty monthly activity calendar sequence.
#'
#' The package is based on a function written in 2012 by Matthias Studer.
#'
#'
#' @param shpdir String. Root path of the SHP data. The path should end with the two-digits number of the last wave, e.g., \code{"C:/shp23"}.
#' @param wavedir String. Path to the SPSS SHP wave data. If \code{NULL}, \code{wavedir} is built from shpdir.
#' @param datadir String. Path to the SPSS WA (All Waves) data. If \code{NULL}, \code{datadir} is built from shpdir.
#' @param pvarseq Vector of strings. Protoname(s) ($$ for year) of the wanted sequence(s) of personal data.
#' @param hvarseq Vector of strings. Protoname(s) ($$ for year) of the wanted sequence(s) of household data.
#' @param MPvar   Vector of strings. Variables to be extracted from the person master (MP) file.
#' @param SOvar   Vector of strings. Variables to be extracted from the social origin (SO) file.
#' @param LJvar   Vector of strings. Variables to be extracted from the last job (LJ) file.
#' @param CAvar   Vector of strings. Variables to be extracted from the activity calendar (CA) file.
#' @param PLWvar  Vector of strings. Variables to be extracted from the \code{covw} wave personal file.
#' @param HLWvar  Vector of strings. Variables to be extracted from the \code{covw} wave household file.
#' @param maxMissing Integer. Maximum allowed missing states in yearly sequences (\code{pvarseq} and \code{hvarseq}).
#' @param maxMissingCA Integer. Maximum allowed missing states in monthly sequences (\code{CAvar}).
#' @param waves   Vector of integers. Selected waves (waves id number, not year!)
#' @param covw    Integer. Id number of wave from which to extract \code{PLWvar} and \code{HLWvar} covariates.
#'
#' @return A tibble with the selected sequence data and covariates.
#'
#' @references
#' Swiss Household Panel documentation at \url{https://forscenter.ch/projects/swiss-household-panel/}
#'
#'
#' @author Gilbert Ritschard
#'
#' @export
#'
#' @import haven
#' @import dplyr
#'
#' @seealso \code{\link{getColumnIndex}}
#'
#' @examples
#' ## Setting paths to SHP data files. Adapt to your local folders!
#' ## It should be something like
#' ## wavedir <- "C:/SwissHPanel/shp23/SHP-Data-W1-W23-SPSS/"
#' ## datadir <- "C:/SwissHPanel/shp23/SHP-Data-WA-SPSS/"
#'
#' ## Consider first the example of 3 waves and a MP file
#' ##  shipping with the package
#' wavedir <- paste0(system.file(package="seqSHP"),"/extdata/")
#' datadir <- wavedir
#'
#' ####### Working status
#'
#' first.w <- 1
#' last.w  <- 3
#' waves <- first.w:last.w
#' maxMissing <- 2
#'
#' ## Sequence of categorical variables
#' ##  WSTAT$$ is working status (WS)
#' shp <- seqFromWaves(wavedir, datadir,
#'                  pvarseq="WSTAT$$",
#'                  waves=waves, maxMissing=maxMissing)
#'
#' ## Retrieve WS labels
#' attr(shp$WSTAT00,"labels")
#'
#' ## Creating WS sequence object
#' library(TraMineR)
#' ws.shortlab <- c("AO","UN","NL")
#' ws.longlab <- c("Active Occupied","Unemployed","Not in Labor Force")
#' ws.alph <- c(1,2,3)
#' xtlab <- (1998+first.w):(1998+last.w)
#'
#' wsvar <- getColumnIndex(shp, "WSTAT$$")
#' ws.seq <- seqdef(shp[, wsvar], right=NA,
#'                  alphabet=ws.alph, states=ws.shortlab, labels=ws.longlab,
#'                  cnames=xtlab)
#'
#' ## plotting first 100 sequences
#' seqIplot(ws.seq[1:100,], sort="from.start")
#'
#'
#'
#' \dontrun{
#' ####################################################
#' ## To run the full examples below, you must first install SHP data
#' ## in an accessible folder
#' ##
#' ## Adapt to your local folders!
#' wavedir <- "C:/SwissHPanel/shp23/SHP-Data-W1-W23-SPSS/"
#' datadir <- "C:/SwissHPanel/shp23/SHP-Data-WA-SPSS/"
#'
#' ####### Working status
#'
#' first.w <- 2
#' last.w  <- 23
#' waves <- first.w:last.w
#' maxMissing <- 10
#'
#' ## Sequence of categorical variables
#' ##  WSTAT$$ is working status (WS) and
#' ##  P$$C44 satisfaction with life
#' shp <- seqFromWaves(wavedir, datadir,
#'                  pvarseq=c("WSTAT$$","P$$C44"),
#'                  waves=waves, maxMissing=maxMissing)
#'
#' ## Retrieve WS labels
#' attr(shp$WSTAT00,"labels")
#'
#' ## Creating WS sequence object
#' library(TraMineR)
#' ws.shortlab <- c("AO","UN","NL")
#' ws.longlab <- c("Active Occupied","Unemployed","Not in Labor Force")
#' ws.alph <- c(1,2,3)
#' xtlab <- (1998+first.w):(1998+last.w)
#'
#' wsvar <- getColumnIndex(shp, "WSTAT$$")
#' ws.seq <- seqdef(shp[, wsvar], right=NA,
#'                  alphabet=ws.alph, states=ws.shortlab, labels=ws.longlab,
#'                  cnames=xtlab, xtstep=2, tick.last=TRUE)
#'
#' seqIplot(ws.seq, sort="from.start")
#'
#'
#' ######### Activity calendar from sep99 to dec2021
#'
#' month.short.names <- tolower(sapply(month.name, substr, 1, 3))
#' xtlab.ca <- c("sep99","oct99","nov99","dec99")
#' for (t in 00:21) {
#'  xtlab.ca <- c(xtlab.ca,paste0(month.short.names, formatC(t,width=2,flag=0)))
#' }
#' names(xtlab.ca) <- xtlab.ca
#' ca.var <- toupper(xtlab.ca) ## SPSS variable names are uppercase
#'
#' CAseqdata <- seqFromWaves(wavedir, datadir, CAvar=ca.var, maxMissingCA=36)
#'
#' attr(CAseqdata$SEP99, "labels")
#' ## First 3 columns are IDPERS, SEX, and BIRTHY. Sequences from the other columns
#' seqCA <- seqdef(CAseqdata[,-(1:3)], cnames=xtlab.ca, right=NA, xtstep=6, tick.last=TRUE)
#' seqdplot(seqCA, border=NA, with.missing=TRUE)
#'
#' }


### Code based on an earlier function by Matthias Studer

seqFromWaves <- function (wavedir=NULL, datadir=NULL, shpdir=NULL, pvarseq=NULL, hvarseq=NULL,
      MPvar = c("SEX", "BIRTHY"),
		  SOvar=NULL, LJvar=NULL, CAvar=NULL, PLWvar=NULL, HLWvar=NULL,
		  waves=NULL, covw=max(waves), maxMissing=length(waves)-1, maxMissingCA=length(CAvar)-1){

  if (is.null(shpdir) & (is.null(wavedir) | is.null(datadir)))
    stop("when shpdir=NULL, both wavedir and datadir must be specified!")

  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

  if (is.null(waves) & any(c(!is.null(pvarseq),!is.null(hvarseq))))
    stop("waves cannot be NULL when any of pvarseq and hvarseq is not NULL")

  if (!is.null(shpdir)){
    nchr <- nchar(shpdir)
    if (substrRight(shpdir,1)=="/") {
      nchr <- nchr-1
      shpdir <- substr(shpdir,1,nchr)
    }
    lastwave <- substr(shpdir,nchr-1,nchr)
    if(is.na(suppressWarnings(as.numeric(lastwave))))
      stop("shpdir must end with a two-digits number (last wave number)!")
    if (is.null(wavedir))
      wavedir <- paste0(shpdir,"/SHP-Data-W1-W",lastwave,"-SPSS/")
    else if (substrRight(wavedir,1)!="/")
	    wavedir <- paste0(wavedir, "/")

    if (is.null(datadir))
      datadir <- paste0(shpdir,"/SHP-Data-WA-SPSS/")
    else if (substrRight(datadir,1)!="/")
      datadir <- paste0(datadir, "/")
  }
  message("shpdir:  ",shpdir)
  message("wavedir: ",wavedir)
  message("datadir: ",datadir)

  ## Master individual file
	suppressWarnings(MP <- read_sav(paste(datadir,"SHP_MP.sav",sep="")))
	#if (is.null(hvarseq))
	  shp <- subset(MP, select=c("IDPERS", MPvar))
	#else {
	#  idhousvar <- paste0("IDHOUS", formatC(waves-2, width=2, flag="0"))
	#  idhousvar[idhousvar=="IDHOUS-1"] <- "IDHOUS99"
	#  shp <- subset(MP, select=c("IDPERS", MPvar, idhousvar))
	#}

	## ==============
	## For each wave
	## ==============

	pvarseq <- c(pvarseq, hvarseq)
	allvars <- NULL
  if (!is.null(waves)){
  	for(w in waves){
  		twoDigitYear <- formatC(w - 2, width=2, flag="0")
  		if(w==1){
  			twoDigitYear <- "99"
  		}
  		suppressWarnings(WW <- read_sav(paste(wavedir,"/W",w, "_", (w+1998), "/SHP", twoDigitYear, "_P_USER.sav",sep="")))
  		## select variables
  		if((!is.null(hvarseq)) || (w==covw && !is.null(HLWvar))){
  		  suppressWarnings(WWH <- read_sav(paste(wavedir,"/W",w, "_", (w+1998), "/SHP", twoDigitYear, "_H_USER.sav",sep="")))
  		  ## IDHOUS$$ can be non unique in $$_H_USER, we keep only first occurrence
  		  idh.unique <- unique(as.data.frame(WWH)[,paste0("IDHOUS",twoDigitYear)])
  		  hmatch <- match(idh.unique, pull(as.data.frame(WWH), paste0("IDHOUS",twoDigitYear)))
  		  WWH <- WWH[hmatch[!is.na(hmatch)],]
  			WW <- left_join(WW, WWH, by=paste("IDHOUS", twoDigitYear, sep=""), relationship = "many-to-one")
  		}
  		varsubsetYear <- sub("\\$\\$", twoDigitYear, pvarseq)
  		allvars <- c(allvars, varsubsetYear)
  		if(w==covw){
  			varsubsetYear <- c(varsubsetYear,  sub("\\$\\$", twoDigitYear, c(HLWvar, PLWvar)))
  		}
  		varsubsetYearPos <- match(varsubsetYear, colnames(WW))
  		## Check for variables not found in this wave
  		if(any(is.na(varsubsetYearPos))){
  			nomatch <- varsubsetYear[is.na(varsubsetYearPos)]
  			message("[!] Variable(s): ", paste(nomatch, collapse=", "), " were not found in wave W", w, ". Skipping.\n")
  			varsubsetYear <- varsubsetYear[!is.na(varsubsetYearPos)]
  		}
  		WW <- subset(WW, select=c("IDPERS", varsubsetYear))
  		## merge WW with previous shp.
  		shp <- left_join(shp, WW, by="IDPERS", relationship="one-to-one")
  	}
  }
	for(file in c("SO", "LJ", "CA")){
		var <- get(paste(toupper(file), "var", sep=""))
		if(!is.null(var)){
			var <- sub("\\$\\$", "..", var)
			suppressWarnings(WW <- read_sav(paste(datadir, "SHP_", file, ".sav", sep="")))
			names(WW) <- sub("\\$\\$", "..", names(WW))
			WW <- subset(WW, select=c("IDPERS", var))
			shp <- left_join(shp, WW, by="IDPERS", relationship="one-to-one")
		}
	}
	if (!is.null(pvarseq) & maxMissing < length(waves)){
 		for(var in pvarseq){
		  shp <- shp[rowSums(is.na(shp[, getColumnIndex(shp, var)])) <= maxMissing, ]
		}
	}
	if (!is.null(CAvar) & maxMissingCA < length(CAvar)){
	  shp <- shp[rowSums(is.na(shp[, CAvar])) <= maxMissingCA, ]
	}
	return(shp)
}


#' @title
#' Column indexes of protoname in the table returned by extractSeqFromWaves
#'
#' @description
#' Column indexes in \code{data} of the variable fitting the provided protoname.
#'
#' @details
#' Returns the column indexes in \code{data} of the variable fitting the provided protoname.
#' Examples in help page of \code{\link{seqFromWaves}}.
#'
#'
#' @param data Tibble or data.frame. Table returned by \code{\link{seqFromWaves}}.
#' @param protoname String. Protoname of a variable.
#'
#' @export
#'
#' @return Vector of indexes of the columns of \code{data} corresponding to the protoname.
#'
#' @seealso \code{\link{seqFromWaves}}


getColumnIndex <- function(data, protoname){
	index <- grep(paste("^", sub("\\$\\$", "[0-9][0-9]", protoname), "$", sep=""), names(data))
	if(length(index)==0){
		stop("[!] Variable ", protoname, " not found.")
	}
	return(index)
}
