####  DistrictBuilder API client

## TODO:
##
## - Package build
## - Planscore API
## - Metadata extraction
## - Convert to geomander format

## .onLoad
##
## Intended to be called on package load
##
## Sets global options
##

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.new <- list(
    dbclient.server = "https://app.districtbuilder.org",
    dbclient.retries = 4,
    dbclient.delay = 2,
    dbclient.verbose = 2,
    dbclient.clobber = FALSE,
    dbclient.gzip = TRUE

  )
  toset <- !(names(op.new) %in% names(op))
  if(any(toset)) options(op.new[toset])

  invisible()
}

##
## functions to export individual plans
##

retrieve_csv_uri <- function(projectid, server) {
  t.uri <- paste(sep="",server,"/api/projects/",projectid,"/export/csv")
  attr(t.uri,"extension") <- "csv"
  c(id=projectid,uri=t.uri,ext="csv")
}

retrieve_geojson_uri <- function(projectid, server) {
  t.uri <- paste(sep="",server,"/api/projects/",projectid,"/export/geojson")
  attr(t.uri,"extension") <- "geojson"
  c(id=projectid,uri=t.uri,ext="geojson")
}

retrieve_meta_uri <- function(projectid, server) {
  t.uri <- paste(sep="",server,"/api/projects/",projectid,"/")
  attr(t.uri,"extension") <- "json"
  c(id=projectid,uri=t.uri,ext="json")
}

retrieve_uris <- function(projectid,
          types=c("csv","geojson","meta")
) {
  validtype <- c("csv","geojson","meta")
  if (!all(types %in% validtype)) {
    warning("Invalid type:", setdiff(types,validtype))
  }

  server<-options()$dbclient.server


  uris <- purrr::map_dfr( intersect(types,validtype) ,
     function(i) eval(call(paste(sep="","retrieve_",i,"_uri"),projectid,server))
  )

  uris
}

#' Retrieve plan information from districtbuilder
#'
#' @param projectid project identifier
#' @param types type of formats to retrieve (details to all)
#' @param dir destination directory
#' @return number of files retrieved
#' @examples
#'
#' retrieve_districtbuilder("ce81fcfb-8caf-49cf-b6ff-c4785a7ac679")
download_districtbuilder_plans <- function (projectids, targetdir=".") {
  if (!dir.exists(targetdir)) {
    stop("Directory does not exist:",targetdir)
    return(0)
  }

  retries<-options()$dbclient.retries
  delay<-options()$dbclient.delay
  verbose<-options()$dbclient.verbose
  gzip<-options()$dbclient.gzip
  clobber<-options()$dbclient.clobber

  uris <- map_dfr(projectids,retrieve_uris)

  download <- function(id,uri,ext) {
    if(verbose>2) print(paste("URI:",uri))

    targetfile <- fs::path(targetdir,id,ext=ext)
    if (!clobber) {
      cltarget <- targetfile
      if(gzip) {cltarget<-paste(sep="",targetfile,".gz")}
      if (file.exists(cltarget)) {
          if(verbose>2) print(paste("CACHED:",uri))
          return(200)
      }
    }
    req <- curl::curl_fetch_disk(uri, targetfile)
    rv <- req$status_code

    if (rv!=200) {
      file.remove(targetfile)
    }  else {
      if (gzip) R.utils::gzip(targetfile)
    }
    rv
  }

  safe_download <- purrr::slowly( purrr::safely(
       purrr::insistently(download, rate_backoff(max_times=retries)),
      otherwise=523, quiet = TRUE), rate_delay(delay) )

  safe_download_w<-function(id,uri,ext) {
    res <- safe_download(id,uri,ext)
      if ((verbose>0) && (res$result != 200)) {
        print(paste("MISSING PLAN:",uri,"(",res$result,")"))
      }
      res$result
  }

  rv<-pmap(uris,safe_download_w)

  rv
}

##
## Harvesting
##

retrieve_plans<-function( targetdir=".", after="1900-01-01", start=1, pagesize=100) {
  page <- start
  earliestplan="2999-01-01"
  verbose<-options()$dbclient.verbose

  if (!dir.exists(targetdir)) {
    stop("Directory does not exist:",targetdir)
    return(0)
  }

  repeat{
    if(verbose>1) print(paste("page:", page, "after:", after))
    resp<-retrieve_planlist(page,pagesize)
    if (!resp$success){
      warning("Failed on page")
    }
    if(verbose >1){
      print(paste("npages",resp$npage,"earliest on page",resp$earliest))
    }
    resr<-download_districtbuilder_plans(resp$ids,targetdir=targetdir)

    if((resp$npage<page) || (as.Date(resp$earliest) <= as.Date(after))) {
      break
    }
    page <- page +1
  }
  return(page)
}

retrieve_planlist<-function(page,pagesize) {

  res <- list(ids=NULL,earliest="",npage=0,success=TRUE)

  #https://app.districtbuilder.org/api/globalProjects?page=1&limit=100
  #completed=true&region=PA

  retries<-options()$dbclient.retries
  delay<-options()$dbclient.delay
  server<-options()$dbclient.server

  uri <- paste(sep="",server,"/api/globalProjects?","page=",page,"&limit=",pagesize)
  sRETRY <- purrr::possibly(httr::RETRY,NULL)
  req <- sRETRY("GET",uri,
                times=retries, pause_base=delay)
  if (is.null(req) || (req$status_code != 200)) {
    res$success<-FALSE
  } else {
    ct <- httr::content(req,as="parsed")
    #print(ct$meta)
    res$npage <- ct$meta$totalPages
    res$earliest <- tail(ct$items,n=1)[[1]]$updatedDt
    res$ids <- sapply(ct$items,function(x)x[["id"]])
  }

  return(res)
}

# TODO: File bugs
#-- api should return 404 on malformed requests, not 200
#-- globalProjects api should include completed flag in metadata
#-- page numbers shift as new plans are added -- need date range to stabilize

