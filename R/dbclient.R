####  DistrictBuilder API client

## TODO:
##
## - Package build
## - Planscore API
## - Metadata extraction
## - Convert to geomander format
## - Add Lstmod date to file name

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

  uris <- purrr::map_dfr(projectids,retrieve_uris)

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
       purrr::insistently(download, purrr::rate_backoff(max_times=retries)),
      otherwise=523, quiet = TRUE), purrr::rate_delay(delay) )

  safe_download_w<-function(id,uri,ext) {
    res <- safe_download(id,uri,ext)
      if ((verbose>0) && (res$result != 200)) {
        print(paste("MISSING PLAN:",uri,"(",res$result,")"))
      }
      res$result
  }

  rv<-purrr::pmap(uris,safe_download_w)

  rv
}

##
## Harvesting
##

#' Retrieve plan information from districtbuilder
#'
#' NOTE: Attempts to be sensible including: retrieving multiple formats and metadata,
#'  compressing files with gzip, retrying on network errors, skipping files previously
#'  retrieved successfully . These are configurable through options()$dbclient.* .
#'
#' @after collect plans updated after given date (for incremental updates)
#' @param targetdir destination directory
#' @return number of last page retrieved (for efficiency in restarting)
#' @examples retrieve_plans( targetdir="tmp", after="2021-09-25")
#'

retrieve_plans<-function( targetdir=".", after="1900-01-01", start=1, pagesize=20) {

  page <- start
  earliestplan="2999-01-01"
  verbose<-options()$dbclient.verbose

  if (!dir.exists(targetdir)) {
    stop("Directory does not exist:",targetdir)
  }

  pagesize <- as.numeric(pagesize)
    if ((pagesize < 2) || (pagesize>100)) {
      warning("Invalid pagesize -- using default")
      pagesize <- 20
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

#' create metadata data frame for a set of districtbuilder plans
#' Note: - this operates on a director of previously harvested files, and will retrieving additional from the districtbuilder app
#' if the requested ids do not exist in the directory
#'
#'
#'
#' @param ids list of districtbuilder ids
#' @param targetdir destination directory
#' @examples db2meta( ids=c("32e5d326-02af-44a9-944b-e2add24d9613","530585f8-557f-4e60-a6af-0ac50d6df271","32d4da61-ef4f-4015-8d21-7f64ab895dbc"), targetdir="tmp", after="2021-09-25")
#'


db2meta <- function(ids, targetdir=".") {
  if (!dir.exists(targetdir)) {
    stop("Directory does not exist:",targetdir)
  }

  metaFiles <- fs::path(targetdir,ids,ext="json.gz")
  missingIds <- ids[which(!file.exists(metaFiles))]
  download_districtbuilder_plans(projectids = missingIds,targetdir = targetdir)

  extractMetaJson <- function(f) {
    if (!file.exists(f)) {return(list(file_src = f))}
    tmpfield <- jsonlite::fromJSON(gzfile(f),flatten=TRUE)
    list(
      file_src = f,
      plan_id = tmpfield$id,
      plan_name = tmpfield$name,
      plan_date = tmpfield$updatedDt,
      plan_creator = tmpfield$user$name,
      plan_creator_id = tmpfield$user$id,
      plan_region = tmpfield$regionConfig$regionCode
    )
  }
  purrr::map_dfr(metaFiles,extractMetaJson)
}

#' create sf objects for db plans
#'
#' Note: - this operates on a director of previously harvested files, and will retrieving additional from the districtbuilder app
#' if the requested ids do not exist in the directory
#'
#'
#'
#' @param ids list of districtbuilder ids
#' @param targetdir destination directory
#' @examples db2meta( ids=c("32e5d326-02af-44a9-944b-e2add24d9613","530585f8-557f-4e60-a6af-0ac50d6df271","32d4da61-ef4f-4015-8d21-7f64ab895dbc"), targetdir="tmp", after="2021-09-25")
#'
db2sf <- function(ids,targetdir=".") {
  metaFiles <- fs::path(targetdir,ids,ext="geojson.gz")
  missingIds <- ids[which(!file.exists(metaFiles))]
  download_districtbuilder_plans(projectids = missingIds,targetdir = targetdir)

  convertGeojson <- function(f) {
    if (!file.exists(f)) {return(NULL)}
    jsonStr <- readr::read_file(f)
    tstSf <- geojsonsf::geojson_sf(jsonStr)
    flatFeatures <-  jsonlite::fromJSON(f,flatten=TRUE)[["features"]]
    res <- bind_cols(
      tstSf,
      flatFeatures %>% select(id,starts_with("properties"))
    )


  }
  purrr::map(metaFiles,convertGeojson)
}

db2geomander <- function (ids, targetdir, year=2020)
{
  if (!dir.exists(targetdir)) {
    stop("Directory does not exist:",targetdir)
  }

  metaFiles <- fs::path(targetdir,ids,ext="csv.gz")
  missingIds <- ids[which(!file.exists(metaFiles))]
  download_districtbuilder_plans(projectids = missingIds,targetdir = targetdir)

  convertBE <- function(f) {
    if (!file.exists(f)) {return(NULL)}
    tmp.tb <- readr::read_csv(f,col_types="ci")
    tmp.tb <- dplyr::rename(tmp.tb,District=DISTRICT)
    fipstate <- stringr::str_sub(tmp.tb[1,1],1,2)
    shp <- tigris::blocks(fipstate,year=year)
    shp <- dplyr::left_join(shp,tmp.tb, by = c(GEOID10 = "BLOCKID"))
  }

  purrr::map(metaFiles,convertBE)
}

generate_planscore_df(ids, targetdir=".", apikey) {
    if (!dir.exists(targetdir)) {
      stop("Directory does not exist:",targetdir)
      return(0)
    }

  # get db plan geojson
  metaFiles <- fs::path(targetdir,ids,ext="geojson.gz")
  missingIds <- ids[which(!file.exists(metaFiles))]
  download_districtbuilder_plans(projectids = missingIds,targetdir = targetdir)

  purrr::map_dfr(metaFiles,geo2planscore)
}


geo2planscore<-function(f="",apikey,to=60, continueURI = NULL ) {

  if (is.null(continueURI)) {
    # post part 1

    r1<- httr::GET("https://api.planscore.org/upload",
                   httr::add_headers(.headers=c(
                     "Authorization" = paste(sep="","Bearer ", apikey)
                   )),
                   httr::timeout(to)
    )

    if (r1$status_code != 200) {
      warning("PlanScore failed at stage 1 (possible bad api key): ", httr::content(r1) )
      return(list(file=f))
    }


    # post part 2: S3 Bucket Upload
    fgu <- R.utils::gunzip(f, temporary=TRUE, overwrite=TRUE, remove=FALSE)
    uri2 <- httr::content(r1)[[1]]
    headers2 <- httr::content(r1)[[2]]
    upl <- httr::upload_file(fgu)

    params <- headers2
    params <- append(params,list("file"=upl))

    r2 <- httr::POST(uri2,
                     body =  params, # NOTE: fragile syntax for upl
                     httr::timeout(to),
                     encode="multipart"
                     #httr::verbose()
    )

    # TODO: Response is 503 with URI should be 301, but redirect URI is included
    #       Follow up

    uri3 <- r2[1]$url

    if (is.null(uri3))  {
      warning("PlanScore failed at stage 1 (possible bad api key): ", httr::content(r2) )
      return(list(file=f))
    }

    # post part 3: follow up


    r3<- httr::POST(uri3,
                    httr::add_headers(.headers=c(
                      "Authorization" = paste(sep="","Bearer ", apikey),
                      "Content-Type" = " application/json"
                    )),
                    body = '{"description": "dbclient upload"}',
                    httr::timeout(to)

    )

    # Stage 4: poll reply

    if (r3$status_code != 200) {
      warning("PlanScore failed at stage 3, status: ", r3$status_code )
      return(list(file=f))
    }

    uri4 <- httr::content(r3)$index_url

  } else {
    uri4=continueURI
  }

  #polling loop

  getResultsOnce<- function(u) {
    r<- httr::GET(u)
    if (r$status_code!=200) stop("Bad http status", r$status_code)
    res <- jsonlite::fromJSON(httr::content(r,encoding="UTF-8"),flatten=TRUE)
    if (!res$status) stop("Bad planscore status")
    if (res$progress[1]!=res$progress[2]) stop("Not finished")
    res
  }

  getResults <-
    purrr::possibly(
      purrr::insistently(getResultsOnce,
                         rate = purrr::rate_backoff(  pause_base = 10,  pause_cap = 120, max_times=8)
      ),
      otherwise=NULL
    )

  rval <- getResults(uri4)

  if (is.null(rval)) {
    warning("PlanScore failed at final stage of computing scores ", uri4 )
    return(list(file=f,planscoreURI=uri4))

  }

  res <- dplyr::tibble(file=f,
                       planscoreURI=uri4,dplyr::as_tibble(rval$summary),
                       districts=list(rval$districts))

  res
}

# ISSUES: bugs on globalProects api
#-- api should return 404 on bad path requests (e.g. /api//globalProjects), not 200
#-- globalProjects api should include completed flag in metadata
#-- page numbers shift as new plans are added -- need date range to stabilize
#-- intermittent 504 errors
#-- limit > 100 generates consistent 504

# ISSUES: missing metadata
# - json - missing plan level scores, chamber, completeness
# - geojson - missing district level score in score panel - PVI, deviation; missing all evaluation, including county splits
# -           - contiguity is reported as a string with arbitray labels, rather than as a logical or numeric

