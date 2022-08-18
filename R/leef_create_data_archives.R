#' Title
#'
#' @param to_dir directory in which the compressed data folders should be saved to
#' @param archive_dir base directory of the data archive
#' @param stage stage of the data. Allowed values are \code{"pre_processed"}, \code{"extracted"} or \code{c("pre_processed", "extracted")}
#' @param cores number of cores to be used for the parallel compression
#'
#' @return names of the created data zip archives
#'
#' @importFrom parallel mclapply
#' @export
#'
leef_create_data_archives <- function(
    to_dir = ".",
    archive_dir = "~/Duck/LEEFSwift3",
    stage = c("pre_processed", "extracted"),
    cores = 4
){


  if (length(stage) > 1) {
    lapply(
      stage,
      function(s) {
        zen_create_data_archives(
          to_dir = to_dir,
          archive_dir = archive_dir,
          stage = s,
          cores = cores
        )
      }
    )
  }

  if (!(stage %in% c("pre_processed", "extracted"))) {
    stop("stage has to be pre_processed or extracted!")
  }


  # Helper function - comp --------------------------------------------------


  comp <- function(datapath, timestamp, zipfile){
    # zip -9X ~/tmp/data_20220406.zip  *.20220406/*

    olddir <- getwd()
    result <- NULL
    on.exit(
      {
        setwd(olddir)
        if (is.null(result)){
          unlink(zipfile)
        }
        return(result)
      }
    )


    dir.create(dirname(zipfile), showWarnings = FALSE, recursive = TRUE)


    setwd(file.path(datapath))

    browser()

    utils::zip(
      zipfile = zipfile,
      flags = "-9X",
      files = file.path(".", paste0("*.", timestamp), "*")
    )
    result <- zipfile
  }


  # get datapath and timestamps ---------------------------------------------


  datapath  <- file.path(
    archive_dir,
    "LEEF.archived.data/LEEF/3.archived.data",
    stage
  )

  dirs <- list.dirs(datapath, full.names = FALSE, recursive = FALSE)
  timestamps <- gsub("^(.*?)\\.202", "202", dirs)
  timestamps <- gsub("^(.*?)\\.302", "302", timestamps)
  timestamps <- unique(timestamps)

  archives <- parallel::mclapply(
    timestamps,
    function(timestamp){
      list(
        timestamp = timestamp,
        zipfile = file.path(
          to_dir, paste0(
            "data", "_",
            stage, "_",
            timestamp, ".",
            "zip"
          )
        )
      )
    }
  )


  # Archive all timestamps --------------------------------------------------

  result <- parallel::mclapply(
    archives,
    function(x) {
      message("processing ", x$timestamp, x$zipfile)
      comp(
        datapath = datapath,
        timestamp = x$timestamp,
        zipfile = x$zipfile
      )
    },
    mc.cores = cores
  )

  return(result)
}
