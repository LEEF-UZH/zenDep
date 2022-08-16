#' Title
#'
#' @param to_dir directory in which the compressed data folders should be saved to
#' @param archive_dir base directory of the data archive
#' @param timestamp timestamp to be uploaded
#' @param stage stage of the data. Allowed values are \code{"pre_processed"}, \code{"extracted"}
#'
#' @return names of the created data zip archives
#' @export
#'
zen_create_data_archives <- function(
    to_dir = ".",
    archive_dir = "~/Duck/LEEFSwift3",
    timestamp,
    stage = c("pre_processed", "extracted")
){


  # Helper function - comp --------------------------------------------------


  comp <- function(datapath, timestamp, zipfile){
    # zip -9X ~/tmp/data_20220406.zip  *.20220406/*

    olddir <- getwd()
    f <- file.path(tempfile())
    result <- NULL
    on.exit(
      {
        setwd(olddir)
        return(result)
      }
    )

    f <- file.path(tempfile())
    dir.create(f)
    f <- file.path(f, "data.zip")

    setwd(file.path(datapath))

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

  archives <- lapply(
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
      comp(
        datapath = datapath,
        timestamp = x$timestamp,
        zipfile = x$zipfile
      )
    },
  )

  return(result)
}
