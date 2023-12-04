#' Create the data packages from the LEEF project.
#'
#' The data packages contain only of the respective data, bundled in a zipped file.
#' @param to_dir directory in which the compressed data folders should be saved to
#' @param data_dir base directory of the data archive
#' @param stage stage of the data. Allowed values are \code{"pre_processed"},
#'   \code{"extracted"} or \code{c("pre_processed", "extracted")}
#' @param cores number of cores to be used for the parallel compression (default
#'   is one due to limits in connection)
#'
#' @return names of the created data zip archives
#'
#' @importFrom pbmcapply pbmclapply
#' @export
#'
leef_create_data_archives <- function(
    to_dir = ".",
    data_dir = "~/Duck/LEEFSwift3/LEEF/3.archived.data",
    metadata_dir = "/Volumes/LEEF/LEEF-1.metadata",
    stage = c("pre_processed", "extracted"),
    cores = 1) {
  if (length(stage) > 1) {
    lapply(
      stage,
      function(s) {
        leef_create_data_archives(
          to_dir = to_dir,
          data_dir = data_dir,
          stage = stage,
          cores = cores
        )
      }
    )
  }

  if (!(stage %in% c("pre_processed", "extracted"))) {
    stop("stage has to be pre_processed or extracted!")
  }

  to_dir <- normalizePath(to_dir)

  # Helper function - comp --------------------------------------------------


  comp <- function(datapath, files, zipfile) {
    # zip -9X ~/tmp/data_20220406.zip  *.20220406/*

    olddir <- getwd()
    result <- NULL
    on.exit({
      setwd(olddir)
      if (is.null(result)) {
        unlink(zipfile)
      }
      return(result)
    })

    dir.create(dirname(zipfile), showWarnings = FALSE, recursive = TRUE)

    setwd(file.path(datapath))

    utils::zip(
      zipfile = zipfile,
      extras = "-q",
      files = files
    )
    result <- zipfile
  }


  # get datapath and timestamps ---------------------------------------------


  datapath <- file.path(
    data_dir,
    stage
  )

  dirs <- list.dirs(datapath, full.names = FALSE, recursive = FALSE)
  timestamps <- gsub("^(.*?)\\.202", "202", dirs)
  timestamps <- gsub("^(.*?)\\.302", "302", timestamps)
  timestamps <- unique(timestamps)

  message("\nSetting up compression\n")
  archives <- pbmcapply::pbmclapply(
    timestamps,
    function(timestamp) {
      list(
        timestamp = timestamp,
        zipfile = file.path(
          to_dir, paste0(
            "data", "_",
            stage, "_",
            timestamp, ".",
            "zip"
          )
        ),
        files = grep(pattern = timestamp, x = dirs, value = TRUE)
      )
    },
    mc.cores = cores
  )


  # Compress all timestamps --------------------------------------------------

  message("\nProcessing ", nrow(archives), " directories - this will take some time!\n")
  result <- pbmcapply::pbmclapply(
    archives,
    function(x) {
      comp(
        datapath = datapath,
        files = x$files,
        zipfile = x$zipfile
      )
    },
    mc.cores = cores
  )

  return(result)
}
