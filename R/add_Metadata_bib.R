#' Add bibliographic metadata to a `ZenodoRecord`
#'
#' @param metadata_bib an object of class `metadata_bib` as created by the
#'   function `new_metadata_bib()`.
#' @param rec an object of class `ZenodoRecord` s created by
#'   `zen4R::ZenodoRecord$new()`. If missing, a new one will be created.
#'
#' @return the object `rec` with the metadata in `metdata_bib` added.
#'
#' @md
#'
#' @import zen4R
#'
#' @export
#'
#' @examples
add_metadata_bib <- function(
    metadata_bib = new_metadata_bib(),
    rec = zen4R::ZenodoRecord$new()) {
  if (!inherits(metadata_bib, "metadata_bib")) {
    stop("`metadata_bib` has to be an object of class `metadata_bib` as created by the function `new_metadata_bib()!")
  }


  rec$setTitle(metadata_bib$title)
  rec$setDescription(metadata_bib$description)
  rec$setKeywords(metadata_bib$keywords)
  rec$addCommunities(metadata_bib$communities)
  rec$setUploadType(metadata_bib$upload_type)
  lapply(
    metadata_bib$authors,
    function(aut) {
      rec$addCreator(
        firstname = aut$firstname,
        lastname = aut$lastname,
        affiliation = aut$affiliation,
        orcid = aut$orcid
      )
    }
  )
  rec$setVersion(metadata_bib$version)
  rec$setLanguage(metadata_bib$language)
  rec$setAccessRight(metadata_bib$access_right)
  rec$setEmbargoDate(metadata_bib$embargo_date)
  rec$setLicense(metadata_bib$license)
  lapply(
    metadata_bib$contributors,
    function(con) {
      rec$addContributor(
        firstname = con$firstname,
        lastname = con$lastname,
        type = con$type,
        affiliation = con$affiliation,
        orcid = con$orcid
      )
    }
  )
  rec$setGrants(metadata_bib$grants)


  return(rec)
}
