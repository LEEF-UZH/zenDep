#' Create list containing the bibligraphic metadata for the data deposit
#'
#' @param community community to which the deposited data should be added
#' @param upload_type type of the deposited data
#' @param authors authors of the deposited data. Each contributor
#'   is also a list with the following fields:
#'   - firstname
#'   - lastname
#'   - affiliation
#'   - orcid
#' @param description description of the deposited data
#' @param version version of the deposited data
#' @param language language of the deposited data
#' @param keywords keywords
#' @param access_right access right of the deposited data
#' @param license license of the deposited data
#' @param contributors list of contributors of the deposited data. Each contributor
#'   is also a list with the following fields:
#'   - firstname
#'   - lastname
#'   - type: the role. This can be a character with multiple values which will be expanded for submission to Zenodo
#'   - affiliation
#'   - orcid
#'
#' @return list containing the metadata necessary for the data deposit
#'
#' @md
#'
#' @export
#'
new_metadata_bib <- function(
    title = "Some Title",
    community = "leef-uzh",
    upload_type = "dataset",
    authors = list(
      RMK = list(
        firstname = "Rainer M",
        lastname = "Krug",
        affiliation = "University of Zurich",
        orcid = "0000-0002-7490-0066"
      ),
      OLP = list(
        firstname = "Owen L",
        lastname = "Petchey",
        affiliation = "University of Zurich",
        orcid = "0000-0002-7724-1633"
      )
    ),
    description = "Description of the data",
    version = "1.0.0",
    language = "eng",
    keywords = c("LEEZ-UZH", "LEEF-1"),
    access_right = "embargoed",
    embargo_date = as.Date("2030-01-13"),
    license = "CC-BY-SA-4.0",
    contributors = list(
      RMK = list(
        firstname = "Rainer M",
        lastname = "Krug",
        type = c("DataManager", "Software"),
        affiliation = "University of Zurich",
        orcid = "0000-0002-7490-0066"
      ),
      OLP = list(
        firstname = "Owen L",
        lastname = "Petchey",
        type = "ProjectLeader",
        affiliation = "University of Zurich",
        orcid = "0000-0002-7724-1633"
      )
    ),
    grants = "some names of the grant"
){

  # Expand contributors when more than one type (role) is specified ---------


  lc <- length(contributors)
  for (i in 1:lc) {
    if (length(contributors[[i]]$type) > 1) {
      for (tp in contributors[[i]]$type[-1]) {
        cnr <- contributors[[i]]
        cnr$type <- tp
        contributors <- c(
          contributors,
          list(cnr)
        )
      }
      contributors[[i]]$type <- contributors[[i]]$type[1]
    }
  }


  # Create metadata list ----------------------------------------------------


  metadata <- list(
    title = title,
    community = community,
    upload_type = upload_type,
    authors = authors,
    description = description,
    version = version,
    language = language,
    keywords = keywords,
    access_right = access_right,
    embargo_date = embargo_date,
    license = license,
    contributors = contributors,
    grants = grants
  )

  class(metadata) <- append("metadata_bib", class(metadata))

  return(metadata)
}
