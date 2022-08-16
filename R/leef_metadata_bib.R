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
#'   - type
#'   - affiliation
#'   - orcid
#'
#' @return list containing the metadata necessary for the data deposit
#'
#' @md
#'
#' @export
#'
leef_metadata_bib <- function(
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
    access_right = "open",
    license = "CC-BY-SA-4.0",
    contributors = list(
      RMK = list(
        firstname = "Rainer M",
        lastname = "Krug",
        type = "DataManager",
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
    )
){
  metadata <- list(
    community = community,
    upload_type = upload_type,
    authors = authors,
    description = description,
    version = version,
    language = language,
    keywords = keywords,
    access_right = access_right,
    license = license,
    contributors = contributors
  )
  return(metadata)
}
