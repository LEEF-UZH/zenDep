#' Create list containing the bibligraphic metadata for the data deposit
#'
#' @param title title of the deposit
#' @param description description of the deposited data
#' @param subject `character` vector with keywords
#'   the deposited data should be added to,
#' @param contributor contributor of the deposited data. Each contributor is also a list
#'   with the following fields:
#'   - firstname
#'   - lastname
#'   - affiliation
#'   - orcid
#' @param language language of the deposited data
#' @param accessRights access right of the deposited data
#' @param embargo_data date until which the data is embargoed if `accessRights`
#'   is "embargoed"
#' @param license license of the deposited data
#' @param contributor list of contributors of the deposited data. Each
#'   contributor is also a list with the following fields:
#'   - firstname
#'   - lastname
#'   - type: the role. This `character` vector can have multiple values which will
#'     be expanded for submission to Zenodo
#'   - affiliation
#'   - orcid
#'
#' @return object of class `metadata_bib` and `list`. It is used by the function
#'   `add_metadata_bib()` to be added to the record which will be deposited to
#'   Zenodo.
#'
#' @md
#'
#' @export
#'
new_metadata_bib <- function(
    title = "Some Title",
    description = "Description of the data",
    subject = list(a = "LEEZ-UZH", b = "LEEF-1"),
    # community = "leef-uzh",
    # upload_type = "dataset",
    creator = list(
      RMK = list(
        name = "Rainer M Krug",
        affiliation = "University of Zurich",
        orcid = "0000-0002-7490-0066"
      ),
      OLP = list(
        name = "Owen L Petchey",
        affiliation = "University of Zurich",
        orcid = "0000-0002-7724-1633"
      )
    ),
    # version = "1.0.0",
    language = "eng",
    accessRights = "embargoed",
    # embargo_date = as.Date("2030-01-13"),
    license = "CC-BY-SA-4.0",
    contributor = list(
      RMK = list(
        name = "Rainer M Krug",
        type = c("DataManager", "Software"),
        affiliation = "University of Zurich",
        orcid = "0000-0002-7490-0066"
      ),
      OLP = list(
        name = "Owen L Petchey",
        type = "ProjectLeader",
        affiliation = "University of Zurich",
        orcid = "0000-0002-7724-1633"
      )
    ),
    # grants = "some names of the grant",
    isPartOf = NULL) {
  # Expand contributor when more than one type (role) is specified ---------


  lc <- length(contributor)
  for (i in 1:lc) {
    if (length(contributor[[i]]$type) > 1) {
      for (tp in contributor[[i]]$type[-1]) {
        cnr <- contributor[[i]]
        cnr$type <- tp
        contributor <- c(
          contributor,
          list(cnr)
        )
      }
      contributor[[i]]$type <- contributor[[i]]$type[1]
    }
  }


  # Create metadata list ----------------------------------------------------


  metadata <- list(
    title = title,
    # community = community,
    # upload_type = upload_type,
    creator = creator,
    description = description,
    # version = version,
    language = language,
    subject = subject,
    accessRights = accessRights,
    # embargo_date = embargo_date,
    license = license,
    contributor = contributor,
    # grants = grants,
    isPartOf = isPartOf
  )


  # Set class ---------------------------------------------------------------


  class(metadata) <- append("metadata_bib", class(metadata))


  # Return -----------------------------------------------------------------


  return(metadata)
}

#' Save metadata_bib object to yaml file
#'
#' A simple wrapper around `yaml::write_yaml()`
#' @param metadata_bib object of class `metadata_bib`
#' @param yamlfile name of the file
#'
#' @return result from `yaml::write_yaml()`
#'
#' @importFrom yaml write_yaml
#' @export
#'
#' @examples
save_metadata_bib <- function(metadata_bib, yamlfile) {
  if (!inherits(metadata_bib, "metadata_bib")) {
    stop("`metadata_bib` has to be an object of class `metadata_bib` as created by the function `new_metadata_bib()!")
  }

  return(yaml::write_yaml(metadata_bib, yamlfile))
}

#' Read metadata_bib object from yaml file
#'
#' A essentially simple wrapper around `yaml::write_yaml()`
#' @param metadata_bib object of class `metadata_bib`
#' @param yamlfile name of the file
#'
#' @return result from `yaml::write_yaml()`
#'
#' @importFrom yaml read_yaml
#' @export
#'
#' @examples
read_metadata_bib <- function(yamlfile) {
  metadata_bib <- yaml::read_yaml(yamlfile)
  class(metadata_bib) <- append("metadata_bib", class(metadata_bib))
  return(metadata_bib)
}
