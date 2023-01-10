
addMetadata <- function(
    rec = zen4R::ZenodoRecord$new(),
    metadata_bib = new_metadata_bib()
){
  rec$setTitle(metadata_bib$title)
  rec$addCommunity
  rec$setUploadType(metadata_bib$upload_type)
  lapply(
    metadata_bib$authors,
    function(aut){
      rec$addCreator(
        firstname = aut$firstname,
        lastname = aut$lastname,
        affiliation = aut$affiliation,
        orcid = aut$orcid
      )
    }
  )
  rec$setDescription(metadata_bib$description)
  rec$setVersion(metadata_bib$version)
  rec$setLanguage(metadata_bib$language)
  rec$setKeywords(metadata_bib$keywords)
  rec$setAccessRight(metadata_bib$access_right)
  rec$setEmbargoDate(metadata_bib$embargo_date)
  rec$setLicense(metadata_bib$license)
  # rec$setGrants("654359") # eLTER
  # rec$setGrants("871126") # eLTER-PPPZen
  # rec$setGrants("871128") # eLTER-Plus
  lapply(
    metadata_bib$contributors,
    function(con){
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
