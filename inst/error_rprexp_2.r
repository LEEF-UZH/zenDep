library(deposits)

sessionInfo()

metadata <- structure(
    list(
        title = "LEEF-1 experiment",
        description = "Data sampled during the LEEF-1 experiment",
        abstract = "The data from the LEEF-1 experiment.",
        created = "2020-01-01",
        language = "eng", accessRights = "embargoed.",
        rightsHolder = "A person or organization owning or managing rights over the resource. Recommended practice is to refer to the rights holder with a URI. If this is not possible or feasible, a literal value that identifies the rights holder may be provided.",
        license = "CC-BY-SA-4.0",
        creator = list(list(
            name = "Rainer M. Krug",
            affiliation = "University of Zurich", orcid = "0000-0002-7490-0066"
        )),
        contributor = list(list(
            name = "Rainer M. Krug", affiliation = "University of Zurich",
            orcid = "0000-0002-7490-0066", type = "DataManager"
        )),
        isPartOf = list(list(
            identifier = "https://doi.org/10.1111/ele.14217",
            relation = "isPartOf"
        ))
    )
)

cli <- depositsClient$new(service = "zenodo", sandbox = TRUE)

cli$deposit_fill_metadata(metadata)
