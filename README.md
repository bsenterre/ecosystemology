# ecosystemology
Management and use of ecosystem taxonomic data from GBIF and Earth Engine into a shiny app to assist Red Listing of Ecosystems and KBA assessments
I am new to GitHub and code writing, but at this point I managed to develop an implementation of the conceptual approach to defining ecosystem types, as described in my paper https://doi.org/10.1016/j.ecocom.2021.100945
It is based on publishing raw ecosystem taxonomic and occurrence data on Zenodo (https://doi.org/10.5281/zenodo.7812549; initially, in version 1.0, it was published on GBIF, https://cloud.gbif.org/africa/resource?r=ecosystemology) and ecosystem distribution spatial objects on Earth Engine (https://code.earthengine.google.com/?accept_repo=users/bsenterre/ecosystemology).
These data are then manipulated with R to produce output files ready to be used in a shiny app designed to explore ecosystem taxonomy and distribution (see https://shiny.bio.gov.sc/bioflora/).
