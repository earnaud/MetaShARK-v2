# v1.6.0

## Features

* Fixed DataOne nodes list

* Increased file input limit to 2 Go

* Added this NEWS file.

* Upgrade to R4.1.0: removed {emldown} dependency for compatibility motives.

* Setup {shinytest} tests functionnal. 

* Fixed unit load on Attributes.

* Fixed better UI for method selection in geographic coverage step.

* Fixed marked ORCID input as facultative in personnel step.

* Fixed revised unit list.

* Fixed revised custom unit list.

## Work In progress

* Tests: set a complete set of tests. 

* Upload: make data packages be fully uploaded.

* Annotations: continue `dev-metafin` branch.

# v1.5.3

* NOTE: two tags were released because of a non-updated archive in 1.5.2.

* stable version (v1.5.1-stable).

* MetaFIN: add xml handling utils.

* Fixed Categorical variables fails to save.

* Fixed Taxonomic Coverage templating on "previous".

* Fixed Corrected one-file handling.

# v1.5.1 : Spring update 

* Set file upload max size to 50 Mb.

* Added {profvis} support.

* Fixed checks for categorical variables templating

* Fixed Taxonocmic Coverage initialization

* Fixed attributes tree, now starts opened

* Fixed attributes & categorical variables trees renders

# v1.5 : Winter Production 

* Added interest ontologies list as resource

* Added a way to use MetaShARK in locale with Docker.

* Initialized MetaFIN.

* Fixed attributes. Other impacted steps have been edited

* Fixed missing value detection.

# v1.4 : Summer production 

* Started tests support with {shinytest} - not achieved

* Improved excel files support: no support further than the first sheet.

* R/ files were renamed.

* Save method improved

* ORCID support (miscellaneous module)

* First step with annotations (see {cedarr})

* WIP mode (display WIP features or not)

* variables refactored around `main.env` reactive values

* support for shiny 1.5

* Added log messages

* Added colorized feedbacks

* Added attributes content preview

* Fixed xcel support: multi-sheet support

* Fixed latitude/longitude input + next/previous unexpected behavior (moved more than one step)

* Fixed app freeze on launch

* Fixed zip download in DP selection

* Fixed upload module and registered endpoints

# v1.3 : Good gui MetaShARK

* Added emldown generation

* Added quick mode to populate inputs upon coming on a module

* Added LICENSE

* Added helpers to each Fill-In module

* Set save method to JSON format

* Fixed upload module

# v1.2 : EML Assembly Line Update - beta-test version

* Changed files organizations for best practices compliance

* Dockerization of the app (https://hub.docker.com/repository/docker/eliearnaud/metashark)

* Revised workflow

* Download data packages from Select Data Packages page.

* Added dev server address (used with OpenGenOuest, no more in use)

* Added markdown support for Miscellaneous step

* Prettified modules organization

* Published in Zenodo

* Fixed ShinyFiles/Shiny correction according to local/server usage

* Fixed image display without using 'renderImage()' method

* Fixed make `make_eml()`.

# 1.1 : EML Assembly Line Update - alpha version

* added CSS styles

* code documentation has been set with {roxygen} syntax

* upload module is ready (some part is still in dev)

* EML-ready data packages can be uploaded, with conditions (soon fixed):only text/csv, files, no scripts, only on PNDB metacat

* Attributes module finished (with custom units)

* Categorical Variables are being worked

* Added Geographic Coverage facultative module with2 methods: columns selection (choose latitude and longitude columns among your dataset) and custom edition (describe each site one by one if you don't have yet a file).

* module CustomUnits is functional

* Update GO FAIR bibtex ref as a package internal data

* Fixed troubles accessing package data

## Initial git setup 

* Reused git parts from archived earnaud/MetaShARK

* Organized code and internal data to be used as an R package

* Set `dev` buttons. Run: `runMetashark(dev = TRUE)` to get access to dev buttons (allowing to browse in middle of the code). dev=FALSE is to use for presentation purposes but is set as default.

* Modified way to install MetaShARK
