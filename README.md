
# MetaShARK
Metadata Shiny-Automated Resources & Knowledge
_First released on 15-04-2019_  
DOI: 10.5281/zenodo.3355324  

The aim of the MetaShARK app is to allow any user a bit familiar with ecology to fill in data packages to provide as many information as possible on any dataset used in a publication, communication, etc. in ecology. The challenge of this work is to produce a user-friendly tool to a science community which is not familiar to heavy metadata standards and their informatic specification. Consequently, the choice has been made to work only with R as it is the currentest programming language in this community and it can be easily accessible as an open source application, despite of its low performances.
This project has the ambition to offer the user a user-friendly alternative to existing tools (such as the hardcore Morpho ;) ) but also to address an other issue which is the EML is not always fully considered.

**Any suggestion is welcome, feel free to contact the dev !**

## Running MetaShARK
You can launch the app through the 'main.R' script (shinyApp/main.R). The supported features and expected behaviors are described below.

### Required libraries

MetaShARK require following libraries to perform:

**GUI libraries**
* [shiny](https://CRAN.R-project.org/package=shiny)
* [shinyTree](https://CRAN.R-project.org/package=shinyTree)
* [shinydashboard](https://CRAN.R-project.org/package=shinydashboard)
* [shinyjs](https://CRAN.R-project.org/package=shinyjs)
* [tcltk2](https://CRAN.R-project.org/package=tcltk2)
* [tippy](https://CRAN.R-project.org/package=tippy)

**Metadata libraries**
* [EML](https://CRAN.R-project.org/package=EML)
* [EMLassemblyline](https://github.com/EDIorg/EMLassemblyline)

**Utilitary libraries**
* [devtools](https://CRAN.R-project.org/package=devtools)
* [RefManageR](https://CRAN.R-project.org/package=RefManageR)

## Releases

### Release 20190918 - EML 2.2.0

With the release of EML v2.2.0 on 2019/09/13 by the NCEAS team, MetaShARK was updated to adapt to the new specification.

#### Features
* Guidelines files for Documentation were updated with EML 2.2.0 .xsd files, and the guidelines were reprocessed. Please note that NCEAS team decided to remove the documentation from high-level nodes (roots of each module). To remedy to this, the original documentation is now accessible through an external link. 
* The 'About' section is now filled with references written directly from .bib files. 

### Release 20190507

#### Features
* The tool still allows only the documentation browsing. The support has been moved from basic shiny style to the shiny Dashboard style. With this feature comes easier ways to organize the UI for future steps. 
* The whole repository architecture has been remade, which caused difficulties on uploading the latest version. 
* A .old directory has been created at the root to store old files which could still be useful later. Any documentation about them shall be contained within it as comments for scripts or data labels for other files.

### Release 20190415

#### Features
The tool allows to browse the documentation contained at any level of the EML XML schema (v. 2.1.1). The links to external sites have been implemented but the references between the modules are not. Content is based on the EML Schema provided through the [NCEAS eml repo](https://github.com/NCEAS/eml).

## Authors
* Elie Arnaud (developper) - elie.arnaud@mnhn.fr

## Contribute
Any contribution can be done and submitted. To contribute, please refer the 'contributing.md' file.

## Submit issues
For anny issue submittance, please add a single-word tag in bracket before the title of your issue. Do not hesitate also to describe it exhaustively and add a label.

