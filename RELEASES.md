# Previous releases of MetaShARK

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
