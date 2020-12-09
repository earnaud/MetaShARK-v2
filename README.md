[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4297467.svg)](https://doi.org/10.5281/zenodo.4297467)

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-orange.svg)](https://www.tidyverse.org/lifecycle/#maturing)

<img src="./inst/media/logo.png" alt="" width="50%">

**Stable Server address:** 
http://metashark.pndb.fr/ ![Active](https://placehold.it/15/c5f015/000000?text=+)`Active`

**Stable Server address:** https://metashark.test.pndb.fr/  ![Maintenance](https://placehold.it/15/FF0000/000000?text=+) `Maintenance`

<!-- ![Active](https://placehold.it/15/c5f015/000000?text=+)`Active`  -->
<!-- ![Maintenance](https://placehold.it/15/FF0000/000000?text=+) `Maintenance` -->

Metadata Shiny-Automated Resources & Knowledge
_First released on 15-04-2019_  

The aim of the MetaShARK app is to allow any user a bit familiar with ecology to fill in data packages to provide as many information as possible on any dataset used in a publication, communication, etc. in ecology. The challenge of this work is to produce a user-friendly tool to a science community which is not familiar to heavy metadata standards and their informatic specification. Consequently, the choice has been made to work only with R as it is the currentest programming language in this community and it can be easily accessible as an open source application, despite of its low performances.
This project has the ambition to offer the user a user-friendly alternative to existing tools (such as the hardcore Morpho ;) ) but also to address an other issue which is the EML is not always fully considered.  
This MetaShARK git is called "v2" because it is the evolution with {golem} package of the previous [MetaShARK git](https://github.com/earnaud/MetaShARK)

MetaShARK has a dedicated [dockerhub](https://hub.docker.com/r/eliearnaud/metashark/dockerfile) and its deployment method is also [accessible](https://github.com/earnaud/MetaShARK_docker/).

**Any suggestion is welcome, feel free to contact the dev !**

## Installing MetaShARK

### Local installation

!!! DEPRECATED !!!

Refer to the `Dockerization` part for locale production use. For dev use, here are some information:

**If you are using local version, reinstall it regularly !** 

All dependencies are described in the DESCRIPTION file. You will also need to install the following system libraries, according to you OS:

| OS          | Debian-like          | Fedora, CentOS, RHEL | Solaris     | Mac OSX     |
|-------------|----------------------|----------------------|-------------|-------------|
| libcurl     | libcurl4-openssl-dev | libcurl-devel        | libcurl_dev | curl        |
| libxml-2.0  | libxml2-dev          | libxml2-devel        | libxml2_dev | libxml2     |
| openssl     | libssl-dev           | openssl-devel        | libssl_dev  | openssl@1.1 |
| libjq       | libjq-dev            | libjq-devel          | libjq_dev   | jq          |
| libv8       | libv8-dev            | v8-devel             | libv8_dev   | v8          |
| redland     | librdf0-dev          | redland-devel        | librdf_dev  | redland     |
| poppler-cpp | libpoppler-cpp-dev   | poppler-cpp-devel    | poppler_dev | poppler     |

You can install the app as follow (through command line, for Ubuntu):

```
apt -y update
apt -y upgrade 
apt install -y r-base
apt install -y libcurl4-openssl-dev libssh2-1-dev libssl-dev libxml2-dev # libgit2-dev 
R -e 'install.packages("devtools")'
apt install -y libv8-dev
R -e 'devtools::install_github("EDIorg/EMLassemblyline", ref="fix_41")'
apt install -y  libjq-dev librdf0-dev 
apt-get install -y libpoppler-cpp-dev
R -e 'install.packages(c("shinyBS","shinycssloaders","readtext"))'
# Stable: 
R -e 'devtools::install_github("earnaud/MetaShARK-v2", dependencies=TRUE)'
# Dev:
R -e 'devtools::install_github("earnaud/MetaShARK-v2", ref="dev", dependencies=TRUE)'
```

### Dockerization

MetaShARK is provided with two docker possibilities: one for locale setup (in this git) and one for server setup (at [this repository](https://github.com/earnaud/MetaShARK_docker)).

For the locale version, you can run a MetaShARK container with the following sequence (tested on Ubuntu). 

1. In a command shell, get to this git's root: `cd <path/to/MetaShARK>`.

2. Execute the following steps:
```
docker build -t metashark .
docker run -d --rm  -p 3838:3838  --name MS  -v "/home/$USER/dataPackagesOutput:/root/dataPackagesOutput"  metashark
```

3. In a web browser, type the following URL: `127.0.0.1:3838`.

## MetaShARK features

### Documentation

First feature developped in MetaShARK, it is possible to consult any documented part of the Ecological Metadata Language. This documentation directly relies on the EML 2.2.0, and some tags (as the "eml-\*" ones) can be undocumented. Also, it is possible to access the original documentation through the dedicated tab.

### Metadata filling

Two methods are being developped to fill in metadata:

* EML Assembly Line (EAL) : it is the EDI tool allowing the user to interact with a major part of the EML. MetaShARK is a user front-end solution to offer more automated and visual access to this tool.
* Metadata Fill-In (MetaFIN) : still not accessible, it is the PNDB specific tool exploring automatic inference to fill in metadata from datasets.

### Data Package upload

It is possibe to upload data packages to metacats registered in MetaShARK. You will need to fetch your metacat token in the corresponding MetaCatUI. During development phase, this might not be fully operable as we are trying to properly set our MetaCat and MetaShARK linked together.

### References

Some references are given that sustain the base principles of this work.

#### Features

MetaShARK/EAL supports:
* data package management (CC BY or CC 0 Licences)
* table files's description \*\* (and *only* those ones yet, badly. WIP on other file types)
* automated\* tables' attributes filling
* custom units' descriptions
* automated\* categorical variables description
* geographic, taxonomic and temporal coverages
* description of persons involved (possibly through ORCID)
* Metadata automated\* generation at EML format

\* automation still requires user's verification
\*\* see Known Bugs below

## Authors
* Elie Arnaud (developper) - elie.arnaud@mnhn.fr

## Contribute

### Code edit
Any contribution can be done and submitted. Care about documenting code chunks you want to edit, and also add motivations about these changes.

### Issues post
On posting issues, make sure your problem/idea has not been discussed before (beware of the "is closed" filter). Some issues are named with a "[XXX]" tag meaning this issue is a topic dedicated to "XXX": such issues are not meant to be closed before a long time.
