[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4297467.svg)](https://doi.org/10.5281/zenodo.4297467)

[![lifecycle](https://img.shields.io/badge/lifecycle-maturing-orange.svg)](https://www.tidyverse.org/lifecycle/#maturing)

<img src="./inst/media/logo.png" alt="" width="50%">

**Stable Server address:** 
http://metashark.pndb.fr/ ![Maintenance](https://placehold.it/15/FF0000/000000?text=+) `Maintenance`

**Stable Test Server address:** 
https://metashark.test.pndb.fr/  ![Maintenance](https://placehold.it/15/FF0000/000000?text=+) `Maintenance`

<!-- ![Active](https://placehold.it/15/c5f015/000000?text=+)`Active`  -->
<!-- ![Maintenance](https://placehold.it/15/FF0000/000000?text=+) `Maintenance` -->

Metadata Shiny-Automated Resources & Knowledge
_First released on 15-04-2019_  

The aim of the MetaShARK app is to allow any user a bit familiar with ecology to fill in data packages to provide as many information as possible on any dataset used in a publication, communication, etc. in ecology. The challenge of this work is to produce a user-friendly tool to a science community which is not familiar to heavy metadata standards and their informatic specification. Consequently, the choice has been made to work only with R as it is the currentest programming language in this community and it can be easily accessible as an open source application, despite of its low performances.
This project has the ambition to offer the user a user-friendly alternative to existing tools (such as the hardcore Morpho ;) ) but also to address an other issue which is the EML is not always fully considered.  
This MetaShARK git is called "v2" because it is the evolution with {golem} package of the previous [MetaShARK git](https://github.com/earnaud/MetaShARK)

MetaShARK has a dedicated [dockerhub](https://hub.docker.com/r/eliearnaud/metashark/dockerfile) and its deployment method is also [accessible](https://github.com/earnaud/MetaShARK_docker/). You can also deploy MetaShARK in a local version: for this, check the `local installation` section.

## Legal disclaimers

By using MetaShARK on a machine, you can get to make the app write files on your machine (see `local installation` and docker items for more details). It is discouraged to launch MetaShARK outside of a docker container. 

**Any suggestion is welcome, feel free to contact the dev !**

## Installing MetaShARK

All instructed install solutions use Docker method. It is therefore required to install this software.

### Local installation

It is now possible to deploy a local container for MetaShARK. Please refer to the `build_docker_local.txt` to get all command lines necessary. 
**Disclaimer:** you can use docker volumes to access files outside of the container (steps 2 & 3a), but this will require to *write files to your computer*. Since you execute by yourself the given command lines, we consider this as a voluntary action from the user.

1. In a command shell, get to this git's root: `cd <path/to/MetaShARK>`.

2. If you want to use volumes (which will let you access files outside of the container, with admin rights), make sure to have a directory in which you can access the data. Here, we will use:
```
mkdir ~/metashark_data
```

3. Execute the following command lines:
  a. If you use volumes:
```
docker build -t metashark:local .
docker run -d --rm  -p 3838:3838  --name MS  -v ~/metashark_data:/root/dataPackagesOutput  metashark:local
```
  
  b. If you do not use volumes:
```
docker build -t metashark:local .
docker run -d --rm  -p 3838:3838  --name MS metashark:local
```

4. In a web browser, access the following URL: `127.0.0.1:3838`.


### Server installation

A dedicated [git](https://github.com/earnaud/MetaShARK_docker) shows the step for setting up an online instance of MetaShARK.
Online version of MetaShARK can be subject to some misfunctions due to incorrect handling of simultaneous multiple users (see issue #124).

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
