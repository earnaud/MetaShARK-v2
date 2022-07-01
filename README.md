[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.5376278.svg)](https://doi.org/10.5281/zenodo.5376278)

[![docker](https://img.shields.io/docker/cloud/build/eliearnaud/metashark)](https://hub.docker.com/r/eliearnaud/metashark)

<img src="./inst/media/logo.png" alt="" width="50%">

**Stable Server address:** 
http://metashark.pndb.fr/ <b style="color:red"> Maintenance </b>

**Stable Test Server address:** 
https://metashark.test.pndb.fr/  <b style="color:green"> Active </b>

<!-- ![Active](https://placehold.it/15/c5f015/000000?text=+)`Active`  -->
<!-- ![Maintenance](https://placehold.it/15/FF0000/000000?text=+) `Maintenance` -->

Metadata Shiny Application for Resources & Knowledge
_First released on 15-04-2019_  

The aim of **MetaShARK** is to allow any user a bit familiar with ecology to fill in data packages to provide as many information as possible on any dataset used in a publication, communication, etc. in ecology. The challenge of this work is to produce a user-friendly tool to a science community which is not familiar to heavy metadata standards and their informatic specification. Consequently, the choice has been made to work only with R as it is the current programming language in this community and it can be easily accessible as an open source application, despite of its low performances.
This project has the ambition to offer the public a user-friendly alternative to existing tools (such as the hardcore Morpho ;) ) but also to address an other issue which is the EML is not always fully considered.

This MetaShARK git is called "v2" because it is the evolution with {golem} package of the previous [MetaShARK git](https://github.com/earnaud/MetaShARK)

MetaShARK has a dedicated [dockerhub](https://hub.docker.com/r/eliearnaud/metashark/dockerfile). You can also deploy MetaShARK in a local version: for this, check the `local installation` section.

## Legal disclaimers

By using MetaShARK on a personal machine, you can get to make the app write files on your machine (see `local installation` and docker items for more details). It is discouraged to launch MetaShARK outside of a docker container. 

**Any suggestion is welcome, feel free to contact the dev !**

## Installing MetaShARK

All instructed install solutions use Docker method. It is therefore required to install this software.

If this is "easy" on linux, for Windows 10 user, the easiest way is to install first Docker Desktop for windows from this url https://hub.docker.com/editions/community/docker-ce-desktop-windows
This will install a linux kernel on your windows so you can use a shell, like PowerShell, to run linux command lines.
Once "docker Desktop" is installed, you have to run it so Docker is activated, then you can run Docker command on PowerShell.

### Local installation

It is now possible to deploy a local container for MetaShARK. Check for the whole repository [here](https://hub.docker.com/r/eliearnaud/metashark).

Two possibilities :

#### Using the public Docker image (recommended)

Typing this command line on a shell:

On linux computer:
``` docker run -d --rm  -p 3838:3838  --name MS  -v ~/metashark_data:/root/dataPackagesOutput  eliearnaud/metashark ```

On windows computer (using Powershell on Windows 10 for example and if you are in a folder where there is or you accept the creation of a `metashark_data` folder):
``` docker run -d --rm  -p 3838:3838  --name MS  -v ${PWD}\metashark_data:/root/dataPackagesOutput  eliearnaud/metashark ```

#### Using this git Dockerfile

By downloading the git in locale following the described steps. Please refer to the `reload_docker.sh` script to get all necessary command lines. 
**Disclaimer:** you can use docker volumes to access files outside of the container (steps 3 & 4a), but this will require to *write files to your computer*. Since you execute by yourself the given command lines, we consider this as a consent from the user.

1. Make sure to have built the package as an archive. To achieve this, in an R or RStudio console:
```
# in the root of the package git
> devtools::build(path=".")
```

You shall get a file like `MetaShARK_*.tar.gz` (replace `*` with the current version number).

2. In a command shell, get to this git's root: `cd <path/to/MetaShARK/git>`.

3. If you want to use volumes (which will let you access files outside of the container, with admin rights), make sure to have a directory in which you can access the data. Here, we will use:
```
mkdir ~/metashark_data
```

4. Execute the following command lines to set up the container:
  a. If you use volumes:
```
# build the MetaShARK image
docker build -t metashark:local .
# run the app using the image
docker run -d --rm  -p 3838:3838  --name MS  -v ~/metashark_data:/root/dataPackagesOutput  metashark:local
```
  
  b. If you do not use volumes:
```
# build the MetaShARK image
docker build -t metashark:local .
# run the app using the image
docker run -d --rm  -p 3838:3838  --name MS metashark:local
```

5. In a web browser, access the following URL: `127.0.0.1:3838`.

## MetaShARK features

### Documentation

First feature developped in MetaShARK, it is possible to consult any documented part of the Ecological Metadata Language. This documentation directly relies on the EML 2.2.0, and some tags (as the "eml-\*" ones) can be undocumented. Also, it is possible to access the original documentation through the dedicated tab.

### Metadata filling

Two methods are being developped to fill in metadata:

* EML Assembly Line (EAL) : it is the EDI tool allowing the user to interact with a major part of the EML. MetaShARK is a user front-end solution to offer more automated and visual access to this tool.
* Metadata Fill-In (MetaFIN) : in active development, it is a complementary tool to edit and annotate EML files. 

### Data Package upload

It is possible to upload data packages to metacats registered in MetaShARK. You will need to fetch your metacat token in the corresponding MetaCatUI. Refer to the `upload` part of the app to get guided into this.

### References

Given references were used as base principles of our work.

#### Features

Features are emphasized into the NEWS.md file.

## Authors

* Elie Arnaud (developper) - elie.arnaud@mnhn.fr

## Contribute

### Code edit

Any contribution can be done and submitted. Care about documenting code chunks you want to edit, and also add motivations about these changes.

#### Naming conventions

CONSTANTS: items who will store never-changing values
theFunctions: items who will take other as arguments
the_variables: items who will store changing or changeable values
.xxx: any item (function or variable) defined and used inside a code block

### Issues post

On posting issues, make sure your problem/idea has not been discussed before (beware of the "is closed" filter).
