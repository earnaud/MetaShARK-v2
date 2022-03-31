FROM rocker/shiny:4.1.0

LABEL version="1.7.1"

RUN apt update --fix-missing -y \
    && apt upgrade -y \
    && apt-get install -y software-properties-common aptitude
RUN add-apt-repository ppa:cran/libgit2
RUN aptitude install -y -f -o APT::Get::Fix-Missing=true \
	default-jre-headless \
	git-core \
	libcurl4-openssl-dev \
	libgit2-dev \
	libjq-dev \
	libpoppler-cpp-dev \
	librdf0-dev \
	libraptor2-dev \
	librasqal3-dev \
	libssh2-1-dev \
	libssl-dev \
	libv8-dev \
	libxml2-dev \
	make pandoc \
	pandoc-citeproc \
	libjpeg-dev \
	zlib1g-dev \
	cargo \
	libmagick++-dev \
	libharfbuzz-dev \
	libfribidi-dev \
	mesa-common-dev \
	libudunits2-dev \
	libgdal-dev \
  && apt-get clean \ 
  && rm -rf /var/lib/apt/lists/*

RUN mkdir /build_zone && mkdir -p /dataPackagesOutput/emlassemblyline
ADD . /build_zone
WORKDIR /build_zone

RUN dpkg -S /usr/include/GL/gl.h \
&& echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site \
&& R -e 'install.packages("devtools")' \
&& R -e 'devtools::install_version("remotes", version = "2.3.0")'
RUN R -e 'remotes::install_github("EDIorg/EMLassemblyline@d3d2eb90a67370c1138e234c09dd96c529408f90")' 
RUN Rscript -e 'remotes::install_github("earnaud/SummeRnote")'


RUN  R -e 'remotes::install_version("RCurl",upgrade="never", version = "1.98-1.3")'\
&& R -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")' \
&& R -e 'remotes::install_version("datapack",upgrade="never", version = "1.4.0")' \
&& R -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")' \
&& R -e 'remotes::install_version("gdata",upgrade="never", version = "2.18.0")' \
&& R -e 'remotes::install_version("rorcid",upgrade="never", version = "0.7.0")' \
&& R -e 'remotes::install_version("readtext",upgrade="never", version = "0.80")' \
&& R -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")' \
&& R -e 'remotes::install_version("shinyBS",upgrade="never", version = "0.61")' \
&& R -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.0.0")' \
&& R -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")' \
&& R -e 'remotes::install_version("shinydashboardPlus",upgrade="never", version = "2.0.1")' \
&& R -e 'remotes::install_version("shinyFeedback",upgrade="never", version = "0.3.0")' \
&& R -e 'remotes::install_version("shinyFiles",upgrade="never", version = "0.9.0")' \
&& R -e 'remotes::install_version("shinyTree",upgrade="never", version = "0.2.7")' \
&& R -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.6.0")' \
&& R -e 'remotes::install_version("dataone",upgrade="never", version = "2.2.1")' \
&& R -e 'remotes::install_version("RefManageR",upgrade="never", version = "1.3.0")' \
&& R -e 'remotes::install_version("DT",upgrade="never", version = "0.18")' \
&& R -e 'remotes::install_version("golem",upgrade="never", version = "0.3.1")' \
&& R -e 'remotes::install_version("textutils",upgrade="never", version = "0.2-1")' \
&& R -e 'remotes::install_version("tippy",upgrade="never", version = "0.1.0")' \
&& R -e 'remotes::install_version("shinybusy",upgrade="never", version = "0.2.2")' \
&& R -e 'remotes::install_version("units",upgrade="never", version = "0.7-2")'\
&& R -e 'remotes::install_version("sf",upgrade="never", version = "1.0-5")' \
&& R -e 'remotes::install_version("leaflet",upgrade="never", version = "2.0.4.1")' \
&& R -e 'remotes::install_version("leaflet.extras",upgrade="never", version = "1.0.0")' \
&& R -e 'remotes::install_version("colourpicker",upgrade="never", version = "1.1.1")' 


RUN R -e 'remotes::install_local(dir(".", pattern = "MetaShARK_.*\\.tar.gz"), upgrade="never")'


EXPOSE 3838

CMD R -e "options('shiny.port'=3838,'shiny.host'='0.0.0.0'); MetaShARK::runMetashark(dev=FALSE)"