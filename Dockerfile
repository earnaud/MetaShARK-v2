FROM rocker/shiny:4.1.0

LABEL version="1.6.1"

RUN apt update --fix-missing -y \
    && apt upgrade -y \
    && apt-get install -y software-properties-common aptitude
RUN sudo add-apt-repository ppa:cran/libgit2
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
  && apt-get clean \ 
  && rm -rf /var/lib/apt/lists/*

RUN dpkg -S /usr/include/GL/gl.h
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("devtools")'
RUN R -e 'devtools::install_version("remotes", version = "2.3.0")'
RUN Rscript -e 'remotes::install_version("bibtex", upgrade="never", version = "0.4.2.3")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("mime",upgrade="never", version = "0.11")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("markdown",upgrade="never", version = "1.1")'
RUN Rscript -e 'remotes::install_version("jsonlite",upgrade="never", version = "1.7.2")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.14.0")'
RUN Rscript -e 'remotes::install_version("RCurl",upgrade="never", version = "1.98-1.3")'
# RUN Rscript -e 'remotes::install_version("xfun",upgrade="never", version = "0.24")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.5.2")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.33")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.1.1")'
RUN Rscript -e 'remotes::install_version("fs",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.6.0")'
RUN Rscript -e 'remotes::install_version("datapack",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.7")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.9")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.0.4")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("gdata",upgrade="never", version = "2.18.0")'
RUN Rscript -e 'remotes::install_version("rorcid",upgrade="never", version = "0.7.0")'
RUN Rscript -e 'remotes::install_version("readtext",upgrade="never", version = "0.80")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("shinyBS",upgrade="never", version = "0.61")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("shinydashboardPlus",upgrade="never", version = "2.0.1")'
RUN Rscript -e 'remotes::install_version("shinyFeedback",upgrade="never", version = "0.3.0")'
RUN Rscript -e 'remotes::install_version("shinyFiles",upgrade="never", version = "0.9.0")'
RUN Rscript -e 'remotes::install_version("shinyTree",upgrade="never", version = "0.2.7")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.6.0")'
RUN Rscript -e 'remotes::install_version("dataone",upgrade="never", version = "2.2.1")'
RUN Rscript -e 'remotes::install_version("RefManageR",upgrade="never", version = "1.3.0")'
RUN Rscript -e 'remotes::install_version("EML",upgrade="never", version = "2.0.5")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.18")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("textutils",upgrade="never", version = "0.2-1")'
RUN Rscript -e 'remotes::install_version("tippy",upgrade="never", version = "0.1.0")'
RUN Rscript -e 'remotes::install_version("shinybusy",upgrade="never", version = "0.2.2")'
RUN Rscript -e 'remotes::install_github("EDIorg/EMLassemblyline@ad838f970ee259eda7f96d6a0bbe7f9fa509c66e")'
RUN Rscript -e 'remotes::install_github("EDIorg/taxonomyCleanr@ea60ad94979ada8c68808f3c3e3d1fbbbd4855b3")'
RUN Rscript -e 'remotes::install_github("LukasK13/SummeRnote@7c404e1578ab3567fdb331716ca831913ccf645a")'

# RUN Rscript -e 'remotes::install_github("ThinkR-open/tagsinput@9aa70ec34c6fa60ef317446daef4cfaf3b682d1d")'
# RUN Rscript -e 'remotes::install_github("ropenscilabs/emldown@8d98c8fc708dc1f2ecc8eec3d23a66e4f487e658")'
# RUN Rscript -e 'remotes::install_github("earnaud/cedarr@dece7479cac3689e36283df74fe0514748e67f18")'
# RUN Rscript -e 'remotes::install_github("trestletech/shinyAce@6f8f7c9976b44246e91bb5dbaef1b87d6bbb7b77")'

RUN mkdir /build_zone
RUN mkdir -p /dataPackagesOutput/emlassemblyline
RUN echo ls ~
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(\
  dir(".", pattern = "MetaShARK_.*\\.tar.gz"),\
  upgrade="never"\
)'
EXPOSE 3838

CMD R -e "options('shiny.port'=3838,'shiny.host'='0.0.0.0'); MetaShARK::runMetashark(dev=FALSE)"
