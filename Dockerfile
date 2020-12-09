FROM rocker/shiny:3.6.3

RUN apt-get update --fix-missing -y \
    && apt-get install -y software-properties-common aptitude
RUN sudo add-apt-repository ppa:cran/libgit2
RUN aptitude install -y -f \
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
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("r-lib/remotes", ref = "97bbf81")'
RUN Rscript -e 'remotes::install_version("glue",upgrade="never", version = "1.4.2")'
RUN Rscript -e 'remotes::install_version("mime",upgrade="never", version = "0.9")'
RUN Rscript -e 'remotes::install_version("stringr",upgrade="never", version = "1.4.0")'
RUN Rscript -e 'remotes::install_version("markdown",upgrade="never", version = "1.1")'
RUN Rscript -e 'remotes::install_version("jsonlite",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("data.table",upgrade="never", version = "1.13.0")'
RUN Rscript -e 'remotes::install_version("RCurl",upgrade="never", version = "1.98-1.2")'
RUN Rscript -e 'remotes::install_version("xfun",upgrade="never", version = "0.15")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.4.4")'
RUN Rscript -e 'remotes::install_version("knitr",upgrade="never", version = "1.29")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.0")'
RUN Rscript -e 'remotes::install_version("fs",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.5.0")'
RUN Rscript -e 'remotes::install_version("datapack",upgrade="never", version = "1.3.2")'
RUN Rscript -e 'remotes::install_version("dplyr",upgrade="never", version = "1.0.2")'
RUN Rscript -e 'remotes::install_version("rmarkdown",upgrade="never", version = "2.3")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "2.3.2")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3")'
RUN Rscript -e 'remotes::install_version("gdata",upgrade="never", version = "2.18.0")'
RUN Rscript -e 'remotes::install_version("rorcid",upgrade="never", version = "0.6.4")'
RUN Rscript -e 'remotes::install_version("readtext",upgrade="never", version = "0.76")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("shinyBS",upgrade="never", version = "0.61")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("shinydashboardPlus",upgrade="never", version = "0.7.5")'
RUN Rscript -e 'remotes::install_version("shinyFeedback",upgrade="never", version = "0.3.0")'
RUN Rscript -e 'remotes::install_version("shinyFiles",upgrade="never", version = "0.8.0")'
RUN Rscript -e 'remotes::install_version("shinyTree",upgrade="never", version = "0.2.7")'
RUN Rscript -e 'remotes::install_version("shinyWidgets",upgrade="never", version = "0.5.4")'
RUN Rscript -e 'remotes::install_version("dataone",upgrade="never", version = "2.1.4")'
RUN Rscript -e 'remotes::install_version("RefManageR",upgrade="never", version = "1.2.12")'
RUN Rscript -e 'remotes::install_version("EML",upgrade="never", version = "2.0.3")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.15")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.2.1")'
RUN Rscript -e 'remotes::install_version("textutils",upgrade="never", version = "0.2-0")'
RUN Rscript -e 'remotes::install_version("tippy",upgrade="never", version = "0.0.1")'
RUN Rscript -e 'remotes::install_github("EDIorg/taxonomyCleanr@fc4d758c075d596013a18c1ae1e2cff4f5f8c2ae")'
RUN Rscript -e 'remotes::install_github("ThinkR-open/tagsinput@9aa70ec34c6fa60ef317446daef4cfaf3b682d1d")'
RUN Rscript -e 'remotes::install_github("ropenscilabs/emldown@8d98c8fc708dc1f2ecc8eec3d23a66e4f487e658")'
RUN Rscript -e 'remotes::install_github("earnaud/cedarr@dece7479cac3689e36283df74fe0514748e67f18")'
RUN Rscript -e 'remotes::install_github("trestletech/shinyAce@6f8f7c9976b44246e91bb5dbaef1b87d6bbb7b77")'
RUN Rscript -e 'remotes::install_github("EDIorg/EMLassemblyline@f05ef748feed7242c9eae0a794a98e163524dd48")'
RUN Rscript -e 'remotes::install_github("LukasK13/SummeRnote@7c404e1578ab3567fdb331716ca831913ccf645a")'

RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local("MetaShARK_0.0.0.9000.tar.gz", upgrade="never")'
EXPOSE 3838

CMD R -e "options('shiny.port'=3838,shiny.host='0.0.0.0');MetaShARK::runMetashark()"
