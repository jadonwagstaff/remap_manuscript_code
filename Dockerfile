FROM rocker/r-base:4.2.1

RUN apt update && DEBIAN_FRONTEND=noninteractive
RUN apt-get update
RUN apt-get install -y libxml2-dev
RUN apt-get install -y libcurl4-openssl-dev
RUN apt-get install -y libssl-dev
RUN apt-get install -y libudunits2-dev
RUN apt-get install -y libgdal-dev

RUN R -e "install.packages('tidyverse', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('sf', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('raster', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('maps', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('mgcv', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('gstat', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('automap', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('remap', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('gridExtra', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('cowplot', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('nngeo', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"

RUN mkdir remap_manuscript_code
RUN cd remap_manuscript_code

ADD replication_code replication_code
COPY Dockerfile replication_code/Dockerfile
COPY LICENSE.md replication_code/LICENSE.md