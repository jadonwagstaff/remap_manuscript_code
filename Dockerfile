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
RUN R -e "install.packages('automap', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('remap', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('gridExtra', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('cowplot', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"
RUN R -e "install.packages('nngeo', dependencies = TRUE, repos = 'http://cran.rstudio.com/')"

RUN mkdir remap_manuscript_code
RUN cd remap_manuscript_code

ADD tables remap_manuscript_code/tables
ADD figures remap_manuscript_code/figures
ADD data remap_manuscript_code/data
ADD code_example remap_manuscript_code/code_example
COPY Dockerfile remap_manuscript_code
COPY LICENSE.md remap_manuscript_code
COPY README.md remap_manuscript_code