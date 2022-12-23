FROM rocker/shiny:4.2.0

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libxml2-dev libssl-dev libgit2-dev pandoc libfontconfig1-dev


RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("ComputationalProteomics/OmicLoupe")'

RUN echo 'OmicLoupe::runApp()' > app.R

RUN mv app.R /srv/shiny-server/

CMD ["/usr/bin/shiny-server"]