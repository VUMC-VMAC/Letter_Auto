FROM rocker/r-ver:4.0.3

LABEL org.opencontainer.image.licenses="GPL-2.0-or-later" \
      org.opencontainer.image.source="https://github.com/rocker-org/rocker-versioned2" \
      org.opencontainer.image.vendor="Rocker Project" \
      org.opencontainer.image.authors=""

ENV S6_VERSION=v1.21.7.0
ENV SHINY_SERVER_VERSION=latest
ENV PANDOC_VERSION=default

RUN /rocker_scripts/install_shiny_server.sh
RUN /rocker_scripts/install_tidyverse.sh
RUN Rscript -e "install.packages(c('shinydashboard','knitr','plotly','kableExtra','RColorBrewer','REDCapR','DiagrammeR','dplyr','flextable','ggpattern','ggplot2','Hmisc','officer','ploty','RCurl','reticulate','shiny','WordR','redcapAPI'))"

RUN sed -i -e 's/\blisten 3838\b/listen 8080/g' /etc/shiny-server/shiny-server.conf

EXPOSE 8080

CMD ["/init"]
