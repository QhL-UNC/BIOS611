FROM rocker/verse
RUN apt update && apt install -y man-db && rm -rf /var/lib/apt/lists/*
RUN yes|unminimize
RUN R -e "install.packages('table1', dependencies=TRUE)"
RUN R -e "install.packages('ggcorrplot', dependencies=TRUE)"

ARG USER_ID
RUN usermod -u $USER_ID rstudio && groupmod -g $GROUP_ID rstudio
RUN chown -R rstudio:rstudio /home/rstudio