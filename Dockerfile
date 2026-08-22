FROM rocker/r-ver:4.4.1

# Posit Package Manager: prebuilt Linux binaries for CRAN packages, no compiling.
RUN echo 'options(repos = c(PPM = "https://packagemanager.posit.co/cran/__linux__/jammy/latest", CRAN = "https://cloud.r-project.org"))' >> /usr/local/lib/R/etc/Rprofile.site

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libuv1 \
    curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY setup.R ./setup.R
RUN Rscript -e 'pkgs <- c("ellmer","querychat","shiny","bslib","DT","dplyr","readr","ggplot2","plotly","scales","DBI","duckdb","broom","rsample","yardstick","parsnip","recipes","workflows","glmnet","ranger","rpart.plot","rpart","mgcv","nnet"); install.packages(pkgs)'

COPY app.R ./app.R
COPY R ./R
COPY greeting.md ml-greeting.md extra-instructions.md ml-instructions.md ./

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp(host = '0.0.0.0', port = 3838)"]
