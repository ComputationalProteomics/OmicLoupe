FROM rocker/shiny:4.5.2 AS builder

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    libgit2-dev \
    pandoc \
    libfontconfig1-dev \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /build
COPY DESCRIPTION NAMESPACE ./
COPY R ./R
COPY man ./man
COPY inst ./inst

RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_deps(dependencies = TRUE)'

RUN R CMD INSTALL --no-multiarch --with-keep.source .

FROM rocker/shiny:4.5.2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4 \
    libxml2 \
    libssl3 \
    libgit2-1.7 \
    pandoc \
    libfontconfig1 \
    curl \
    && rm -rf /var/lib/apt/lists/*

COPY --from=builder /usr/local/lib/R/site-library /usr/local/lib/R/site-library

RUN echo 'OmicLoupe::runApp()' > /srv/shiny-server/app.R

RUN if id shiny >/dev/null 2>&1; then \
      if [ "$(id -u shiny)" -ne 999 ]; then \
        if getent passwd 999 >/dev/null 2>&1; then \
          userdel -r "$(getent passwd 999 | cut -d: -f1)" || true; \
        fi; \
        usermod -u 999 shiny; \
      fi; \
    else \
      useradd -u 999 -m -s /bin/bash shiny; \
    fi \
    && mkdir -p /home/data /data /home/data/omicloupe-handoff \
    && chown -R shiny:shiny /srv/shiny-server/ /var/lib/shiny-server/ /var/log/shiny-server/ /home/shiny /home/data /data

USER shiny

HEALTHCHECK --interval=30s --timeout=3s --start-period=10s --retries=3 \
  CMD curl -f http://localhost:3838/ || exit 1

EXPOSE 3838

CMD ["/usr/bin/shiny-server"]
