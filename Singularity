BootStrap: docker
From: ubuntu:18.04

# Build:  sudo singularity build out.simg Singularity
# Build:  sudo writable: singularity build --writable out.sim Singularity

# Update: 200713

%post
    R_BASE_VERSION="3.6.3"

    # Setup system packages
    apt-get -qq update
    apt-get upgrade -y
    apt-get install -y \
        apt-transport-https \
        gnupg \
        ca-certificates \
        libc6 \
        libcurl4-openssl-dev \
        libxml2-dev \
        libfftw3-dev \
        git \
        wget \
        zip \
        libssl-dev \
        vim-tiny \
        libglu1-mesa-dev \
        locales \
        locales-all \
        libudunits2-dev
 
    locale-gen en_US.UTF-8
    export LANG=en_US.UTF-8
    export LANGUAGE=en_US.UTF-8
    export LC_ALL=en_US.UTF-8
    export LC_MONETARY=en_US.UTF-8
    export LC_PAPER=en_US.UTF-8
    export LC_MEASUREMENT=en_US.UTF-8
    export LC_TIME=en_US.UTF-8

    # Setup R repository
    echo "deb https://cloud.r-project.org/bin/linux/ubuntu bionic-cran35/" >> /etc/apt/sources.list
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 51716619E084DAB9 
    # apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
    apt-get -qq update
    apt-get upgrade -y

    # Install R
    apt-get install -y --no-install-recommends \
    littler \
    r-base-core=${R_BASE_DEV}* \
    r-base-dev=${R_BASE_DEV}*
 
    # Prepare R repositories
    echo 'options(repos = c(CRAN = "https://cran.rstudio.com/"), download.file.method = "libcurl")' >> /etc/R/Rprofile.site
    echo 'source("/etc/R/Rprofile.site")' >> /etc/littler.r
    # Rscript /etc/little.r

    echo 'devtools::install_github("ComputationalProteomics/OmicLoupe")' >> install_omicloupe.R
    # Rscript install_omicloupe.R

    echo 'OmicLoupe::runApp()' > run.R

%runscript
    exec Rscript /run.R
