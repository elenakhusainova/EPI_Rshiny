#!/bin/sh

# Sources:
# - Jay
# - https://github.com/r-lib/devtools/issues/2131
# - https://rstudio.com/products/shiny/download-server/ubuntu/


# ----------------------------------------------------------------------------
# --------------- Preliminary installations ----------------------------------
# ----------------------------------------------------------------------------
apt-get update
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

echo "\n\nDoing LaTeX and Apache...\n\n"
apt-get -y --force-yes install apache2
apt-get -y --force-yes install libcairo2-dev
echo "\n\nFinished Apache, doing libxt, knitr, ...\n\n"
apt-get -y --force-yes install libxt-dev

apt-get update
apt-get -y --force-yes install python-software-properties python g++ make
add-apt-repository ppa:chris-lea/node.js
apt-get update
apt-get -y --force-yes install nodejs

echo "\n\nFinished preliminary, installing R...\n\n"

apt-get install gdebi-core

# To repair the problem with devtools:
# from https://github.com/r-lib/devtools/issues/2131 :
apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev

# ----------------------------------------------------------------------------
# --------------- Installing R -----------------------------------------------
# ----------------------------------------------------------------------------
apt-get update
debsource='deb http://cran.case.edu/bin/linux/ubuntu trusty/'
rversion='3.2.5-1trusty0'
echo ${debsource} >> /etc/apt/sources.list
# Get this and modify by hand for further package customization:
wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.R

apt-get -y --force-yes install r-base=${rversion} r-recommended=${rversion} r-base-dev=${rversion}
apt-get -y --force-yes install r-base-core=${rversion}

R CMD BATCH InstallPackages.R        # bigmemory, foreach, ... Takes some time (~15min)

echo "\n\nDoing Shiny...\n\n"
# Shiny:
su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
# From https://rstudio.com/products/shiny/download-server/ubuntu/  :
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.13.944-amd64.deb
gdebi shiny-server-1.5.13.944-amd64.deb

echo "\n\nDone!\n\n"



# -------------------------------------------
# ----- At this point everything worked. ----
# -------------------------------------------

# -------------------------------------------
# ----- Below are some drafts.  -------------
# -------------------------------------------


#apt-get update
# --- Preliminary installation ----------------------------------------------
#apt-get --assume-yes install libxt-dev
#apt-get --assume-yes install libcairo2-dev
#echo "\n\nInstalled libxt-dev and libcairo2-dev ...\n\n"

# ----- Getting R (instructions are from CRAN) -----------------------
# See: https://cran.rstudio.com/bin/linux/ubuntu/README.html
#echo "deb https://cloud.r-project.org/bin/linux/ubuntu trusty-cran35/" >> /etc/apt/sources.list
#apt-get update
#apt-get install r-base
#apt-get install r-base-dev
#deb http://mirror.vcu.edu/pub/gnu+linux/ubuntu/ bionic-backports main restricted universe 
#sudo apt-get build-dep r-cran-foo
#apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E298A3A825C0D65DFD57CBB651716619E084DAB9
#apt-get update
#apt-get upgrade
#apt-get install r-base-dev

#apt-get install gdebi-core
#wget https://download2.rstudio.org/server/trusty/amd64/rstudio-server-1.2.5033-amd64.deb
#gdebi rstudio-server-1.2.5033-amd64.deb
#echo "\n\nFollowed CRAN instructions here ...\n\n"

# ----- Getting RShiny (instructions are from CRAN) -------------------
# See: https://rstudio.com/products/shiny/download-server/ubuntu/
#su - -c "R -e \"install.packages('shiny', repos='https://cran.rstudio.com/')\""
# couple more times, then into R: install.packages("httpuv"), install.packages("devtools"), then 
# from here: https://github.com/r-lib/devtools/issues/2131
#apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
# then ran in R: install.packages("devtools") and it finally worked
# then again install.packages("httpuv"), doesn't work
# but from here: https://github.com/rstudio/shiny/issues/1971: devtools::install_version("httpuv", "1.3.5") works

# Stuck here
#apt-get update
#apt-get -y --force-yes install python-software-properties python g++ make
#add-apt-repository ppa:chris-lea/node.js
#apt-get update
#apt-get -y --force-yes install nodejs
#sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""

#apt-get install gdebi-core
#wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.13.944-amd64.deb
#gdebi shiny-server-1.5.13.944-amd64.deb

# -- Get R packages I need ---------
#wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.R
#R CMD BATCH InstallPackages.R
#echo "\n\nInstalled R packages ...\n\n"


# ---- Useful for debugging / trouble-shooting --------
# R -e 'rownames(installed.packages())' # To get the list of installed packages
# To upgrade R:
# echo "deb http://www.stats.bris.ac.uk/R/bin/linux/ubuntu precise/" >> /etc/apt/sources.list
# apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
# apt-get update
# apt-get upgrade
# R -e "install.packages('Rcpp', dependencies = TRUE, INSTALL_opts = '--no-lock')"

# In case dplyr is needed and there are problems with its LinkingTo -- BH:
# From https://community.rstudio.com/t/install-packages-dplyr-fails-on-new-project/55787/3
# remotes::install_version('dplyr', '0.8.3', repos = 'https://demo.rstudiopm.com/cran/__linux__/xenial/latest')

