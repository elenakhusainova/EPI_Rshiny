#!/bin/sh

# From Jay with some updates, worked on 04/09/2020
debsource='deb http://cran.case.edu/bin/linux/ubuntu trusty/'
rversion='3.2.5-1trusty0'
# Get this and modify by hand for further package customization:
wget http://www.stat.yale.edu/~jay/EC2/InstallPackages.R

echo ${debsource} >> /etc/apt/sources.list
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
apt-get update

echo "\n\nFinished update, installing R...\n\n"

apt-get -y --force-yes install r-base=${rversion} r-recommended=${rversion} r-base-dev=${rversion}
apt-get -y --force-yes install r-base-core=${rversion}

wget http://www.stat.yale.edu/~jay/EC2/InstallExtras.R
echo "\n\nFinished R, doing LaTeX and Apache...\n\n"
apt-get -y --force-yes install apache2
apt-get -y --force-yes install libcairo2-dev

echo "\n\nFinished Apache.\n\n"
echo "\n\nDoing libxt, knitr, ...\n\n"

apt-get -y --force-yes install libxt-dev
R CMD BATCH InstallExtras.R        # Rserve, FastRWeb, knitr, takes some time (~5min)
R CMD BATCH InstallPackages.R        # bigmemory, foreach, ... also takes some time (~5min)

echo "\n\nDoing Shiny and FastRWeb postinstallation.\n\n"

# Shiny:
apt-get update
apt-get -y --force-yes install python-software-properties python g++ make
add-apt-repository ppa:chris-lea/node.js
apt-get update
apt-get -y --force-yes install nodejs
sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
apt-get install gdebi-core

# From https://rstudio.com/products/shiny/download-server/ubuntu/  :
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.13.944-amd64.deb
gdebi shiny-server-1.5.13.944-amd64.deb





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
#wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.r
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

