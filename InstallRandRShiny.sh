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
R CMD BATCH InstallExtras.R        # Rserve, FastRWeb, knitr

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

# To repair the problem with devtools:
# from https://github.com/r-lib/devtools/issues/2131 :
apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev

R CMD BATCH InstallPackages.R        # bigmemory, foreach, ... also takes some time (~5min)

# -------------------------------------------
# ----- At this point everything worked. ----
# -------------------------------------------

# -------------------------------------------
# ----- Below are some drafts.  -------------
# -------------------------------------------

# ---- Useful for debugging / trouble-shooting --------
# R -e 'rownames(installed.packages())' # To get the list of installed packages

# In case dplyr is needed and there are problems with its LinkingTo -- BH:
# From https://community.rstudio.com/t/install-packages-dplyr-fails-on-new-project/55787/3
# remotes::install_version('dplyr', '0.8.3', repos = 'https://demo.rstudiopm.com/cran/__linux__/xenial/latest')
