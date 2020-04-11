#!/bin/sh

# ------------------------------------------------------------------------------
# ------- Preliminary installations --------------------------------------------
# ------------------------------------------------------------------------------

apt-get update
echo "\n\nDoing LaTeX and Apache...\n\n"
apt-get -y --force-yes install apache2
apt-get -y --force-yes install libcairo2-dev
echo "\n\nFinished Apache.\n\n"

echo "\n\nDoing libxt, knitr, ...\n\n"
apt-get -y --force-yes install libxt-dev

apt-get update
apt-get -y --force-yes install python-software-properties python g++ make
add-apt-repository ppa:chris-lea/node.js
apt-get update
apt-get -y --force-yes install nodejs

# Tool to instal .deb files:
apt-get install gdebi-core

# To repair the problem with devtools:
# from https://github.com/r-lib/devtools/issues/2131 :
apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
apt-get update
echo "\n\nFinished update\n\n"

# From https://cran.r-project.org/bin/linux/ubuntu/README :
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# ------------------------------------------------------------------------------
# ------- Installing R ---------------------------------------------------------
# ------------------------------------------------------------------------------
debsource='deb http://cran.case.edu/bin/linux/ubuntu trusty/'
rversion='3.2.5-1trusty0'
# Get this and modify by hand for further package customization:
wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.R
echo ${debsource} >> /etc/apt/sources.list

echo "\n\nInstalling R...\n\n"
apt-get update
apt-get -y --force-yes install r-base=${rversion} r-recommended=${rversion} r-base-dev=${rversion}
apt-get -y --force-yes install r-base-core=${rversion}


# ------------------------------------------------------------------------------
# ------- Installing Shiny -----------------------------------------------------
# ------------------------------------------------------------------------------
echo "\n\nDoing Shiny.\n\n"
# Shiny:
sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
# From https://rstudio.com/products/shiny/download-server/ubuntu/  :
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.13.944-amd64.deb
gdebi shiny-server-1.5.13.944-amd64.deb

echo "\n\nShiny and Shiny server are installed! Installing R packages, might take a while...\n\n"

R CMD BATCH InstallPackages.R 

# ------------------------------------------------------------------------------
# ------- Below are some drafts.  ----------------------------------------------
# ------------------------------------------------------------------------------

# ---- Useful for debugging / trouble-shooting --------
# R -e 'rownames(installed.packages())' # To get the list of installed packages

# In case dplyr is needed and there are problems with its LinkingTo -- BH:
# From https://community.rstudio.com/t/install-packages-dplyr-fails-on-new-project/55787/3
# remotes::install_version('dplyr', '0.8.3', repos = 'https://demo.rstudiopm.com/cran/__linux__/xenial/latest')




# ------------------------------------------------------------------------------
# ------- Working code: --------------------------------------------------------
# ------------------------------------------------------------------------------

#debsource='deb http://cran.case.edu/bin/linux/ubuntu trusty/'
#rversion='3.2.5-1trusty0'
#wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.R
#echo ${debsource} >> /etc/apt/sources.list
#apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
#apt-get update
#apt-get -y --force-yes install r-base=${rversion} r-recommended=${rversion} r-base-dev=${rversion}
#apt-get -y --force-yes install r-base-core=${rversion}
#wget http://www.stat.yale.edu/~jay/EC2/InstallExtras.R
#apt-get -y --force-yes install apache2
#apt-get -y --force-yes install libcairo2-dev
#apt-get -y --force-yes install libxt-dev
#R CMD BATCH InstallExtras.R        # Rserve, FastRWeb, knitr
#apt-get update
#apt-get -y --force-yes install python-software-properties python g++ make
#add-apt-repository ppa:chris-lea/node.js
#apt-get update
#apt-get -y --force-yes install nodejs
#sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
#apt-get install gdebi-core
#wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.13.944-amd64.deb
#gdebi shiny-server-1.5.13.944-amd64.deb
#apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
#R CMD BATCH InstallPackages.R        # bigmemory, foreach, ... also takes some time (~5min)




