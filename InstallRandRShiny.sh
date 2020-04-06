#!/bin/sh

apt-get update
echo "\n\nUpdated apt-get ...\n\n"

# --- Preliminary installation ------------
apt-get --assume-yes install libxt-dev
apt-get --assume-yes install libcairo2-dev
echo "\n\nInstalled libxt-dev and libcairo2-dev ...\n\n"

# ----- Get R -----------------------
echo "deb http://cran.rstudio.com/bin/linux/ubuntu xenial/" >> /etc/apt/sources.list
echo "\n\nAdded CRAN -- ubuntu xenial to source.list ...\n\n"
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9 # Some magic here...
#rversion='3.4.4-1xenial0'
#apt-get install r-base=${rversion} r-recommended=${rversion} r-base-dev=${rversion} r-base-core=${rversion}
apt-get --assume-yes install r-base r-recommended r-base-dev r-base-core # -y, --yes, --assume-yes: Automatic yes to prompts
echo "\n\nInstalled R ...\n\n"

# -- Get R packages I need ---------
wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.r
R CMD BATCH InstallPackages.R
echo "\n\nInstalled R packages ...\n\n"

# ----- Get Shiny server -----------
apt-get --assume-yes install gdebi-core
sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.2.786-amd64.deb
gdebi shiny-server-1.4.2.786-amd64.deb
echo "\n\nInstalled R Shiny ...\n\n"

# ---- Useful for debugging / trouble-shooting --------
# R -e 'rownames(installed.packages())' # To get the list of installed packages
# To upgrade R:
# echo "deb http://www.stats.bris.ac.uk/R/bin/linux/ubuntu precise/" >> /etc/apt/sources.list
# apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
# apt-get update
# apt-get upgrade

