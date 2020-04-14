#!/bin/sh


# ------------------------------------------------------------------------------
# ------- Preliminaries --------------------------------------------------------
# ------------------------------------------------------------------------------
debsource='deb http://cran.case.edu/bin/linux/ubuntu trusty/'
echo ${debsource} >> /etc/apt/sources.list
rversion='3.4.4-1trusty0'
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 51716619E084DAB9
apt-get update

echo "\n\n Preliminaries are done! \n\n"

# ------------------------------------------------------------------------------
# ------- Installing R ---------------------------------------------------------
# ------------------------------------------------------------------------------
echo "\n\n Installing R... \n\n"
apt-get --assume-yes install r-base=${rversion}
apt-get --assume-yes install r-base-dev=${rversion}
apt-get --assume-yes install r-recommended=${rversion}
apt-get --assume-yes install r-base-core=${rversion}
apt-get update

echo "\n\n R is installed! \n\n"

# ------------------------------------------------------------------------------
# ------- Installing RShiny ----------------------------------------------------
# ------------------------------------------------------------------------------
echo "\n\n Installing RShiny... \n\n"

# To repair the problem with devtools (and httpuv):
# from https://github.com/r-lib/devtools/issues/2131 :
apt-get install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev
apt-get update
R -e 'install.packages("shiny", repos="http://cran.rstudio.com")'

apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.13.944-amd64.deb
gdebi shiny-server-1.5.13.944-amd64.deb

R -e 'install.packages("devtools", repos="http://cran.rstudio.com/")'
R -e 'remotes::install_version("dplyr", "0.8.3", repos="https://demo.rstudiopm.com/cran/__linux__/xenial/latest")'
R -e 'install.packages("rmarkdown", repos="http://cran.rstudio.com/")'

echo "\n\n RShiny is installed! \n\n"

# ------------------------------------------------------------------------------
# ------- Installing project-specific packages ---------------------------------
# ------------------------------------------------------------------------------
echo "\n\n Installing R packages... \n\n"
wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.R
R CMD BATCH InstallPackages.R        # Might take some time (~5min)

echo "\n\n R packages are installed! \n\n"

# ------------------------------------------------------------------------------
# ------- Fetching the app -----------------------------------------------------
# ------------------------------------------------------------------------------
echo "\n\n Fetching the app...\n\n"
cd ../../srv/shiny-server
wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/app_red.R
mv app_red.R app.R
wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/MasterFile.csv
wget https://github.com/elenakhusainova/EPI_Rshiny/raw/master/alldata_red.RData
wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/master_variable_list.csv

echo "\n\n The app is live!\n\n"



# ------------------------------------------------------------------------------
# ------- Below are some drafts.  ----------------------------------------------
# ------------------------------------------------------------------------------

# ---- Useful for debugging / trouble-shooting --------
# R -e 'rownames(installed.packages())'      # To get the list of installed packages
# apt list --installed                       # list of installed packages
# apt-get purge --auto-remove packagename    # delete a package


# In case dplyr is needed and there are problems with its LinkingTo -- BH:
# From https://community.rstudio.com/t/install-packages-dplyr-fails-on-new-project/55787/3
# remotes::install_version('dplyr', '0.8.3', repos = 'https://demo.rstudiopm.com/cran/__linux__/xenial/latest')


