#!/bin/sh


# ------------------------------------------------------------------------------
# ------- Preliminaries --------------------------------------------------------
# ------------------------------------------------------------------------------
debsource='deb http://cran.case.edu/bin/linux/ubuntu trusty/'
echo ${debsource} >> /etc/apt/sources.list
rversion='3.2.5-1trusty0'
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
apt-get update

echo "\n\n Preliminaries are done... \n\n"

# ------------------------------------------------------------------------------
# ------- Installing R ---------------------------------------------------------
# ------------------------------------------------------------------------------
echo "\n\n Installing R... \n\n"
apt-get -y --force-yes install r-base=${rversion} r-recommended=${rversion} r-base-dev=${rversion}
apt-get update
apt-get -y --force-yes install r-base-core=${rversion}

echo "\n\n R is installed! \n\n"


# ------------------------------------------------------------------------------
# ------- Installing Rshiny ----------------------------------------------------
# ------------------------------------------------------------------------------
echo "\n\n Installing RShiny... \n\n"
apt-get -y --force-yes install apache2
apt-get -y --force-yes install libcairo2-dev
apt-get -y --force-yes install libxt-dev
apt-get update
apt-get -y --force-yes install python-software-properties python g++ make
apt-get update
add-apt-repository ppa:chris-lea/node.js
apt-get update
apt-get -y --force-yes install nodejs
sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
apt-get install gdebi-core
wget https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-1.5.13.944-amd64.deb
gdebi shiny-server-1.5.13.944-amd64.deb

echo "\n\n RShiny is installed! \n\n"

# ------------------------------------------------------------------------------
# ------- Installing project-specific packages ---------------------------------
# ------------------------------------------------------------------------------
echo "\n\n Installing R packages... \n\n"
# To repair the problem with devtools:
# from https://github.com/r-lib/devtools/issues/2131 :
apt install build-essential libcurl4-gnutls-dev libxml2-dev libssl-dev

R -e 'install.packages("devtools", repos="http://cran.rstudio.com/")'
R -e 'remotes::install_version("dplyr", "0.8.3", repos="https://demo.rstudiopm.com/cran/__linux__/xenial/latest")'
R -e 'install.packages("rmarkdown", repos="http://cran.rstudio.com/")'

wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.R
R CMD BATCH InstallPackages.R        # Might take some time (~5min) 

echo "\n\n R packages are installed. \n\n"

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
# R -e 'rownames(installed.packages())' # To get the list of installed packages

# In case dplyr is needed and there are problems with its LinkingTo -- BH:
# From https://community.rstudio.com/t/install-packages-dplyr-fails-on-new-project/55787/3
# remotes::install_version('dplyr', '0.8.3', repos = 'https://demo.rstudiopm.com/cran/__linux__/xenial/latest')




# ------------------------------------------------------------------------------
# ------- Working code: dont change a thing! -----------------------------------
# ------------------------------------------------------------------------------

#debsource='deb http://cran.case.edu/bin/linux/ubuntu trusty/'
#rversion='3.2.5-1trusty0'
#echo ${debsource} >> /etc/apt/sources.list
#apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
#apt-get update
#apt-get -y --force-yes install r-base=${rversion} r-recommended=${rversion} r-base-dev=${rversion}
#apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
#apt-get update
#apt-get -y --force-yes install r-base=${rversion} r-recommended=${rversion} r-base-dev=${rversion}
#apt-get -y --force-yes install r-base-core=${rversion}
#apt-get -y --force-yes install apache2
#apt-get -y --force-yes install libcairo2-dev
#apt-get -y --force-yes install libxt-dev
#apt-get update
#apt-get -y --force-yes install python-software-properties python g++ make
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
#R - Rcpp -- there, devtools,remotes -- not, 
#R - install.packages(devtools), then install.packages(remotes) - works
#R - remotes::install_version('dplyr', '0.8.3', repos = 'https://demo.rstudiopm.com/cran/__linux__/xenial/latest') - works
#R - install.packages("ggvis", repos='https://cran.rstudio.com/') - works
#wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.R
#R CMD BATCH InstallPackages.R        # takes some time (~5min) - works
#R install.packages("rmarkdown", repos='https://cran.rstudio.com/')

# At this point everything works and the intro page should be seen

#cd ../../srv/shiny-server
#wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/app_red.R
#mv app_red.R app.R
#wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/MasterFile.csv
#wget https://github.com/elenakhusainova/EPI_Rshiny/raw/master/alldata_red.RData
#wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/master_variable_list.csv

# The app is live!



