## Get R
rversion='3.3.2-1xenial0'
sudo apt-get update
sudo apt-get install r-base=${rversion} r-recommended=${rversion} r-base-dev=${rversion} r-base-core=${rversion}

## Get the packages I need
wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.r
R CMD BATCH InstallPackages.R

## Get Shiny server
wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.4.807-amd64.deb
sudo dpkg -i shiny-server-1.4.4.807-amd64.deb
