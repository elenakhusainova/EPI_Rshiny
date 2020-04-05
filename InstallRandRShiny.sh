## Get R
sudo apt-get update
sudo apt-get install r-base
sudo apt-get install r-base-dev

## Get the packages I need
wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.r
R CMD BATCH InstallPackages.R

## Get Shiny server
wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.4.807-amd64.deb
