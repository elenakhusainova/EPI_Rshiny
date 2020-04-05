#!/bin/sh

sudo apt-get update

## Get R
#rversion='3.4.4-1xenial0'
#sudo apt-get install r-base=${rversion} r-recommended=${rversion} r-base-dev=${rversion} r-base-core=${rversion}
sudo apt-get install r-base r-recommended r-base-dev r-base-core

## Get R packages I need
wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.r
R CMD BATCH InstallPackages.R

## Get Shiny server
wget https://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.4.4.807-amd64.deb
sudo dpkg -i shiny-server-1.4.4.807-amd64.deb
