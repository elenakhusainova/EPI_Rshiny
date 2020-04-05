#!/bin/sh
##
## InstallR.sh for use on Amazon EC2
##
## - Jay Emerson and Susan Wang, originally May 2013
## - Added "doextras" for Apache, Rserve/FastRWeb, Shiny server (JE June 2013)
## - Revised for new EC2 and software (JE August 2014)
## - Revised for new EC2 (FastRWeb and rserve commented out, JE April 2016)
## - Changes required for shiny installation (JE April 2016)
##
## -------------------------
## To log in the first time:
##
## NOTE: HOSTNAME might also be the host IP address!
##
## ssh -i ~/.ssh/jaykey.pem ubuntu@HOSTNAME
##
## sudo su
## wget http://www.stat.yale.edu/~jay/EC2/InstallR.sh
## chmod +x InstallR.sh
## ./InstallR.sh
##
## - Modified by Elena Khusainova

## Set some variables here:

debsource='deb http://cran.case.edu/bin/linux/ubuntu xenial/'
doextras=1           # 0 if you don't want apache, LaTeX, Rserve/FastRWeb, shiny

## Choose the R version here:

rversion='3.3.2-1xenial0'
wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.r

## ----------------------------------------------------------------------------
## - Probably don't modify, below
## ----------------------------------------------------------------------------

echo ${debsource} >> /etc/apt/sources.list
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
apt-get update

echo "\n\nFinished update, installing R...\n\n"

apt-get -y --force-yes install r-base=${rversion} r-recommended=${rversion} r-base-dev=${rversion}
apt-get -y --force-yes install r-base-core=${rversion}

# If devtools/ghit needed and there are problems with installation:
# First, try to uncomment two lines bellow: 
#apt-get -y build-dep libcurl4-gnutls-dev
#apt-get -y install libcurl4-gnutls-dev
# If doesn't help (I never tried) type it manually in Terminal


if [ $doextras = 1 ] ; then
  
  echo "\n\nFinished R, doing LaTeX and Apache...\n\n"

  apt-get -y --force-yes install apache2
  apt-get -y --force-yes install libcairo2-dev

  echo "\n\nFinished Apache.\n\n"
  echo "\n\nDoing libxt, knitr, ...\n\n"

  apt-get -y --force-yes install libxt-dev

fi

R CMD BATCH InstallPackages.R        # bigmemory, foreach, ...

if [ $doextras = 1 ] ; then

  echo "\n\nDoing Shiny installation.\n\n"
  
  # Shiny:
  apt-get update
  apt-get -y --force-yes install python-software-properties python g++ make
  add-apt-repository ppa:chris-lea/node.js
  apt-get update
  apt-get -y --force-yes install nodejs
  sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
  apt-get install gdebi-core
  wget -O shiny-server.deb http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.5.1.834-amd64.deb
  gdebi shiny-server.deb
  
fi

mkdir /mnt/test
chown ubuntu:ubuntu /mnt/test

echo "Installation complete\n"
echo "Test Shiny at http://host:3838 after starting up shiny-server as root.\n"



