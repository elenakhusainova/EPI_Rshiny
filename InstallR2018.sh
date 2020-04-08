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

## Set some variables here:

#debsource='deb http://cran.case.edu/bin/linux/ubuntu precise/'
debsource='deb http://cran.case.edu/bin/linux/ubuntu trusty/'
#fastrweb='/usr/local/lib/R/site-library/FastRWeb'
doextras=1           # 0 if you don't want apache, LaTeX, Rserve/FastRWeb, shiny

## Choose the R version here:

#rversion='2.15.3-1precise0precise1'
#rversion='3.0.1-1precise0precise2'
#rversion='3.1.1-1trusty0_all
rversion='3.2.5-1trusty0'
# Get this and modify by hand for further package customization:
wget http://www.stat.yale.edu/~jay/EC2/InstallPackages.R

## ----------------------------------------------------------------------------
## - Probably don't modify, below
## ----------------------------------------------------------------------------

echo ${debsource} >> /etc/apt/sources.list
apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9
apt-get update

echo "\n\nFinished update, installing R...\n\n"

apt-get -y --force-yes install r-base=${rversion} r-recommended=${rversion} r-base-dev=${rversion}
apt-get -y --force-yes install r-base-core=${rversion}


if [ $doextras = 1 ] ; then

  wget http://www.stat.yale.edu/~jay/EC2/InstallExtras.R
  
  echo "\n\nFinished R, doing LaTeX and Apache...\n\n"

  #apt-get -y --force-yes install texlive-latex-base
  apt-get -y --force-yes install apache2
  apt-get -y --force-yes install libcairo2-dev

  echo "\n\nFinished Apache.\n\n"
  echo "\n\nDoing libxt, knitr, ...\n\n"

  apt-get -y --force-yes install libxt-dev
  R CMD BATCH InstallExtras.R        # Rserve, FastRWeb, knitr

fi

R CMD BATCH InstallPackages.R        # bigmemory, foreach, ...

if [ $doextras = 1 ] ; then

  #wget http://www.stat.yale.edu/~jay/EC2/InstallShiny.R
  
  echo "\n\nDoing Shiny and FastRWeb postinstallation.\n\n"
  
  # FastRWeb configuration
  #cd ${fastrweb}
  #sh ./install.sh
  #cp Rcgi/Rcgi /usr/lib/cgi-bin/R
  #cd /home/ubuntu

  #echo '#!/usr/bin/perl' > /usr/lib/cgi-bin/foo.cgi
  #echo 'print "Content-type: text/html\n\n";' >> /usr/lib/cgi-bin/foo.cgi
  #echo 'print "Hello World from a Perl test CGI script.";' >> /usr/lib/cgi-bin/foo.cgi
  #chmod +x /usr/lib/cgi-bin/foo.cgi

  #/var/FastRWeb/code/start

  # Shiny:
  apt-get update
  apt-get -y --force-yes install python-software-properties python g++ make
  add-apt-repository ppa:chris-lea/node.js
  apt-get update
  apt-get -y --force-yes install nodejs
  sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
  apt-get install gdebi-core
  wget -O shiny-server.deb http://download3.rstudio.org/ubuntu-12.04/x86_64/shiny-server-1.3.0.403-amd64.deb
  gdebi shiny-server.deb
  
  # Jay, unsure if the following is really needed, might check.
  #wget https://raw.github.com/rstudio/shiny-server/master/config/upstart/shiny-server.conf -O /etc/init/shiny-server.conf
  #useradd -r shiny
  #mkdir -p /var/shiny-server/www
  #mkdir -p /var/shiny-server/log

  # These are no lo
  #cp -rp /usr/local/lib/R/site-library/shiny/examples /var/shiny-server/www
  
  start shiny-server

fi

mkdir /mnt/test
chown ubuntu:ubuntu /mnt/test

echo "Installation complete\n"
#echo "Test CGI script at http://host/cgi-bin/foo.cgi.\n"
#echo "Test FastRWeb at http://host/cgi-bin/R/main.\n"
echo "Test Shiny at http://host:3838 after starting up shiny-server as root.\n"



