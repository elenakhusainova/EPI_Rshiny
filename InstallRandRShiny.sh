sudo apt-get update
sudo apt-get install r-base
sudo apt-get install r-base-dev

wget https://raw.githubusercontent.com/elenakhusainova/EPI_Rshiny/master/InstallPackages.r
R CMD BATCH InstallPackages.R
