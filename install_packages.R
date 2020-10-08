####################
##install packages##
####################


install.packages("lubridate")
install.packages("shiny")
install.packages("colorRamps")
install.packages("leaflet")

####not necessary yet
install.packages("rgdal")
#for Ubuntu 16 and higher do 
#sudo apt-get install libudunits2-dev libgdal-dev libgeos-dev libproj-dev
system("sudo -S apt-get -y install libudunits2-dev libgdal-dev libgeos-dev libproj-dev", input = "<password>")
