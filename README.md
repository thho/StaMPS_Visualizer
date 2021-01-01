# StaMPS-Visualizer

![](https://github.com/thho/StaMPS_Visualizer/blob/master/preview30_map.png "StaMPS-Visualizer 3.0")

A Shiny application for visualizing DInSAR results processed by [StaMPS/MTI](https://homepages.see.leeds.ac.uk/~earahoo/stamps/).

Hooper A., Bekaert D. Spaans K. and M. Arikan (2012), Recent advances in SAR interferometry time series analysis for measuring crustal deformation, Tectonophysics, 514-517, pp.1-13. doi: 10.1016/j.tecto.2011.10.013

Further discussion about the application can be found in ESA's [Step-Forum](https://forum.step.esa.int/t/stamps-visualizer-snap-stamps-workflow/9613?u=thho)

An example of the app is currently available online, hosted with a free shinyapps.io account. Therefore it may be that the app is not accessible if all active hours per month are exhausted. To work with the app and adding your own data locally, download or clone the repo. [StaMPS-Visualizer example](https://thho.shinyapps.io/StaMPS_Visualizer/)

## Installation

You need R 3.6 or higher and RStudio

**Linux**

Tested on Ubuntu 20.04.1 LTS

```
# first install gdal libraries
sudo apt update
sudo apt install gdal-bin libgdal-dev -y

# install R
sudo apt update
sudo apt install r-base gdebi-core -y

# install RStudio
# download RStudio
# https://rstudio.com/products/rstudio/download/#download

cd ~/Downloads
sudo gdebi rstudio-1.3.1093-amd64.deb
```

**Windows**

Download R https://cran.r-project.org/bin/windows/base/ and run the ```.exe```

Download RStudio https://rstudio.com/products/rstudio/download/#download and run the ```.exe```

### Installation with renv (recommended)

Since version 3.0 StaMPS-Visualizer is managed as a *Rproject* with ```renv```. ```renv``` [is a tool to create reproducible environments of R packages](https://rstudio.github.io/renv/index.html).

* clone this repository 
* open the ```StaMPS_Visualizer.Rproj``` file with Rstudio
* run ```install.packages("renv")``` in the R Console
* run ```renv::restore()``` in the R Console to restore the complete environment (this might take some time)
* go to File --> Open File... and open ```ui.R```
* click on **Run App** in the upper left panel in the upper right corner

### Manual installation of all packages

To run the application locally, following packages are requiered:

```{r install-packages eval=FALSE}
install.packages("leaflet")
install.packages("lubridate")
install.packages("shiny")
install.packages("shinycssloaders")
install.packages("shinyalert")
install.packages("colorRamps")
install.packages("rgdal")
install.packages("data.table")
```

Copy paste the code to your R console, or run the R script *install_packages.R*

in order to start the Visualizer, open the ```ui.R``` script in RStudio and click on **Run App** in the upper left panel in the upper right corner.

## License

The StaMPS-Visualizer is licensed under a [creative commons attribution license](https://github.com/thho/StaMPS_Visualizer/blob/master/LICENSE.md)

## Citing *StaMPS-Visualizer*

Since version 3.0 StaMPS-Visualizer is published as software on Zenodo:

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.4407188.svg)](https://doi.org/10.5281/zenodo.4407188)

```
@software{thorsten_hoeser_2020_4407188,
  author       = {Thorsten Hoeser},
  title        = {StaMPS\_Visualizer},
  month        = dec,
  year         = 2020,
  publisher    = {Zenodo},
  version      = {v3.0},
  doi          = {10.5281/zenodo.4407188},
  url          = {https://doi.org/10.5281/zenodo.4407188}
}
```

The original version was published as part of a [master thesis at the Department of Geography, Bonn University](https://doi.org/10.13140/RG.2.2.35085.59362).

Bibtex:

```{css eval=FALSE}
@mastersthesis{thho2018,
  author       = {Thorsten Höser}, 
  title        = {Analysing the Capabilities and Limitations of {InSAR} using {Sentinel-1} Data for Landslide Detection and Monitoring},
  school       = {Department of Geography, Bonn University},
  year         = 2018,
  address      = {Bonn},
  doi          = {10.13140/RG.2.2.35085.59362}
}
```

## Disclaimer

<br/>

*StaMPS-Visualizer* is a free application and comes with **absolutely no warranty**.

<br/>
