# StaMPS-Visualizer

A Shiny application for visualizing DInSAR results processed by StaMPS/MTI.

Futher discussion about the application can be found in ESA's [Step-Forum](https://forum.step.esa.int/t/stamps-visualizer-snap-stamps-workflow/9613?u=thho)

To run the application locally, follwing packages are requiered:

```{r install-packages eval=FALSE}
install.packages("leaflet")
install.packages("lubridate")
install.packages("shiny")
install.packages("colorRamps")
```

Copy paste the code to your R console, or run the R script *install_packages.R*

## About the App and Disclaimer

<br/>

*StaMPS/MTI-Visualizer* is a free application and comes with **absolutely no warrenty**. The application is distributed under the terms of the GNU General
Public License, either Version 2, June 1991 or Version 3, June 2007. You are welcome to redistribute the application in its complete version. See further information on how to cite *StaMPS/MTI-Visualizer*.


## Citing the Application *StaMPS/MTI Visualizer*

<br/>

The application is part of a master thesis at the Department of Geography, Bonn University. To cite the application, cite the thesis including the application:

Bibtex:

```{css eval=FALSE}
@mastersthesis{thho2018,
  author       = {Thorsten Höser}, 
  title        = {Analysing the Capabilities and Limitations of {InSAR} using {Sentinel-1} Data for Landslide Detection and Monitoring},
  school       = {Department of Geography, Bonn University},
  year         = 2018,
  address      = {Bonn}
}
```