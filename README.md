# StaMPS-Visualizer

![](https://github.com/thho/StaMPS_Visualizer/blob/master/stamps_visualizer21_preview.png "StaMPS-Visualizer 2.1 preview")

A Shiny application for visualizing DInSAR results processed by [StaMPS/MTI](https://homepages.see.leeds.ac.uk/~earahoo/stamps/).

Hooper A., Bekaert D. Spaans K. and M. Arikan (2012), Recent advances in SAR interferometry time series analysis for measuring crustal deformation, Tectonophysics, 514-517, pp.1-13. doi: 10.1016/j.tecto.2011.10.013

Further discussion about the application can be found in ESA's [Step-Forum](https://forum.step.esa.int/t/stamps-visualizer-snap-stamps-workflow/9613?u=thho)

An example of the app is currently available online, hosted with a free shinyapps.io account. Therefore it may be that the app is not accessible if all active hours per month are exhausted. To work with the app and adding your own data locally, download or clone the repo. [StaMPS-Visualizer example](https://thho.shinyapps.io/StaMPS_Visualizer/)

To run the application locally, following packages are requiered:

```{r install-packages eval=FALSE}
install.packages("leaflet")
install.packages("lubridate")
install.packages("shiny")
install.packages("colorRamps")
```

Copy paste the code to your R console, or run the R script *install_packages.R*

## About the App and Disclaimer

<br/>

*StaMPS-Visualizer* is a free application and comes with **absolutely no warrenty**. The application is distributed under the terms of the GNU General
Public License, either Version 2, June 1991 or Version 3, June 2007. You are welcome to redistribute the application in its complete version. See further information on how to cite *StaMPS-Visualizer*.


## Citing the Application *StaMPS-Visualizer*

<br/>

The application is part of a [master thesis at the Department of Geography, Bonn University](https://doi.org/10.13140/RG.2.2.35085.59362). To cite the application, cite the thesis including the application:

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
