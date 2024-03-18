[![Documentation](https://img.shields.io/badge/documentation-chartbli-orange.svg?colorB=E91E63)](https://d2gex.github.io/chartlbi/)

# chartlbi

chartlbi is a project built around the utilities for the Length Based Indicator (LBI) algorithm developed 
by [ICES](https://github.com/ices-tools-dev/LBI_shiny/blob/master/utilities.R) and provides an alternative visualisation 
of the LBI indicators to the old traffic light system. As a consequence most of the obsolete libraries the original
source code was relying on such [ReportRs](https://github.com/davidgohel/ReporteRs) have now vanished. In addition, neither
JAVA nor Java dependencies such [ReporteRsjars](https://github.com/davidgohel/ReporteRsjars) are any longer required, 
therefore easing its installation.

## LBI Visualisation
The primary goal of this wrapper is to make the LBI visual outputs more modern-like, visually-appealing and
hopefully more intuitive, facilitating its integration with other similar length-based methods applied as part of everyone's
data analysis. Each indicator is now plotted by using `ggplot2` and stacked up one on top of another, where
every individual indicator value per timestep is visually compared against their theoretical thresholds. Any value equal 
or above the threshold will be plotted in <span style="color: green;">green</span> and below in <span style="color: red;">red</span>.

## Awareness of old bugs

chartlbi has cut from the source code the capacity of showing the **Optimal Yield** indicator as a function of `Lmaxy`,
that is, the length class from the length-frequency matrix that holds the largest biomass per timestep. The reason being
is that there are two bugs in the original source code as follows:

<ol>
<li>The old algorithm does not contemplate that two length-classes may hold the same biomass. Such case is possible
when the mean weight for a single year and length class is calculated as the average weight of all individuals within, 
rather than using the formula $W = a*L^b$.(See <a href="https://github.com/ices-tools-dev/LBI_shiny/blob/8a8e7706232d68e5ccab9207ae9c273c83b778b6/utilities.R#L185">bug source code</a>).</li>
<li>Even when there is only one single length-class per maximum biomass and timestep, the calculation of `Lmaxy` is
performed by multiplying the columns of the length-frequency and weight-at-length in desynchronised mode. There is a
mismatch for every pair of columns. (See <a href="https://github.com/ices-tools-dev/LBI_shiny/blob/8a8e7706232d68e5ccab9207ae9c273c83b778b6/utilities.R#L184">bug source code</a>)</li>
</ol>

# Installation

This R package can be installed through the devtools as follows:
```r 
  devtools::install_github("https://github.com/d2gex/chartlbi", dep=TRUE)
```

