# DSplot
Hw 4: Biostat 625 assignment focused in data visualization using the Graphics 
R package to replicate some of the functionality in the GGplot2 package

## Introduction

The `DSplot` package replicates some of the functionality in the R package `ggplot2`. 
The DSplot package was an attempt of creating basic scatter plots in R, using only R Base 
packages. This also includes editting features such as changing the colors, size, or shape
of certain features in a scatter plot.


## Installation

To install, clone this repository to your desired directory. The folder `R` should 
contain two sets of code for two different functions included in the DSplot package. 
Use the the following to install the package:
```angular2html
devtools::install_github("dsylvert/DSplot")
```


##Overview

The intention of this package is to create basic scatter plots and edit the features 
of the scatter plot created. It can change titles, points (shape, size, and color), 
you can add titles(main title and subtitle), and add lines (connecting points or line 
of best fit). This is an attempt to replicate features in the ggplot2 package.

## Usage

`scatterplot.basic()` must be used in order to access the editting features of 
DSplot. Once the function is called, `customize()` can be used to change characteristics
in the original plot. Note that hidden variables are created after the use of the 
`scatterplot.basic()` function to be passed into the `customize()` function. There
are a total of 6 hidden variables created:
* .xdata
* .ydata
* .xname
* .yname
* .xrange
* .yrange





