# PGE Wildfire Analysis

Downed electrical lines are among the top causes of wildfires. In 2019 the California energy provider Pacific Gas and Electric implemented a program of rolling blackouts to help prevent fires from being started. If this approach prevents major fires it will be of immense value, but it also carries significant costs, leaving hundreds of thousands without power for hours or even days at a time.

This repository contains a set of simple analyses looking at the costs of this program, focusing on their spatial distribution and how this relates to where fires have actually occurred.


## Data ##
#### outages_expanded.csv 
is the dataset of PG&E outages.


## Exploratory Analysis ##
#### trendline.ipynb
illustrates the trends in the number of California residents affected by PG&E's rolling outages during October of 2019.

#### map.ipynb
is a jupyter notebook in which Python is used to draw a map showing the distribution estimated hours of PG&E outages across California.

#### person_hours_choropleth.qgs
is a QGIS file containing spatial analysis of these datasets. This file was used to produce map_person_hours_perimeters.pdf

#### map_person_hours_perimeters.pdf
is a choropleth map, juxtaposing the person-hours of outages by county with the perimeters of wildfires from 2000-2018.


## Statistical Analysis
#### regression_rjags.R
defines, estimates, and evaluates the fit of a simple Bayesian regression model of the relationship between emergency outages and the number and size of powerline-related wildfires.

#### posterior_areas.pdf
shows the posterior distributions of the regression coefficients on wildfire count and size.
