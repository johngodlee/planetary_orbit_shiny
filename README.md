# Visualise planetary orbits on a world map

Calculations and a Shiny app to visualise planetary orbits on a world map, using a model Sun object of a given size. This project was heavily inspired by a [primary school experiment](https://nssdc.gsfc.nasa.gov/planetary/education/schoolyard_ss/) we did with a beach ball in the playground to visualise just how small the planets are and how big the Solar System is.

* `ellipses.R` calculates the elliptical orbits of each planet (and pluto), given their major and minor axes, eccentricity. 
* `orbit_app/` contains a Shiny app which plots these elliptical orbits onto a world map using `leaflet` and provides an interface for adjusting some parameters.
* `circle_orbit_app/` contains an earlier protoype app which models the planetary orbits as circular, assuming their mean distance from the Sun during their orbit.

Figures on the planetary orbits came from [this NASA factsheet](https://nssdc.gsfc.nasa.gov/planetary/factsheet/), which takes data from multiple sources. 

