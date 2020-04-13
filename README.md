# Data Visualization Engineer (Back End)
## Case Study - Spring 2020
## Author: Jacob Pollien


## Objective:
Build endpoints using Flask that provide data to an application.  This application should allow the user to explore where different commodity crops are grown in the United States.


## Overview of my approach:
I opted for a flattened Flask approach leaning on flask_rest_jsonapi to help me compact the entire application into more or less a single file.  The hierarchy of this application is:

Indigo Case Study>
	-app.py
	-manage.py
	templates
		-index.html
	-.env
	-requirements.txt
	-Procfile

There is also a locally hosted Postgres database into which I've imported the usda_crops_5yr.csv file provided with the case study.  The postgres database was cloned up to a Heroku (free platform as a service site for hobbyists) database.  The Flask app was uploaded to Github and then pushed to Heroku to operate on top of the postgres database hosted in the cloud.  The index for this API can be found at: https://indigocasestudyjtp.herokuapp.com/

Please note that due to the free nature of the service, the site will sleep after a certain period of inactivity and will require a few seconds to relaunch.

The interactive map was made in R and relies on the Leaflet and Shiny packages.  Leaflet is an open-source JavaScript library that allows for convenient manipulation of spatial data.  In this case, I downloaded the 2018 US Census TIGER shapefiles for county borders and merged it with the dataset pulled from the API.  Meanwhile, Shiny is a package that makes it easy to embed interactive web apps.

The map is hosted at: https://jpollien.shinyapps.io/IndigoCaseStudy/

## PLEASE NOTE: due to the aforementioned sleep timer of Heroku, the app may time out the first time you try to load it.  If this happens, please refresh and it should work fine.
