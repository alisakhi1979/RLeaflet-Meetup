
library(leaflet)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)

# First Map ------------------------------------------------------

#empty viewer window
m <- leaflet()
m

#world map
m <- leaflet() %>%
  addTiles()
m

#map centered on Alaska
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4)
m


# Customizing Look and Feel --------------------------------------

#Load counties shape file.
counties <- readOGR("data/tl_2013_02_cousub/tl_2013_02_cousub.shp")
#Filter for Alaska
ak_counties <- subset(counties, counties$STATEFP == "02")


#add shapefile
m <- leaflet() %>%
  addTiles() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5)
m


#use a different basemap
m <- leaflet() %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 4) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5)
m


#add our homicide data
ak_unsolved <- readRDS("data/alaska_data_jittered.rds")

m <- leaflet() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(lng = ak_unsolved$lon, 
                   lat = ak_unsolved$lat,
                   color = "ffffff",
                   weight = 1,
                   radius = 5,
                   label = ak_unsolved$Year)
m


#note that the label appears blank. this is because Year is an integer, needs to
#be text.

#create character vectors in HTML that contain what we want to show in the
#labels

ak_unsolved$label <- paste("<p>", ak_unsolved$City, "</p>",
                           "<p>", ak_unsolved$Month, " ", ak_unsolved$Year, "</p>",
                           "<p>", ak_unsolved$Victim.Sex, ", ", ak_unsolved$Victim.Age, "</p>",
                           "<p>", ak_unsolved$Victim.Race, "</p>",
                           "<p>", ak_unsolved$Weapon, "</p>",
                           sep="")



#map has HTML label but it reads it as plain text.  Need to treat text like HTML
#code.  To do this, we use the HTML function from the htmltools package.
m <- leaflet() %>%
  setView(lng = -149.4937, lat = 64.2008, zoom = 3) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = ak_counties,
              color = "#660000", 
              weight = 1, 
              smoothFactor = 0.5) %>%
  addCircleMarkers(lng = ak_unsolved$lon, 
                   lat = ak_unsolved$lat,
                   color = "red",
                   weight = 1,
                   radius = 5, 
                   opacity = 0.75,
                   label = lapply(ak_unsolved$label, HTML))
m




# Chloropleths ------------------------------------------------------------


#Let's create a chloropleth to show the number of solved and unsolved murders
#for each state

#Get shapefile for states
states <- readOGR("data/cb_2016_us_state_500k/cb_2016_us_state_500k.shp")

m <- leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = states,
              weight = 1, 
              smoothFactor = 0.5)
m

#import our summary stats
us <- readRDS("data/US_summary_data.rds")

#should now be all true
is.element(us$State, states$NAME)

#now check that all shapefile states are in us data
is.element(states$NAME, us$State)

#we see that we're missing the following: 33, 34, 54, 55, 56
states <- subset(states, is.element(states$NAME, us$State))

#now order the crime stats so that its the same order as the shapefile states
us <- us[order(match(us$State, states$NAME)),]

bins <- c(0.30, 0.40,0.50,0.60,0.70,0.80,0.90, 1.0)
pal <- colorBin("RdYlBu", domain = us$Solve.Rate, bins = bins)

labels <- paste("<p>", us$State, "</p>",
                "<p>", "Solve Rate: ", round(us$Solve.Rate, digits = 3), "</p>",
                sep="")

m <- leaflet() %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles(providers$Stamen.Toner) %>%
  addPolygons(data = states,
              fillColor = pal(us$Solve.Rate),
              weight = 1, 
              color = "white",
              fillOpacity = 0.8,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.7,
                bringToFront = TRUE),
              label = lapply(labels, HTML)) %>%
  addLegend(pal = pal, 
            values = us$Solve.Rate, 
            opacity = 0.7, 
            title = NULL,
            position = "topright")
m


#save static version of map
#important to run webshot::install_phantomjs() if haven't already installed phantomjs()
mapshot(m, file ="static_map.png")

#save dynamic version of map
saveWidget(m, file = "dynamic_map.html")





