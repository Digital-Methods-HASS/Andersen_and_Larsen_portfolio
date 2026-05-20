# Activate the library
library(leaflet)
library(htmlwidgets) # not essential, only needed for saving the map as .html
library(htmltools)
library(tidyverse)


########################################  TASK NUMBER ONE

# Task 1: Create a Danish equivalent of AUSmap with Esri layers, 
# but call it DANmap. You will need it layer as a background for Danish data points.

DANmap <- leaflet() %>%
  setView(lng = 10.85089,55.2339084,zoom =6) %>% 
  addTiles() %>% 
  addProviderTiles("Esri.WorldPhysical", group = "Physical") %>%  
  addProviderTiles("Esri.WorldImagery", group = "Aerial") %>%  
  addProviderTiles("MtbMap", group = "Geo") %>%               
  addLayersControl(                                 
    baseGroups = c("Geo","Aerial", "Physical"),
    options = layersControlOptions(collapsed = F))
DANmap

########################################
######################################## ADD DATA TO LEAFLET

# Before you can proceed to Task 2, you need to learn about coordinate creation. 
# In this section you will manually create machine-readable spatial
# data from GoogleMaps, load these into R, and display them in Leaflet with addMarkers(): 

### First, go to https://bit.ly/CreateCoordinates1
### Enter the coordinates of your favorite leisure places in Denmark 
# extracting them from the URL in googlemaps, adding name and type of monument.
# Remember to copy the coordinates as a string, as just two decimal numbers separated by comma. 

# Caveats: Do NOT edit the grey columns! They populate automatically!

### Second, read the sheet into R. You will need gmail login information. 
# IMPORTANT: watch the console, it may ask you to authenticate or put in the number 
# that corresponds to the account you wish to use.

# Libraries
library(tidyverse)
library(googlesheets4)
library(leaflet)

# If you experience difficulty with your read_sheet() function (it is erroring out), 
# uncomment and run the following function:
gs4_deauth()  # run this line and then rerun the read_sheet() function below

# Read in the Google sheet you've edited
places <- read_sheet("https://docs.google.com/spreadsheets/d/1PlxsPElZML8LZKyXbqdAYeQCDIvDps2McZx1cTVWSzI/edit?gid=1808791124#gid=1808791124",
                     col_types = "cccnncnc",   # check that you have the right number and type of columns
                     range = "DAM2026")  # select the correct worksheet name

glimpse(places)  

places %>% 
  filter(!is.na(Longitude)) %>% 
  filter(!is.na(Latitude))


# Question 3: are the Latitude and Longitude columns present? YES
# Do they contain numeric decimal degrees? YES

# If your coordinates look good, see how you can use addMarkers() function to
# load them in a basic map. Run the lines below and check: are any points missing? Why?

studentmap<- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = places$Longitude, 
             lat = places$Latitude,
             popup = paste(places$Description, "<br>", places$Type))
studentmap

# Now that you have learned how to load points from a googlesheet to a basic leaflet map, 
# apply the know-how to YOUR DANmap object. 

############################################# TASK TWO


# Task 2: Read in the googlesheet data you and your colleagues created
# into your DANmap object (with 11 background layers you created in Task 1).

# Solution
DANmap %>% 
  addMarkers(lng = places$Longitude, 
             lat = places$Latitude,
             popup = paste(places$Description, "<br>", places$Type))


######################################## TASK THREE

# Task 3: Can you cluster the points in Leaflet?
# Hint: Google "clustering options in Leaflet in R"

DANmap_cluster <- DANmap %>% 
  addMarkers(
    lng = places$Longitude,
    lat = places$Latitude,
    popup = paste(places$Description, "<br>", places$Type),
    clusterOptions = markerClusterOptions()
  )
DANmap_cluster

######################################## TASK FOUR

# Task 4: Look at the two maps (with and without clustering) and consider what
# each is good for and what not.

# Answer:
#A clustered map is useful for getting an overview when there are many points together. The clustered map groups the nearby point and makes the map less crowded. 
# A map without clustering is better for seeing each point, but it can sometimes be difficult to read if many points overlap, so it scales well with large datasets. 


######################################## TASK FIVE

# Task 5: Find out how to display the notes and classifications column in the map. 
# Hint: Check online help in sites such as 
# https://r-charts.com/spatial/interactive-maps-leaflet/#popup

# Solution

DANmap_notes <- DANmap %>% 
  addMarkers(lng = places$Longitude, 
             lat = places$Latitude,
             popup = paste( places$Placename, "<br>",
                            places$Type, "<br>",
                            places$Classification, "<br>",
                            places$Notes, "<br>",
                            places$Description
             ),
             clusterOptions = markerClusterOptions()
  )
DANmap_notes
######################################## CONGRATULATIONS - YOUR ARE DONE :)
