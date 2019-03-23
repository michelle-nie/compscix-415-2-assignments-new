install.packages('rvest')
library('rvest')

get_out <- read_html('https://www.imdb.com/title/tt5052448/?ref_=nv_sr_1')
class(get_out)

get_out %>% 
  html_nodes("table") %>% 
  .[[1]] %>% 
  html_table()


library(jsonlite)
library(shiny)
library(tidyverse)
library(nycflights13)


url <- ("https://api.jcdecaux.com/vls/v1/stations?contract=Dublin&apiKey=f57c56ee5683249b1d4f1fb38009d48c83827c81")
glimpse(url)
df_api <- fromJSON(url)
View(df_api)

saveRDS(flights, file = "C:/Users/mnie/Documents/RClass/compscix-415-2-assignments-new/flights.rds")
readRDS("C:/Users/mnie/Documents/RClass/compscix-415-2-assignments-new/flights.rds")


install.packages('XLConnect')
library(readxl)

predictor <- 'carat'
mod_formula <- paste0('price ~ ', predictor)
mod_formula



fit_model <- function(samp_size, x_var) {
  diam_samp <- diamonds %>% sample_n(samp_size) # random sample
  diam_samp_lm <- lm(paste0('price ~ ', x_var), data = diam_samp)
  slopes <- coef(diam_samp_lm)[2] # store the coefficient
  fit_model_list <- list(samp = diam_samp, model = diam_samp_lm, slope = slopes)
  return(fit_model_list)
}
# call function
fit_model(1000, 'cut')

library(mdsr)
glimpse(CholeraDeaths)
cholera_df <- as.data.frame(CholeraDeaths)
head(cholera_df)

install.packages('rgdal')
library(rgdal)

proj4string(CholeraDeaths) <- CRS("+init=epsg:27700")

cholera_latlong <- CholeraDeaths %>% spTransform(CRS("+init=epsg:4326"))
head(cholera_latlong)

cholera_latlong <- as.data.frame(cholera_latlong)
cholera_latlong <- cholera_latlong %>% 
  mutate(longitude = coords.x1, latitude = coords.x2)


install.packages('ggmap')

library(ggmap)
ucb_ext <- '160 Spear St, San Francisco, CA'
ucb_geo <- geocode(ucb_ext)

register_google('AIzaSyCZTJlfvGFH_lgUPEoOgLKl6CYXY-yyjT8', 'standard')

library(leaflet)
leaflet() %>% addTiles()
leaflet() %>% 
  setView(lng = -122.3937, lat = 37.79137, zoom = 14) %>% 
  addTiles() 

map <- leaflet() %>% 
  addTiles() %>% 
  addMarkers(lng = ~lon, lat = ~lat, data = ucb_geo)
map

cholera <- leaflet(cholera_latlong) %>% 
  addTiles() %>% 
  addMarkers()

cholera
