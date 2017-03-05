# Experimenting with ACS and TIGRIS packages
library(acs)
library(tigris)
library(dplyr)
library(tidyr)

api.key.install(key="") # enter API key

# guide: http://zevross.com/blog/2015/10/14/manipulating-and-mapping-us-census-data-in-r-using-the-acs-tigris-and-leaflet-packages-3/#census-data-the-easyer-way
geo <- geo.make(state = "NY", county = "*") # define geography for acs.fetch
counties <- counties(state = "NY", cb = T) # download county boundaries


income <- acs.fetch(endyear = 2015, span = 5, geography = geo, table.number = "B19001", col.names = "pretty")

# convert to df
attr(income, "acs.colnames")
income_df <- data.frame(countyname = income@geography$NAME, state = income@geography$state, county = income@geography$county, total = income@estimate[,1], rich = income@estimate[,17])
income_df <- income_df %>% 
  mutate(fips = paste0(state, county),
         share = 100*rich/total)

income_merged <- geo_join(counties, income_df, "GEOID", "fips")

# Map it
library(leaflet)
popup <- paste0("FIPS: ", income_merged$countyname, "<br>", "Wealthy share: ", round(income_merged$share, 2), "%")
pal <- colorNumeric(palette = "YlGnBu",
                    domain = income_merged$share)

map <- leaflet() %>%  
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data = income_merged,
              fillColor = ~pal(share),
              fillOpacity = 0.7,
              weight = 1,
              smoothFactor = 0.2,
              color = "#b2aeae", popup = popup) %>% 
  addLegend(pal = pal,
             values = income_merged$share,
             position = "bottomright",
             title = "Share of households <br> With incomes > $200k",
            labFormat = labelFormat(suffix = "%"))
map     

library(htmlwidgets)
saveWidget(map, file = "ny_income.html", selfcontained = F)
