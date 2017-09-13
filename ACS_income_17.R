library(tidyverse)
library(readr)

ACS_2016 <- read_csv(unzip("2016_ACS_income_county.zip", "ACS_16_1YR_S1903_with_ann.csv"))
ACS_2015 <- read_csv(unzip("2015_ACS_income_county.zip", "ACS_15_1YR_S1903_with_ann.csv"))

# Get just incomes and MOEs
income_compare <- ACS_2016 %>% select(GEO.id2, `GEO.display-label`,
                                      HC02_EST_VC02, HC02_MOE_VC02) %>% 
  rename(median_16 = HC02_EST_VC02,
         error_16 = HC02_MOE_VC02)
income_compare <- ACS_2015 %>% select(GEO.id2,
                                      HC02_EST_VC02, HC02_MOE_VC02) %>% 
  rename(median_15 = HC02_EST_VC02,
         error_15 = HC02_MOE_VC02) %>% 
  inner_join(income_compare, by = "GEO.id2")

income_compare <- income_compare %>% 
  mutate(change = median_16 - median_15,
         siggain = ifelse((median_16 - error_16) > (median_15 + error_15), 1, 0),
         sigloss = ifelse((median_16 + error_16) < (median_15 - error_15), 1, 0),
         sig = ifelse(siggain == 1 | sigloss ==1, 1, 0))

income_compare %>% mutate(sigs = ifelse(siggain == 1, "gain", 
                                        ifelse(sigloss == 1, "loss", "no sig change"))) %>% 
  group_by(sigs) %>% 
  summarize(total = length(sigs))

library(censusapi)
load("~/mapping/census_key.RData")

vars2012 <- listCensusMetadata(name="acs1", vintage=2012, "v")
vars2012 %>% filter(grepl("Median", concept) & grepl("Household Income", concept)) %>% View(.)

# we want variables from table S1903
ACS_2012 <-  getCensus(name="acs1", 
                       vintage=2012,
                       key=census_key, 
                       vars=c("NAME", "B19013E_001E", "B19013E_001M"), 
                       region="county:*")

# Need to inflation adjust and rectify places
cpi_adj <- read_csv("~/ASEC/CPI_RS.csv")
cpi_adj <- cpi_adj %>% 
  mutate(adj = cpi_adj$CPI_RS[cpi_adj$Year == 2016]/CPI_RS)
adjust <- cpi_adj$adj[cpi_adj$Year == 2012]

ACS_2012 <- ACS_2012 %>% mutate(GEOID = paste0(state, county),
                                median_12 = B19013E_001E * adjust,
                                error_12 = B19013E_001M * adjust)

income_compare <- left_join(income_compare, 
                            ACS_2012 %>% select(GEOID, median_12, error_12), 
                            by = c("GEO.id2" = "GEOID"))


income_compare <- income_compare %>% 
  mutate(change_12 = median_16 - median_12,
         siggain_12 = ifelse((median_16 - error_16) > (median_12 + error_12), 1, 0),
         sigloss_12 = ifelse((median_16 + error_16) < (median_12 - error_12), 1, 0),
         sig_12 = ifelse(siggain_12 == 1 | sigloss_12 == 1, 1, 0),
         pct_change_12 = (change_12/median_12) -1)

income_compare %>% mutate(sigs = ifelse(siggain_12 == 1, "gain", 
                                        ifelse(sigloss_12 == 1, "loss", "no sig change"))) %>% 
  group_by(sigs) %>% 
  summarize(total = length(sigs))

# Map it!
library(leaflet)

# library(devtools)
# install_version("tigris", version = "0.5", dep = FALSE) # need this version or download fails
library(tigris)

counties_map <- counties(cb = TRUE)

county_merge <- geo_join(counties_map, income_compare, "GEOID", "GEO.id2")

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = county_merge, 
              fillColor = county_merge$change_12/county_merge$median_12 -1, # percent change
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2)

popup <- paste0(county_merge$NAME, ": ", sprintf("%.1f",county_merge$pct_change_12*100), "%")
pal <- colorNumeric("Greens", domain=county_merge$pct_change_12)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = county_merge, 
              fillColor = ~pal(county_merge$pct_change_12),
              fillOpacity = 0.7, 
              weight = 0.2, 
              smoothFactor = 0.2,
              popup = ~popup) %>% 
  addLegend(pal = pal,
            values = county_merge$pct_change_12,
            position = "bottomright",
            title = "Change since 2012")
