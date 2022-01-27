setwd("C:/Users/USER/R studio/Interactive map")

covid19_test_county_all<-fread("https://opendata.digilugu.ee/opendata_covid19_test_county_all.csv", encoding = "UTF-8")

covid19_test_county_all<-covid19_test_county_all %>%
  filter(ResultValue=="P")

covid19_test_county_all %>% 
  distinct(ResultValue, .keep_all = FALSE)

ggplot()+
  geom_line(data = covid19_test_county_all, aes(x= StatisticsDate, y = DailyCases), size= .25)+
  facet_wrap(vars(County), scale = "free_y")

glimpse(covid19_test_county_all)

covid19_test_county_all$County <- str_replace(covid19_test_county_all$County, " maakond", "") 

covid19_test_county_all <- covid19_test_county_all %>% 
  filter(!is.na(County))

gg_cov_cases <- ggplot()+
  geom_line(data = covid19_test_county_all, aes(x = StatisticsDate, y = DailyCases), size = .25)+
  facet_wrap(vars(County), scale = "free_y", ncol = 3) 

plotly::ggplotly(gg_cov_cases)

# filter:
covid19_test_county_all_2 <- covid19_test_county_all %>% 
  filter(County == "Tartu" | County =="Ida-Viru")

# ggplot:
gg_cov_cases_2 <- ggplot()+
  geom_line(data = covid19_test_county_all_2, aes(x= StatisticsDate, y = DailyCases, color = County), size= .25, alpha = .5)+
  stat_smooth(data = covid19_test_county_all_2, aes(x= StatisticsDate, y = DailyCases, color = County), method = "loess", size= .25, se = F)+ 
  scale_colour_manual(values = c("blue", "red"))

# plotly plot:  
plotly::ggplotly(gg_cov_cases_2)


library(dygraphs)

# Select relevant variables:
covid19_test_county_all_2a <- covid19_test_county_all_2 %>% 
  select(StatisticsDate, County, DailyCases)

# For dygraph the tabel should be in wide format:
covid19_test_county_all_2a <- covid19_test_county_all_2a %>% 
  pivot_wider(names_from = County, values_from = DailyCases, id_cols = StatisticsDate)

# conversion to xts:
covid19_test_county_all_2b <- xts::xts(covid19_test_county_all_2a, order.by = as_date(covid19_test_county_all_2a$StatisticsDate), "%Y%m%d") 

glimpse(covid19_test_county_all_2b)
# delete the date-column (in xts date is stored separately; try (glimpse()):
covid19_test_county_all_2b$StatisticsDate <- NULL

# plot the dygraph:
dygraph(covid19_test_county_all_2b) %>% 
  dySeries() %>% 
  dyRangeSelector() # allows to zoom to specific period: 

#Interactive maps####
covid19_test_county_all_latest <- covid19_test_county_all %>% 
  select(CountyEHAK, DailyCases, StatisticsDate) %>% 
  filter(StatisticsDate == max(StatisticsDate))

#download data
download.file("https://geoportaal.maaamet.ee/docs/haldus_asustus/maakond_shp.zip", destfile="maakond_shp.zip")

#unzip data
unzip("maakond_shp.zip")

#asustusyksus_shp
list.files(pattern = ".shp")

counties <- st_read("maakond_20211201.shp")

# data structure:
glimpse(counties)

# map:
ggplot()+
  geom_sf(data = counties, fill = "slategray2", col = "blue", size= .25)

counties <- counties %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 200) %>% 
  st_cast("MULTIPOLYGON") # defines the type of geometry after simplification

counties <- counties %>% 
  mutate(MKOOD = as.integer(MKOOD)) 

# structure:
glimpse(counties)
covid19_test_county_all_latest_sf <- left_join(counties, covid19_test_county_all_latest, by = c("MKOOD" = "CountyEHAK"))

glimpse(covid19_test_county_all_latest_sf)

gg_covid19_map <- ggplot()+
  geom_sf(data = covid19_test_county_all_latest_sf, aes(fill = DailyCases))+
  scale_fill_gradientn(colours = c("forestgreen", "grey70", "orange", "red"))

gg_covid19_map


covid19_test_county_all_latest_sf_cntr <- covid19_test_county_all_latest_sf %>% 
  st_centroid()

gg_covid19_map <- ggplot()+
  theme_void()+
  geom_sf(data = covid19_test_county_all_latest_sf, aes(fill = DailyCases))+
  geom_sf_text(data = covid19_test_county_all_latest_sf_cntr, aes(label = DailyCases))+
  scale_fill_gradientn(colours = c("forestgreen", "grey70", "orange", "red"))

gg_covid19_map

plotly::ggplotly(gg_covid19_map)

# polygons:
covid19_test_county_all_latest_sf_4326 <- covid19_test_county_all_latest_sf %>% 
  st_transform(4326)

# labels:
covid19_test_county_all_latest_sf_cntr_4326 <- covid19_test_county_all_latest_sf_cntr %>% 
  st_transform(4326)

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = covid19_test_county_all_latest_sf_4326) 

pal <- colorBin(palette = "Purples", 
                domain = covid19_test_county_all_latest_sf_4326$DailyCases, n = 5) # split colors from white to red into 5 even bins

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = covid19_test_county_all_latest_sf_4326, 
              label= ~DailyCases, #mouseover value
              color = "gray", # border color 
              fillColor = ~pal(covid19_test_county_all_latest_sf_4326$DailyCases), # polygons fill color
              weight = 1.0, # border lines thickness 
              opacity = 1.0, # border lines transparency
              fillOpacity = 0.8) %>%  # polygons fill transparency
  addLabelOnlyMarkers(data = covid19_test_county_all_latest_sf_cntr_4326,
                      label = ~covid19_test_county_all_latest_sf_cntr_4326$DailyCases,
                      labelOptions = labelOptions(noHide = T))

#tmaps####
tmap_mode("view")
tm_shape(covid19_test_county_all_latest_sf_4326)+
  tm_polygons(col = "DailyCases", 
              style = "pretty",
              palette = "Reds",
              alpha = .7)+
  tm_shape(covid19_test_county_all_latest_sf_cntr_4326)+
  tm_text(text = "DailyCases",
          bg.color = "grey",
          shadow = T)
