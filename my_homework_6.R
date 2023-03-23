library(ggthemes)
library(sf)
library(ggmap)
library(tmap)
library(tidycensus)
library(leaflet)
library(osmdata)
library(tigris)
library(tidyr)
library(dplyr)
library(forcats)
library(stringr)
library(gt)
library(purrr)
library(plotly)

#Creating a Leaflet

available_tags("shop")


#Let's grab the liquor stores and hunting stores
osm_shop_sf.df <- opq(bbox = "hennepin") %>%
  add_osm_feature(key = "shop", value = c("coffee", "bakery", "gift")) %>%
  osmdata_sf()

#Extract the relevent sf data frame
shop_points.df <- osm_shop_sf.df$osm_points %>% 
  janitor::clean_names() %>%
  filter(!is.na(shop)) %>% #only retain 'valid' tags
  dplyr::select(osm_id, name, shop, opening_hours, phone, website, geometry) %>%
  mutate(shop = str_to_title(shop) %>% as_factor())

#Check it out (minus geometry for display)
shop_points.df %>%
  as_tibble() %>%
  dplyr::select(-geometry) %>%
  gt_preview()

#Get the bounding box and county outline
mn.box           <- osmdata::getbb("minnesota")
hennepin.box     <- osmdata::getbb("hennepin")
hennepin.outline <- osmdata::getbb("hennepin", format_out = "polygon")[[1]] %>%
  as_tibble() %>%
  rename(longitude = V1, latitude = V2)

#Get the base map (foundational layer)
mn_base.map <- get_map(
  location = hennepin.box,
  source   = "stamen",
  maptype  = "terrain",
  crop = TRUE
)

hennepin_base.gg <- ggmap(mn_base.map) +
  geom_polygon(data = hennepin.outline, aes(x = longitude, y = latitude), colour = "black", size = 1.6, alpha = 0.1) +
  theme_map() +
  theme(
    plot.title   = element_text(size  = 16,
                                hjust = 0.5),
    legend.text  = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.position = "right"
  )

hennepin_base.gg

#Call viridis library  
library(viridis, quietly = TRUE)

#Extract the levels/order of shop type factor
shop_types <- levels(shop_points.df$shop)
n_types    <- length(shop_types)

#Initialize our colour pallete (discrete in this case)
my_pallete <- colorFactor(viridis_pal(option = "D")(n_types), levels = shop_types)

#Call our leaflet
leaflet(data = shop_points.df) %>%
  addProviderTiles('CartoDB.Positron') %>% 
  addCircleMarkers(
    color   = ~my_pallete(shop),
    opacity = 0.4,
    weight  = 2, #outline strength
    radius  = 4 #size of circle
  ) %>%
  addLegend(
    title    = "Store Type",
    position = "bottomright",
    colors   = viridis_pal(option = "D")(n_types),
    labels   = shop_types 
  )

#Need html tools to get our labels to render appropriately
library(htmltools, quietly = TRUE)

#Add a text label like normal
shop_label.df <- shop_points.df %>%
  mutate(
    across(
      c(name, opening_hours:website),
      ~ifelse(is.na(.x), "NA", .x)
    ),
    text_label = str_c(
      name,
      "<br/>Store Type: ",
      shop,
      "<br/>Open Hours: ",
      opening_hours,
      "<br/>Phone: ",
      phone,
      "<br/>Website: ",
      website
    )
  )

shop_label.df %>%
  leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>% 
  addCircleMarkers(
    color   = ~my_pallete(shop),
    label   = ~map(text_label, HTML), #map over labels, make html
    opacity = 0.6, #alpha
    weight  = 4, #outline strength
    radius  = 4 #size of circle
  ) %>%
  addLegend(
    title    = "Store Type",
    position = "bottomright",
    colors   = viridis_pal(option = "D")(n_types),
    labels   = shop_types 
  )

#----------------------------------------------------

#GGplot

HC_gini.df <- get_acs(
  geography = "tract",
  variables = "B19083_001", #Code for gini index
  state     = "MN",
  county    = "Hennepin",
  year      = 2020,
  geometry  = TRUE)

HC_gini_plotly.df <- HC_gini.df %>%
  mutate(
    tract      = str_split(NAME, ",") %>%
      map_chr(1) %>%
      str_remove("Census Tract "),
    text_label = str_c(
      "Tract: ",
      tract,
      "\nGini: ",
      (estimate)
    )
  )

gini.gg <- ggplot() + 
  geom_sf(data = HC_gini.df, aes(fill = estimate)) + 
  labs(title = "Hennepin County 2020 ACS Estimated Gini Index", subtitle = "Where 0.3 indicates reasonable income gap and >0.5 indicates severe income disparity") + 
  theme_void() + 
  scale_fill_viridis_c("Gini Index")+
  theme_map() +
  theme(
    plot.title   = element_text(size  = 16,
                                hjust = 0.5),
    legend.text  = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.position = "right"
  )

#Display
gini.gg

ggplotly(gini.gg,
         data=HC_gini_plotly.df,
         tooltip = "text_label",
         height  = 600,
         width   = 800) %>%
  style(hoveron = "fills")

#ggmap example 3
mn.box           <- osmdata::getbb("minnesota")
hennepin.box     <- osmdata::getbb("hennepin")
hennepin.outline <- osmdata::getbb("hennepin", format_out = "polygon")[[1]] %>%
  as_tibble() %>%
  rename(longitude = V1, latitude = V2)

mn_base.map <- get_map(
  location = hennepin.box,
  source   = "stamen",
  maptype  = "terrain",
  crop = TRUE
)

hennepin_base.gg <- ggmap(mn_base.map) +
  geom_polygon(data = hennepin.outline, aes(x = longitude, y = latitude), colour = "black", size = 1.6, alpha = 0.1) +
  theme_map() +
  theme(
    plot.title   = element_text(size  = 16,
                                hjust = 0.5),
    legend.text  = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.position = "right"
  )

hennepin_base.gg

coord_ggmap <- st_crs(hennepin_base.gg) #NA
coord_sf    <- st_crs(HC_gini_plotly.df) #NAD83

mn_gini.ggmap <- hennepin_base.gg +  
  geom_sf(data = HC_gini_plotly.df, 
          aes(fill = estimate, text = text_label),
          colour = "black", size = 0.1,
          inherit.aes = FALSE) + 
  labs(title = "Hennepin County, MN 2020 ACS Gini Index") + 
  scale_fill_viridis_c("Gini Index") +
  theme_map() +
  theme(
    plot.title   = element_text(size  = 16,
                                hjust = 0.5),
    legend.text  = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.position = "right"
  )



ggplotly(mn_gini.ggmap,
         tooltip = "text",
         height  = 600,
         width   = 800) %>%
  style(hoveron = "fills")



#_____________________

#coming up with something

HC_rent.df <- get_acs(
  geography = "tract",
  variables = "B25064_001", #Code for educational attainment
  state     = "MN",
  county    = "Hennepin",
  year      = 2020,
  geometry  = TRUE)

HC_rent_plotly.df <- HC_rent.df %>%
  mutate(
    tract      = str_split(NAME, ",") %>%
      map_chr(1) %>%
      str_remove("Census Tract "),
    text_label = str_c(
      "Tract: ",
      tract,
      "\nMedian Rent: ",
      scales::dollar(estimate)
    )
  )

rent.gg <- ggplot() + 
  geom_sf(data = HC_rent.df, aes(fill = estimate)) + 
  labs(title = "Hennepin County 2020 Median Rent") + 
  theme_void() + 
  scale_fill_viridis_c("Median Rent", labels = scales::dollar)+
  theme_map() +
  theme(
    plot.title   = element_text(size  = 16,
                                hjust = 0.5),
    legend.text  = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.position = "right"
  )

#Display
rent.gg

ggplotly(rent.gg,
         data=HC_rent_plotly.df,
         tooltip = "text_label",
         height  = 600,
         width   = 800) %>%
  style(hoveron = "fills")

mn.box           <- osmdata::getbb("minnesota")
hennepin.box     <- osmdata::getbb("hennepin")
hennepin.outline <- osmdata::getbb("hennepin", format_out = "polygon")[[1]] %>%
  as_tibble() %>%
  rename(longitude = V1, latitude = V2)

mn_base.map <- get_map(
  location = hennepin.box,
  source   = "stamen",
  maptype  = "terrain",
  crop = TRUE
)

hennepin_base.gg <- ggmap(mn_base.map) +
  geom_polygon(data = hennepin.outline, aes(x = longitude, y = latitude), colour = "black", size = 1.6, alpha = 0.1) +
  theme_map() +
  theme(
    plot.title   = element_text(size  = 16,
                                hjust = 0.5),
    legend.text  = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.position = "right"
  )

hennepin_base.gg

coord_ggmap <- st_crs(hennepin_base.gg) #NA
coord_sf_rent    <- st_crs(HC_rent_plotly.df) #NAD83

mn_rent.ggmap <- hennepin_base.gg +  
  geom_sf(data = HC_rent_plotly.df, 
          aes(fill = estimate, text = text_label),
          colour = "black", size = 0.1,
          inherit.aes = FALSE) + 
  labs(title = "Hennepin County, MN 2020 Median Rent") + 
  scale_fill_viridis_c("Median Rent", labels = scales::dollar) +
  theme_map() +
  theme(
    plot.title   = element_text(size  = 16,
                                hjust = 0.5),
    legend.text  = element_text(size = 16),
    legend.title = element_text(size = 16),
    legend.position = "right"
  )



ggplotly(mn_rent.ggmap,
         tooltip = "text",
         height  = 600,
         width   = 800) %>%
  style(hoveron = "fills")
