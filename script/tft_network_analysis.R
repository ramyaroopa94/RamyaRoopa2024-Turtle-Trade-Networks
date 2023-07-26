library(igraph)
library(dplyr)
library(stringr)
library(data.table)
library(magicfor)
library(ggplot2)
library(rnaturalearth)
library(sf)

df <- read.csv("data/tft_network_data.csv", header = T, na.strings = c(""," ","NA"))
str(df)

##Trimming trailing white space in character strings
for(i in 1:ncol(df)){
  if(class(df[,i]) == "character")
  {
    df[,i] <- stringr::str_trim(df[,i], "right")
  } else
    df[,i] <- df[,i]
}

str(df)

##----------------##-------------------------
##Preparing data for SNA 
##Adding column for source location information
df <- df %>% mutate(source_sna = case_when(sour_district != "" ~ paste(sour_district),
                                           sour_country == "India" & is.na(sour_district) ~ paste0("Unspecified-",sour_state),
                                           sour_country != "India" & sour_location != "" ~ paste0(sour_location),
                                           sour_country != "India" & is.na(sour_location) ~ paste0("Unspecified-",sour_country),
                                           TRUE ~ "NA" 
))

##Adding column for last transit location information
df <- df %>% mutate(last_transit_sna = case_when(last_transit_district != "" ~ paste(last_transit_district),
                                                 last_transit_country == "India" & is.na(last_transit_district) ~ paste0("Unspecified-",last_transit_state),
                                                 last_transit_country != "India" & last_transit_location!="" ~ paste0(last_transit_location),
                                                 last_transit_country != "India" & is.na(last_transit_location) ~ paste0("Unspecified-",last_transit_country),
                                                 TRUE ~ "NA"
))

##Adding column for seizure location information
df <- df %>% mutate(seizure_sna = case_when(seiz_district != "" ~ paste0(seiz_district),
                                            seiz_country == "India" & is.na(seiz_district) ~ paste0("Unspecified-",seiz_state),
                                            TRUE ~ "NA"
))

##Adding column for destination 1 location information
df <- df %>% mutate(destination1_sna = case_when(destination1_district != "" ~ paste0(destination1_district),
                                                 destination1_country == "India" & is.na(destination1_district) ~ paste0("Unspecified-",destination1_state),
                                                 destination1_country != "India" & destination1_location != "" ~ paste0(destination1_location),
                                                 destination1_country != "India" & is.na(destination1_location) ~ paste0("Unspecified-",destination1_country),
                                                 TRUE ~ "NA"
))

##Adding column for destination 2 location information
df <- df %>% mutate(destination2_sna = case_when(destination2_district != "" ~ paste0(destination2_district),
                                                 destination2_country == "India" & is.na(destination2_district) ~ paste0("Unspecified-",destination2_state),
                                                 destination2_country != "India" & destination2_location != "" ~ paste0(destination2_location),
                                                 destination2_country != "India" & is.na(destination2_location) ~ paste0("Unspecified-",destination2_country),
                                                 TRUE ~ "NA"
))

##THT (Tortoise and Hardshell Turtle) Network----------------------------------
tht1 <- df %>% filter(group1 == "Tortoise/Hardshell Turtle")
tht2 <- df %>% filter(group2 == "Tortoise/Hardshell Turtle")
tht_combined <- rbind(tht1, tht2)

##Summary of THT data
tht1 %>% summarise(n = n(),sum = sum(group1_number, na.rm = T))
tht2 %>% summarise(n = n(),sum = sum(group2_number, na.rm = T))
tht_combined %>% group_by(seiz_country) %>% summarise(n = n())

#Number of incidents from the 118 incidents, that have from-to connections: 78
tht_with_connections <- tht_combined %>% filter(last_transit_state != ""|last_transit_country != ""|
                                                sour_state != ""|sour_country != ""|
                                                destination1_location != ""|destination1_district != ""|destination1_country != ""|
                                                destination2_location != ""|destination2_district != ""|destination2_country != ""
)

##Creating dataset of illegal trade connections for THTs
##THTs in group1
magic_for(print, silent = TRUE) 
for(i in 1:nrow(tht1)){
  from1 <- tht1$source_sna[i]
  print(from1)
  to1 <- tht1$last_transit_sna[i]
  print(to1)
  quantity1 <- tht1$group1_number[i]
  print(quantity1)
  from2 <- tht1$last_transit_sna[i]
  print(from2)
  to2 <- tht1$seizure_sna[i]
  print(to2)
  quantity2 <- tht1$group1_number[i]
  print(quantity2)
  from3 <- tht1$seizure_sna[i]
  print(from3)
  to3 <- tht1$destination1_sna[i]
  print(to3)
  quantity3 <- tht1$group1_number[i]
  print(quantity3)
  from4 <- tht1$destination1_sna[i]
  print(from4)
  to4 <- tht1$destination2_sna[i]
  print(to4)
  quantity4 <- tht1$group1_number[i]
  print(quantity4)
}

tht1_connections <- magic_result_as_dataframe()

##THTs in group2
magic_for(print, silent = TRUE)

for(i in 1:nrow(tht2)){
  from1 <- tht2$source_sna[i]
  print(from1)
  to1 <- tht2$last_transit_sna[i]
  print(to1)
  quantity1 <- tht2$group2_number[i]
  print(quantity1)
  from2 <- tht2$last_transit_sna[i]
  print(from2)
  to2 <- tht2$seizure_sna[i]
  print(to2)
  quantity2 <- tht2$group2_number[i]
  print(quantity2)
  from3 <- tht2$seizure_sna[i]
  print(from3)
  to3 <- tht2$destination1_sna[i]
  print(to3)
  quantity3 <- tht2$group2_number[i]
  print(quantity3)
  from4 <- tht2$destination1_sna[i]
  print(from4)
  to4 <- tht2$destination2_sna[i]
  print(to4)
  quantity4 <- tht2$group2_number[i]
  print(quantity4)
}

tht2_connections <- magic_result_as_dataframe()
tht_connections_df <- rbind(tht1_connections, tht2_connections)

##Creating the raw network data/edge list
tht_set1 <- select(tht_connections_df, from = from1,to = to1, quantity = quantity1)
tht_set2 <- select(tht_connections_df, from = from2,to = to2, quantity = quantity2)
tht_set3 <- select(tht_connections_df, from = from3,to = to3, quantity = quantity3)
tht_set4 <- select(tht_connections_df, from = from4,to = to4, quantity = quantity4)

tht_sna <- rbind(tht_set1, tht_set2, tht_set3, tht_set4)

##removing links containing 'NA' in from/to columns and from==to
##removing links with ambiguity in state-level node information
tht_sna_clean <- tht_sna %>% filter(from != "NA"& to != "NA") %>% filter(from != to) %>%
  filter(from != "Unspecified-Bihar/Uttar Pradesh" &
         from != "Unspecified-Odisha/Andhra Pradesh" &
         from != "Unspecified-Rajasthan/Haryana" &
         to != "Unspecified-Rajasthan/Haryana")

##List of unique nodes
nodes_from_tht <- tht_sna_clean %>% select(nodes = from)
nodes_to_tht <- tht_sna_clean %>% select(nodes = to)
nodes_unique_tht <- rbind(nodes_from_tht, nodes_to_tht) %>% unique()

nodes_unique_tht <- nodes_unique_tht %>% mutate(state = case_when(nodes == "Chennai"|nodes == "Coimbatore"|nodes == "Madurai"|
                                                                  nodes == "Thoothukudi"|nodes == "Tiruchirappalli" ~ "Tamil Nadu",
                                                                  nodes == "Chittoor"|nodes == "Krishna"|nodes == "Unspecified-Andhra Pradesh"|
                                                                  nodes == "Anantapur"|nodes == "Visakhapatnam"|nodes == "Tirupati" ~ "Andhra Pradesh",
                                                                  nodes == "Kolkata"|nodes == "North 24 Parganas"|nodes == "Howrah"|
                                                                  nodes == "Unspecified-West Bengal"|nodes == "Paschim Burdwan" ~ "West Bengal",
                                                                  nodes == "Agra"|nodes == "Unspecified-Uttar Pradesh"|nodes == "Lucknow"|
                                                                  nodes == "Jhansi"|nodes == "Allahabad"|nodes == "Meerut"|nodes == "Firozabad"|
                                                                  nodes == "Sultanpur"|nodes == "Raebareli"|nodes == "Varanasi"|
                                                                  nodes == "Bareilly"|nodes == "Kanpur Nagar"|nodes == "Etawah" ~ "Uttar Pradesh",
                                                                  nodes == "Mumbai"|nodes == "Pune"|nodes == "Nagpur"|nodes == "Thane" ~ "Maharashtra",
                                                                  nodes == "Indore"|nodes == "Dewas" ~ "Madhya Pradesh", nodes == "Udham Singh Nagar" ~ "Uttarakhand",
                                                                  nodes == "Delhi" ~ "Delhi",nodes == "Dimapur" ~ "Nagaland",nodes == "Khordha" ~ "Odisha",
                                                                  nodes == "Bengaluru"|nodes == "Chikkaballapur"|nodes == "Unspecified-Karnataka" ~ "Karnataka",
                                                                  nodes == "Thiruvananthapuram"|nodes == "Kochi"|nodes == "Kozhikode" ~ "Kerala",
                                                                  nodes == "Ahmedabad"|nodes == "Surat"|nodes == "Vadodara" ~ "Gujarat",
                                                                  nodes == "Patna"|nodes == "Gaya"|nodes == "Rohtas" ~ "Bihar",
                                                                  TRUE ~ "International"))

nodes_unique_tht <- nodes_unique_tht %>% arrange(state)
nodes_unique_tht$ID <- seq(1:nrow(nodes_unique_tht))

tht_network_df <- tht_sna_clean %>% group_by(from, to) %>% summarise(number_incidents = n(),weight = sum(quantity, na.rm = TRUE))

##Writing tht_network_df into an excel file
write.excel <- function(tht_network_df,row.names=FALSE,col.names=TRUE,...) {
  write.table(tht_network_df,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(tht_network_df)

##Creating the igraph object
tht_network <- graph_from_data_frame(d = tht_network_df, vertices = nodes_unique_tht, directed=TRUE) 

##E: edges or links and V: vertices or nodes
E(tht_network)
V(tht_network)
vertex_attr(tht_network) ##vertex attributes 
edge_attr(tht_network)

##SNA measures for THTs---------------
##Graph-level centrality measures for THTs--------------
round(centr_degree(tht_network, mode = "in",loops = FALSE, normalized = TRUE)$centralization,3)
round(centr_degree(tht_network, mode = "out",loops = FALSE, normalized = TRUE)$centralization,3)
round(mean(degree(tht_network)),3)
mean(degree(tht_network,mode = "in")) ##half of mean degree
mean(degree(tht_network,mode = "out")) ##half of mean degree
round(reciprocity(tht_network),3)
round(edge_density(tht_network, loops = FALSE),3) #number of realized links/number of potential links
centr_betw(tht_network, directed = TRUE)

##Vertex-level centrality measures for THTs--------------
##degree centrality 
tht_measures <- data.frame(degree(tht_network, v = V(tht_network), mode = "in", loops = FALSE, normalized = FALSE)) #in degree
tht_measures <- setDT(tht_measures, keep.rownames = "node")
tht_measures$in_degree_nor <- degree(tht_network, v = V(tht_network), mode = "in", loops = FALSE, normalized = TRUE) #in degree normalized
tht_measures$out_degree <- degree(tht_network, v = V(tht_network), mode = "out", loops = FALSE, normalized = FALSE) #out degree 
tht_measures$out_degree_nor <- degree(tht_network, v = V(tht_network), mode = "out", loops = FALSE, normalized = TRUE) #out degree normalized
tht_measures$all_degree <- degree(tht_network, v = V(tht_network), mode = "all", loops = FALSE, normalized = FALSE) #all degree 
tht_measures$all_degree_nor <- degree(tht_network, v = V(tht_network), mode = "all", loops = FALSE, normalized = TRUE) #all degree normalized

##Strength centrality 
tht_measures$in_strength <- data.frame(graph.strength(tht_network, mode = "in")) #in strength
tht_measures$out_strength <- graph.strength(tht_network, mode = "out") #out strength 
tht_measures$all_strength <- graph.strength(tht_network) #all strength 
colnames(tht_measures)[2] <- "in_degree"

##Betweenness centrality 
tht_network_betw <- delete_edge_attr(tht_network, "weight")
tht_measures$betweenness <- betweenness(tht_network_betw, directed = T)

write.excel <- function(tht_measures,row.names=TRUE,col.names=TRUE,...) {
  write.table(tht_measures,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(tht_measures)

##Visualizations in network space for THTs---------------------
##circular layout
l_circle_tht <- layout_in_circle(tht_network)
values_tht <- Map(c, tht_measures$in_degree, tht_measures$out_degree)

##Function for aligning labels around the edge
radian_rescale <- function(x, start = 0, direction = 1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}
lab_locs_tht <- radian_rescale(x = 1:length(V(tht_network)), direction = -1, start = 0)

##Light gray = Incoming links, Dark gray = Outgoing links
##Unweighted nodes and edges
tht_graph_unweighted_nodes <- plot(tht_network, edge.color = "gray17", edge.arrow.size = 0.4, edge.curved = 0.2, 
                                   vertex.color = adjustcolor("red", alpha.f = 0.6),
                                   vertex.frame.color = "gray17", vertex.label = V(tht_network)$name,
                                   vertex.label.color = "black", vertex.label.cex = 0.4,
                                   vertex.label.degree = pi/2,
                                   vertex.size = 4,
                                   vertex.label.degree = lab_locs_tht, vertex.label.dist = 0.5,
                                   layout = l_circle_tht)
tht_graph_unweighted_nodes

##Weighted nodes and edges (edges weighted by number of incidents)
tht_graph_weighted_nodesedges <- plot(tht_network, edge.color = "black", edge.arrow.size = 0.4, edge.curved = 0.2, 
                                      edge.width = E(tht_network)$number_incidents,
                                      vertex.shape = "pie", vertex.pie = values_tht, vertex.pie.color = list(c("#666666", "#cccccc")),
                                      vertex.frame.color = "black",vertex.label = V(tht_network)$name,
                                      vertex.label.color = "black",vertex.label.cex = 0.6,
                                      vertex.size = tht_measures$all_degree,
                                      vertex.label.degree = lab_locs_tht, vertex.label.dist = 0.5,
                                      layout = l_circle_tht)
tht_graph_weighted_nodesedges

##Weighted nodes and edges, without node labels
tiff(filename = "figures/raw/tht_network_chart_unlabelled.tif",
    width = 17.1, height = 17.1, units = "cm", res = 900)
tht_graph_weighted_nolabels <- plot(tht_network, edge.color = "black", edge.arrow.size = 0.5, edge.curved = 0.2, 
                                      edge.width = E(tht_network)$number_incidents*0.8,
                                      vertex.shape = "pie", vertex.pie = values_tht, vertex.pie.color = list(c("#666666", "#cccccc")),
                                      vertex.frame.color = "black",
                                      vertex.label = NA,
                                      vertex.size = tht_measures$all_degree*1.1,
                                      layout = l_circle_tht)
dev.off()

##Histograms of in degree and out-degree
ggplot(data = tht_measures,aes(x = tht_measures$in_degree)) +
  geom_histogram(breaks = seq(min(tht_measures$in_degree), max(tht_measures$in_degree), by = 1),
                 col = "grey17", fill = "grey", alpha = 0.7) +
  theme_classic() +
  xlab("Indegree") + 
  ylab("Frequency")

ggplot(data = tht_measures, aes(x = tht_measures$out_degree)) +
  geom_histogram(breaks = seq(min(tht_measures$out_degree), max(tht_measures$out_degree), by = 1), 
                 col = "grey17", fill = "grey", alpha = 0.7) +
  theme_classic() +
  xlab("Outdegree") + 
  ylab("Frequency")

##Visualizing THT network on map----------------
##Submitted version made in R, final publication version made in QGIS
nodelist_tht_coords <- read.csv("data/tht_coords_manual.csv")

##removing trailing white space
nodelist_tht_coords$nodes[nodelist_tht_coords$nodes == "Unspecified-Malaysia "] <- "Unspecified-Malaysia"
nodelist_tht_coords$nodes[nodelist_tht_coords$nodes == "Unspecified-Uttar Pradesh "] <- "Unspecified-Uttar Pradesh"
nodelist_tht_coords$nodes[nodelist_tht_coords$nodes == "Unspecified-West Bengal "] <- "Unspecified-West Bengal"
table(nodelist_tht_coords$nodes)

##Merging coordinates with network dataframe
tht_network_coords <- tht_network_df %>% merge(nodelist_tht_coords, by.x = "from", by.y = "nodes") %>% 
  rename(x = longitude, y = latitude) %>%
  merge(nodelist_tht_coords, by.x = "to", by.y = "nodes") %>% 
  rename(xend = longitude, yend = latitude) %>%
  select(from, to, number_incidents, weight, y, x, yend, xend)

write.csv(tht_network_coords, "tht_flow_coordinates.csv")

##Network representation on map
india_states <- sf::st_read("data/Updated_India_State_Boundary_5May2020.shp") 
india_states_df <- fortify(india_states)
india_states_shape <- geom_sf(data = india_states_df, color = "grey47",
                              fill = "#f2f2f2", linewidth = 0.2) 
countries <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf", continent = "asia")

tht_map_main <- ggplot() +
  geom_sf(data = countries, color = "grey65", fill = "white", size = 0.2) +
  coord_sf(xlim = c(53, 115), ylim = c(-8, 38), expand = FALSE) +
  ggspatial::annotation_scale(line_width = 0.5, height = unit(0.15, "cm"), text_cex = 0.5) +
  india_states_shape +
  geom_point(data = nodelist_tht_coords, aes(x = longitude, y = latitude), size = 0.8, colour ="darkblue", show.legend = FALSE) +
  geom_curve(data = tht_network_coords,aes(x = x, y = y, xend = xend, yend = yend), color="red", size = 0.2, curvature = 0.33, show.legend = FALSE, arrow = arrow(length = unit(0.010, "npc"))) +
  labs(y = "Latitude", x = "Longitude") +
  theme_void()

tht_map_supp <- ggplot() +
  geom_sf(data = countries, color = "grey65", fill = "white", size = 0.2) +
  ggspatial::annotation_scale(line_width = 0.5, height = unit(0.15, "cm"), text_cex = 0.5) +
  coord_sf(xlim = c(53, 115), ylim = c(-8, 38), expand = FALSE) +
  india_states_shape +
  geom_point(data = nodelist_tht_coords, aes(x = longitude, y = latitude), size = 0.8, colour = "darkblue", show.legend = FALSE) +
  labs(y = "Latitude", x="Longitude") +
  theme_void()

tht_map_supp_labelled <- ggplot() +
  geom_sf(data = countries, color = "grey65", fill = "white", size = 0.2) +
  ggspatial::annotation_scale(line_width = 0.5, height = unit(0.15, "cm"), text_cex = 0.5) +
  coord_sf(xlim = c(53, 115), ylim = c(-8, 38), expand = FALSE) +
  india_states_shape +
  geom_point(data = nodelist_tht_coords, aes(x = longitude, y = latitude), size = 0.8, colour = "darkblue", show.legend = FALSE) +
  geom_text(data = nodelist_tht_coords, aes(x = longitude, y = latitude, label = nodes), colour = "red", size = 1, hjust = 0, vjust = 0) +
  labs(y = NULL, x = NULL) +
  theme_void()

##Softshell Turtle (ST) Network----------------------------------
st1 <- df %>% filter(group1 == "Softshell Turtle") %>% filter(product == "Live")
st2 <- df %>% filter(group2 == "Softshell Turtle") %>% filter(product == "Live")
st_combined <- rbind(st1, st2)

##Summary of ST data
st1 %>% summarise(n = n(), sum = sum(group1_number, na.rm = T))
st2 %>% summarise(n = n(), sum = sum(group2_number, na.rm = T))
st_combined %>% group_by(seiz_country) %>% summarise(n = n())

#Number of incidents from the 103 incidents, that have from-to connections: 64
st_with_connections <- st_combined %>% filter(last_transit_state != ""|last_transit_country != ""|
                                              sour_state != ""|sour_country != ""|
                                              destination1_location != ""|destination1_district != ""|destination1_country != ""|
                                              destination2_location != ""|destination2_district != ""|destination2_country != ""
)

##Getting the dataset of trade connections for STs
magic_for(print, silent = TRUE)
##STs in group 1
for(i in 1:nrow(st1)){
  from1 <- st1$source_sna[i]
  print(from1)
  to1 <- st1$last_transit_sna[i]
  print(to1)
  quantity1 <- st1$group1_number[i]
  print(quantity1)
  from2 <- st1$last_transit_sna[i]
  print(from2)
  to2 <- st1$seizure_sna[i]
  print(to2)
  quantity2 <- st1$group1_number[i]
  print(quantity2)
  from3 <- st1$seizure_sna[i]
  print(from3)
  to3 <- st1$destination1_sna[i]
  print(to3)
  quantity3 <- st1$group1_number[i]
  print(quantity3)
  from4 <- st1$destination1_sna[i]
  print(from4)
  to4 <- st1$destination2_sna[i]
  print(to4)
  quantity4 <- st1$group1_number[i]
  print(quantity4)
}

st1_connections <- magic_result_as_dataframe()

##STs in group 2
magic_for(print, silent = TRUE)
for(i in 1:nrow(st2)){
  from1 <- st2$source_sna[i]
  print(from1)
  to1 <- st2$last_transit_sna[i]
  print(to1)
  quantity1 <- st2$group2_number[i]
  print(quantity1)
  from2 <- st2$last_transit_sna[i]
  print(from2)
  to2 <- st2$seizure_sna[i]
  print(to2)
  quantity2 <- st2$group2_number[i]
  print(quantity2)
  from3 <- st2$seizure_sna[i]
  print(from3)
  to3 <- st2$destination1_sna[i]
  print(to3)
  quantity3 <- st2$group2_number[i]
  print(quantity3)
  from4 <- st2$destination1_sna[i]
  print(from4)
  to4 <- st2$destination2_sna[i]
  print(to4)
  quantity4 <- st2$group2_number[i]
  print(quantity4)
}

st2_connections <- magic_result_as_dataframe()
st_connections_df <- rbind(st1_connections, st2_connections)

##Creating the raw network data/edge list for sts
st_set1 <- select(st_connections_df, from = from1, to = to1, quantity = quantity1)
st_set2 <- select(st_connections_df, from = from2, to = to2, quantity = quantity2)
st_set3 <- select(st_connections_df, from = from3, to = to3, quantity = quantity3)
st_set4 <- select(st_connections_df, from = from4, to = to4, quantity = quantity4)

st_sna <- rbind(st_set1, st_set2, st_set3, st_set4)

##removing rows containing 'NA' in from/to columns and from == to
##removing rows with ambiguity in node information
st_sna_clean <- st_sna %>% filter(from != "NA" & to != "NA") %>% filter(from != to) %>%
  filter(from != "Amethi/Pratapgarh" & from != "Unspecified-Bihar/Uttar Pradesh" &
         to != "Unspecified-Odisha and West Bengal" & to != "Unspecified-Bangladesh, Myanmar, China, Thailand, Hong Kong and other SE Asian Countries") %>%
  mutate(from = recode(from,"Purba Burdwan" = "Purba Bardhaman",
                            "Paschim Burdwan" = "Paschim Bardhaman"),
         to = recode(to,"Purba Burdwan" = "Purba Bardhaman",
                        "Paschim Burdwan" = "Paschim Bardhaman",
                        "North 24-Parganas" = "North 24 Parganas"))

##Getting the node list and unique nodes
nodes_from_st <- st_sna_clean %>% select(nodes = from)
nodes_to_st <- st_sna_clean %>% select(nodes = to)
nodes_unique_st <- rbind(nodes_from_st, nodes_to_st) %>% unique()

nodes_unique_st <- nodes_unique_st %>% mutate(state = case_when(nodes == "Amethi"|nodes == "Raebareli"|nodes == "Varanasi"|nodes == "Sultanpur"|nodes == "Allahabad"|nodes == "Unspecified-Uttar Pradesh"|
                                                                nodes == "Auraiya"|nodes == "Pratapgarh"|nodes == "Fatehpur"|
                                                                nodes == "Lucknow"|nodes == "Jaunpur"|nodes == "Faizabad"|nodes == "Bareilly"|nodes == "Chandauli"|nodes == "Mainpuri"|
                                                                nodes == "Ambedkar Nagar"|nodes == "Agra"|nodes == "Etawah"|nodes == "Gonda" ~ "Uttar Pradesh",
                                                                nodes == "Unspecified-West Bengal"|nodes == "Kolkata"|nodes == "North 24 Parganas"|nodes == "Howrah"|nodes == "Purba Bardhaman"|
                                                                nodes == "Paschim Bardhaman"|nodes == "Hooghly"|nodes == "Paschim Medinipur"|nodes == "Nadia"|nodes == "Malda" ~ "West Bengal",
                                                                nodes == "Delhi" ~ "Delhi",
                                                                nodes == "Khordha"|nodes == "Puri"|nodes == "Sambalpur"|nodes == "Cuttack"|nodes == "Malkangiri"|nodes == "Balasore"|nodes == "Unspecified-Odisha" ~ "Odisha",
                                                                nodes == "Visakhapatnam"|nodes == "Unspecified-Andhra Pradesh"|nodes == "East Godavari"|nodes == "Krishna" ~ "Andhra Pradesh",
                                                                nodes == "Kasaragod" ~ "Kerala", nodes == "Dakshina Kannada" ~ "Karnataka", nodes == "Unspecified-Rajasthan" ~ "Rajasthan",
                                                                nodes == "Rohtas"|nodes == "Gaya"|nodes == "Patna"|nodes == "Unspecified-Bihar" ~ "Bihar",
                                                                nodes == "West Tripura" ~ "Tripura",
                                                                nodes == "Udham Singh Nagar"|nodes == "Nainital" ~ "Uttarakhand",
                                                                nodes == "Sahibganj"|nodes == "Dumka"|nodes == "Dhanbad" ~ "Jharkhand",
                                                                TRUE ~ "International"))

nodes_unique_st <- nodes_unique_st %>% arrange(state)
nodes_unique_st$ID <- seq(1:nrow(nodes_unique_st))

##Writing nodes_unique_st
write.excel <- function(nodes_unique_st, row.names=FALSE, col.names=TRUE,...) {
  write.table(nodes_unique_st,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(nodes_unique_st)

st_network_df <- st_sna_clean %>% group_by(from, to) %>% summarise(number_incidents = n(), weight = sum(quantity,na.rm = TRUE))

##Writing st_network_df
write.excel <- function(st_network_df, row.names=FALSE, col.names=TRUE,...) {
  write.table(st_network_df, "clipboard", sep="\t", row.names=row.names, col.names = col.names,...)
}

write.excel(st_network_df)

##Creating the igraph object
st_network <- graph_from_data_frame(d = st_network_df, vertices = nodes_unique_st, directed = TRUE) 

##E: edges or links and V: vertices or nodes
E(st_network)
V(st_network)
vertex_attr(st_network)##vertex attributes 
edge_attr(st_network)

##SNA measures for STs---------------
##Graph-level centrality measures for STs--------------
round(centr_degree(st_network, mode = "in",loops = FALSE, normalized = TRUE)$centralization,3)
round(centr_degree(st_network, mode = "out",loops = FALSE, normalized = TRUE)$centralization,3)
round(centr_degree(st_network, mode = "all",loops = FALSE, normalized = TRUE)$centralization,3)
round(mean(degree(st_network)),3)
mean(degree(st_network,mode = "in")) ##half of mean degree
mean(degree(st_network,mode = "out")) ##half of mean degree
round(reciprocity(st_network),3)
round(edge_density(st_network, loops = FALSE),3)
centr_betw(st_network, directed = TRUE)

##Vertex-level centrality measures--------------
st_measures <- data.frame(degree(st_network, v = V(st_network), mode = "in", loops = FALSE, normalized = FALSE))
st_measures <- setDT(st_measures, keep.rownames = "node")
st_measures$in_degree_nor <- degree(st_network, v = V(st_network), mode = "in", loops = FALSE, normalized = TRUE)
st_measures$out_degree <- degree(st_network, v = V(st_network), mode = "out", loops = FALSE, normalized = FALSE)
st_measures$out_degree_nor <- degree(st_network, v = V(st_network), mode = "out", loops = FALSE, normalized = TRUE)
st_measures$all_degree <- degree(st_network, v = V(st_network), mode = "all", loops = FALSE, normalized = FALSE) 
st_measures$all_degree_nor <- degree(st_network, v = V(st_network), mode = "all", loops = FALSE, normalized = TRUE)
st_measures$in_strength <- strength(st_network, vids = V(st_network),mode = "in")
st_measures$out_strength <- strength(st_network, mode = "out")
st_measures$all_strength <- strength(st_network, mode="all")
colnames(st_measures)[2] <- "in_degree"

##Betweenness centrality 
st_network_betw <- delete_edge_attr(st_network, "weight")
st_measures$betweenness <- betweenness(st_network_betw, directed = T)

write.excel <- function(st_measures,row.names=TRUE,col.names=TRUE,...) {
  write.table(st_measures,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

write.excel(st_measures)

##Visualizations in network space for Softshells---------------------
##Vizualizing based on degree (all links, normalized) of each node
l_circle_st <- layout_in_circle(st_network)
values_st <- Map(c, st_measures$in_degree, st_measures$out_degree)

# generate label positions and graph
radian.rescale <- function(x, start = 0, direction = 1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(scales::rescale(x, c(0, 2 * pi), range(x)))
}

lab_locs_st <- radian.rescale(x = 1:length(V(st_network)), direction = -1, start = 0)

##Dark gray = Import links, Light gray = export links
##Unweighted nodes and edges
st_graph_unweighted_nodes <- plot(st_network, edge.color = "gray17", edge.arrow.size = 0.4, edge.curved = 0.2, 
                                  vertex.color = adjustcolor("red", alpha.f = 0.6),
                                  vertex.frame.color = "gray17",vertex.label = V(st_network)$name,
                                  vertex.label.color = "black",vertex.label.cex = 0.4,
                                  vertex.label.degree = pi/2,
                                  vertex.size = 4,
                                  vertex.label.degree = lab_locs_st, vertex.label.dist = 0.5,
                                  layout = l_circle_st)

##Weighted nodes and edges (edges weighted by number of incidents)
st_graph_weighted_nodesedges <- plot(st_network, edge.color = "gray17", edge.arrow.size = 0.4, edge.curved = 0.2, 
                                     edge.width = E(st_network)$number_incidents,
                                     vertex.shape = "pie", vertex.pie = values_st, vertex.pie.color = list(c("#666666", "#cccccc")),
                                     vertex.frame.color = "gray17",vertex.label = V(st_network)$name,
                                     vertex.label.color = "black",vertex.label.cex = 0.6,
                                     vertex.size = st_measures$all_degree,
                                     vertex.label.degree = lab_locs_st, vertex.label.dist = 0.5,
                                     layout = l_circle_st)

##Weighted nodes and edges, without node labels
tiff(filename = "figures/raw/st_network_chart_unlabelled.tif",
    width = 17.1, height = 17.1, units = "cm", res = 900)
st_graph_weighted_nolabels <- plot(st_network, edge.color = "black", edge.arrow.size = 0.5, edge.curved = 0.2, 
                                     edge.width = E(st_network)$number_incidents*0.8,
                                     vertex.shape = "pie", vertex.pie = values_st, vertex.pie.color = list(c("#666666", "#cccccc")),
                                     vertex.frame.color = "black",vertex.label = NA,
                                     vertex.size = st_measures$all_degree,
                                     layout = l_circle_st)
dev.off()

##Visualizing st network on map----------------
nodelist_st_coords <- read.csv("data/st_coords_manual.csv")

##removing trailing white space
nodelist_st_coords$nodes[nodelist_st_coords$nodes == "Unspecified-Odisha "] <- "Unspecified-Odisha"
nodelist_st_coords$nodes[nodelist_st_coords$nodes == "Unspecified-Rajasthan "] <- "Unspecified-Rajasthan"
nodelist_st_coords$nodes[nodelist_st_coords$nodes == "Unspecified-Uttar Pradesh "] <- "Unspecified-Uttar Pradesh"
nodelist_st_coords$nodes[nodelist_st_coords$nodes == "Unspecified-West Bengal "] <- "Unspecified-West Bengal"

##Merging coordinates with network dataframe
st_network_coords <- st_network_df %>% merge(nodelist_st_coords, by.x = "from", by.y = "nodes") %>% 
  rename(x = longitude, y = latitude) %>%
  merge(nodelist_st_coords, by.x = "to", by.y = "nodes") %>% 
  rename(xend = longitude, yend = latitude) %>%
  select(from, to, number_incidents, weight, y, x, yend, xend)

write.csv(st_network_coords, "st_flow_coordinates.csv")

##Map representation of network (unlabelled)
st_map_main <- ggplot() +
  geom_sf(data = countries,color = "grey65",fill = "white", size = 0.2) +
  coord_sf(xlim = c(64, 100), ylim = c(5, 38), expand = FALSE) +
  ggspatial::annotation_scale(line_width = 0.5, height = unit(0.15, "cm"), text_cex = 0.5) +
  india_states_shape +
  geom_point(data = nodelist_st_coords, aes(x = longitude, y = latitude), size = 0.8, colour ="darkblue", show.legend = FALSE) +
  geom_curve(data = st_network_coords, aes(x = x, y = y, xend = xend, yend = yend), color="red", size = 0.2, curvature = 0.33, show.legend = FALSE, arrow = arrow(length = unit(0.010, "npc"))) +
  labs(y = "Latitude", x = "Longitude")+
  theme_void()

##Supplementary - Map representation of nodes (unlabelled)
st_map_supp <- ggplot() +
  geom_sf(data = countries,color = "grey65",fill = "white", size = 0.2) +
  ggspatial::annotation_scale(line_width = 0.5, height = unit(0.15, "cm"), text_cex = 0.5) +
  coord_sf(xlim = c(64, 100), ylim = c(5, 38), expand = FALSE) +
  india_states_shape +
  geom_point(data = nodelist_st_coords, aes(x = longitude, y = latitude), size = 0.8, colour ="darkblue", show.legend = FALSE) +
  labs(y="Latitude", x="Longitude") +
  theme_void()

##Supplementary - Map representation of nodes (labelled)
st_map_supp_labelled <- ggplot() +
  geom_sf(data = countries,color = "grey65",fill = "white", size = 0.2) +
  ggspatial::annotation_scale(line_width = 0.5,height = unit(0.15, "cm"),text_cex = 0.5) +
  coord_sf(xlim = c(64, 100),ylim = c(5, 38),expand = FALSE) +
  india_states_shape +
  geom_point(data = nodelist_st_coords, aes(x = longitude, y = latitude), size = 0.8, colour = "darkblue", show.legend = FALSE) +
  geom_text(data = nodelist_st_coords, aes(x = longitude, y = latitude, label = nodes), colour = "red", size = 1, hjust = 0, vjust = 0) +
  labs(y = "Latitude", x = "Longitude") +
  theme_void()
