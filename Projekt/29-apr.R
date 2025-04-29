set.seed(07042025)

library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)


## Create edge list
edge_list <- read_csv("Projekt/alliance_v4.1_by_dyad_yearly.csv") |>
  rename(
    from = ccode1,
    to = ccode2,
    from_name = state_name1,
    to_name = state_name2,
    begin = dyad_st_year,
    end = dyad_end_year
  ) |>
  select(-nonaggression) |>
  filter(defense == 1 | neutrality == 1 | entente == 1) |>
  select(year, from, to, from_name, to_name) |>
  filter(year >= 1950, year <= 2000) |>
  distinct() |>  # Remove any duplicate edges
  arrange(year, from, to)

## Create node list
node_list <- edge_list |>
  # Get unique nodes from both 'from' and 'to' columns
  select(id = from, name = from_name) |>
  bind_rows(select(edge_list, id = to, name = to_name)) |>
  distinct() |>  # Remove duplicates
  arrange(id)

coi <- c(
  "United Kingdom",
  "United States of America",
  "Canada",
  "Denmark",
  "Norway",
  "Germany",
  "Luxembourg",
  "Netherlands",
  "Belgium",
  "Spain",
  "France",
  "Italy",
  "Portugal",
  "Poland",
  "Belarus",
  "Hungary",
  "Czech Republic",
  "Slovakia",
  "Croatia",
  "Albania",
  "Bosnia and Herzegovina",
  "Greece",
  "Cyprus",
  "Malta",
  "Bulgaria",
  "Moldova",
  "Romania",
  "Russia",
  "Estonia",
  "Latvia",
  "Lithuania",
  "Ukraine",
  "Armenia",
  "Georgia",
  "Azerbaijan",
  "Finland",
  "Iceland",
  "German Federal Republic",
  "German Democratic Republic",
  "Yugoslavia",
  "Czechoslovakia"
)

# Remove Western countries
western_countries <- c(
  "United Kingdom",
  "United States of America",
  "Canada",
  "Denmark",
  "Norway",
  "Germany",
  "Luxembourg",
  "Netherlands",
  "Belgium",
  "Spain",
  "France",
  "Italy",
  "Portugal",
  "Iceland",
  "German Federal Republic",
  "Greece"
)

## Pull countries of interest
node_list <- node_list |>
  filter(name %in% coi) #|>
  # filter(!name %in% western_countries)  # Remove Western countries

## Filter edgelist
edge_list <- edge_list |>
  filter(from_name %in% node_list$name,
         to_name %in% node_list$name)


## Create network for a specific year
create_year_network <- function(year) {
  # Filter edges for the specific year
  year_edges <- edge_list |>
    filter(year == !!year)

  # Create igraph object
  g <- graph_from_data_frame(
    d = select(year_edges, from, to),
    vertices = node_list,
    directed = FALSE
  )

  # Remove isolated vertices
  g <- delete.vertices(g, which(degree(g) == 0))

  # Convert to tbl_graph
  as_tbl_graph(g)
}

# Example: Create network for 1980
# network_year <- 1980
# network_year <- 1985
# network_year <- 1990
# network_year <- 1991
network_year <- 1992
# network_year <- 1995
# network_year <- 1997
# network_year <- 2000
network <- create_year_network(network_year)

# Basic network statistics
cat("Network Statistics for", network_year, ":\n")
cat("Number of nodes (with connections):", gorder(network), "\n")
cat("Number of edges:", gsize(network), "\n")

# Visualize the network
set.seed(123)  # For reproducible layout
# ggraph(network, layout = "fr") +  # Using Fruchterman-Reingold layout
ggraph(network, layout = "kk") +  # Using Fruchterman-Reingold layout
  geom_edge_link(alpha = 0.2, width = 0.5) +
  geom_node_point(aes(size = centrality_degree()),
                  color = "steelblue",
                  alpha = 0.7) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 2.5,
                 max.overlaps = 15) +  # Allow some overlap to show more labels
  scale_size_continuous(range = c(3, 10)) +
  theme_graph(base_family = "sans") +
  labs(title = paste("Alliance Network in", network_year),
       subtitle = "Node size represents number of alliances",
       size = "Number of\nAlliances") +
  theme(legend.position = "right",
        plot.subtitle = element_text(size = 10))

# Function to calculate and visualize shortest paths to Russia for all countries
visualize_distance_to_russia <- function(network) {
  # Get Russia node
  russia_node <- which(V(network)$name == "Russia")

  if (length(russia_node) == 0) {
    warning("Russia not found in the network for this year")
    # Just show the basic network
    ggraph(network, layout = "kk") +
      geom_edge_link(alpha = 0.2, width = 0.5) +
      geom_node_point(size = 5) +
      geom_node_text(aes(label = name),
                    repel = TRUE,
                    size = 2.5,
                    max.overlaps = 15) +
      theme_graph() +
      labs(title = paste("Network for", network_year),
           subtitle = "Russia not present in this year") +
      theme(legend.position = "right")
  } else {
    # Calculate shortest paths from all nodes to Russia
    distances <- distances(network, to = russia_node)

    # Get distances for each node to Russia
    russia_distances <- as.vector(distances)

    # Replace Inf with NA for disconnected nodes
    russia_distances[is.infinite(russia_distances)] <- NA

    # Get the maximum finite distance
    max_distance <- max(russia_distances, na.rm = TRUE)

    # Create a color palette for distances
    # Russia (0 steps) will be blue
    # Other distances will use a gradient from red (close) to green (far)
    color_palette <- c("blue", colorRampPalette(c("red", "yellow", "green"))(max_distance))

    # Create a factor for distances to ensure categorical coloring
    distance_factor <- factor(russia_distances, levels = 0:max_distance)

    # Create shape vector (triangle for Russia, circle for others)
    node_shapes <- ifelse(V(network)$name == "Russia", 17, 16)

    # Visualize the network with distance-based coloring
    ggraph(network, layout = "kk") +
      geom_edge_link(alpha = 0.2, width = 0.5) +
      geom_node_point(aes(color = distance_factor,
                         shape = node_shapes),
                     size = 5) +
      geom_node_text(aes(label = name),
                    repel = TRUE,
                    size = 2.5,
                    max.overlaps = 15) +
      scale_color_manual(values = color_palette,
                        name = "Distance to\nRussia",
                        na.value = "gray",
                        labels = c("Russia", paste(1:max_distance, "steps"))) +
      scale_shape_identity() +
      theme_graph() +
      labs(title = paste("Network with Distance to Russia"),
           subtitle = paste("Year:", network_year),
           caption = "Green = Further from Russia (better), Red = Closer to Russia (worse)") +
      theme(legend.position = "right")
  }
}

# Example usage:
visualize_distance_to_russia(network)

## Freedom house
library(freedomhouse)
freedomhouse::country_score |>
  pull(item_description) |>
  unique()

freedomhouse::country_rating_status |>
  filter(continent == "Europe") |>
  select(-year) |>
  distinct()

# Function to calculate distance to Russia for a single year
calculate_year_distances <- function(year) {
  # Create network for the year
  year_network <- create_year_network(year)

  # Get Russia node
  russia_node <- which(V(year_network)$name == "Russia")

  if (length(russia_node) == 0) {
    # If Russia not in network, return empty tibble
    tibble(
      country = character(),
      year = integer(),
      distance_to_russia = numeric()
    )
  } else {
    # Calculate distances
    distances <- distances(year_network, to = russia_node)
    russia_distances <- as.vector(distances)

    # Create result tibble and filter out infinite distances
    tibble(
      country = V(year_network)$name,
      year = year,
      distance_to_russia = russia_distances
    ) |>
      filter(!is.infinite(distance_to_russia))
  }
}

# Function to calculate distances across a range of years
calculate_distance_tibble <- function(start_year, end_year) {
  # Create sequence of years
  years <- seq(start_year, end_year)

  # Calculate distances for each year and combine
  distance_data <- map_dfr(years, calculate_year_distances)

  # Get political rights scores from Freedom House data
  political_rights <- freedomhouse::country_rating_status |>
    select(country, year, status, political_rights, civil_liberties) |>
    filter(year >= start_year, year <= end_year)

  # Join political rights with distance data
  distance_data <- distance_data |>
    left_join(political_rights, by = c("country", "year")) |>
    group_by(country) |>  # Group by country to create lag within each country
    mutate(civil_liberties = lag(civil_liberties),
           # political_rights = lag(political_rights),
           status = lag(status)) |>  # Create lagged variable
    ungroup()  # Ungroup to return to normal tibble

  # Get country borders and identify Russia's neighbors
  library(rnaturalearth)
  library(sf)

  # Get country borders
  countries <- ne_countries(scale = "medium", returnclass = "sf")

  # Get Russia's border
  russia <- countries |>
    filter(name == "Russia") |>
    st_geometry()

  # Get other countries and create border indicator
  other_countries <- countries |>
    filter(name %in% distance_data$country) |>
    mutate(borders_russia = as.numeric(st_touches(geometry, russia, sparse = FALSE))) |>
    select(name, borders_russia) |>
    st_drop_geometry()

  # Join with main dataset
  distance_data <- distance_data |>
    left_join(other_countries, by = c("country" = "name")) |>
    mutate(borders_russia = if_else(country == "Czech Republic", 0, borders_russia)) |>  # Manually set Czech Republic to not border
    filter(country != "Russia")  # Remove Russia from the final dataset

  return(distance_data)
}

# Example usage:
# Calculate distances from 1992 to 2000
distance_tibble <- calculate_distance_tibble(1992, 2000)

# Run regression with border indicator
fixest::feols(
  # as.numeric(political_rights) ~ distance_to_russia + borders_russia | country + year,
  as.numeric(political_rights) ~ distance_to_russia | country + year,
  data = distance_tibble
)







