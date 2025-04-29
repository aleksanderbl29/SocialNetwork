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

# Define Western countries
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
  filter(name %in% coi) |>
  mutate(western = if_else(name %in% western_countries, TRUE, FALSE))

## Filter edgelist
edge_list <- edge_list |>
  filter(from_name %in% coi,
         to_name %in% coi)


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
# network_year <- 1995
network_year <- 1997
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

# Function to calculate and visualize shortest paths to Western nodes for all countries
visualize_all_shortest_paths <- function(network) {
  # Get Western nodes
  western_nodes <- which(V(network)$western)

  if (length(western_nodes) == 0) {
    warning("No Western nodes found in the network for this year")
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
           subtitle = "No Western nodes present in this year") +
      theme(legend.position = "right")
  } else {
    # Calculate shortest paths from all nodes to Western nodes
    distances <- distances(network, to = western_nodes)

    # Get minimum distance for each node to any Western node
    min_distances <- apply(distances, 1, min)

    # Replace Inf with NA for disconnected nodes
    min_distances[is.infinite(min_distances)] <- NA

    # Get the maximum finite distance
    max_distance <- max(min_distances, na.rm = TRUE)

    # Create a color palette for distances
    # Western countries (0 steps) will be blue
    # Other distances will use a gradient from green to red
    color_palette <- c("blue", colorRampPalette(c("green", "yellow", "red"))(max_distance))

    # Create a factor for distances to ensure categorical coloring
    distance_factor <- factor(min_distances, levels = 0:max_distance)

    # Visualize the network with distance-based coloring
    ggraph(network, layout = "kk") +
      geom_edge_link(alpha = 0.2, width = 0.5) +
      geom_node_point(aes(color = distance_factor,
                         shape = V(network)$western),
                     size = 5) +
      geom_node_text(aes(label = name),
                    repel = TRUE,
                    size = 2.5,
                    max.overlaps = 15) +
      scale_color_manual(values = color_palette,
                        name = "Distance to\nWestern Country",
                        na.value = "gray",
                        labels = c("Western", paste(1:max_distance, "steps"))) +
      scale_shape_manual(values = c("FALSE" = 16, "TRUE" = 17),
                        name = "Western Country") +
      theme_graph() +
      labs(title = paste("Network with Distance to Western Countries"),
           subtitle = paste("Year:", network_year)) +
      theme(legend.position = "right")
  }
}

# Example usage:
visualize_all_shortest_paths(network)

## Freedom house
library(freedomhouse)
freedomhouse::country_score |>
  pull(item_description) |>
  unique()

freedomhouse::country_rating_status |>
  filter(continent == "Europe") |>
  select(-year) |>
  distinct()



