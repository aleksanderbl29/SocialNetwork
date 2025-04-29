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
network_year <- 1980
network <- create_year_network(network_year)

# Basic network statistics
cat("Network Statistics for", network_year, ":\n")
cat("Number of nodes (with connections):", gorder(network), "\n")
cat("Number of edges:", gsize(network), "\n")

# Visualize the network
set.seed(123)  # For reproducible layout
ggraph(network, layout = "fr") +  # Using Fruchterman-Reingold layout
  geom_edge_link(alpha = 0.2, width = 0.5) +
  geom_node_point(aes(size = centrality_degree()),
                  color = "steelblue",
                  alpha = 0.7) +
  scale_size_continuous(range = c(3, 10)) +
  theme_graph(base_family = "sans") +
  labs(title = paste("Alliance Network in", network_year),
       subtitle = "Node size represents number of alliances",
       size = "Number of\nAlliances") +
  theme(legend.position = "right",
        plot.subtitle = element_text(size = 10))

qog_std <- read_csv("Projekt/standard_qog.csv") |>
  filter(year < 2000) |>
  select(cname, year, ccode, ccodecow, gle_gdp, gle_rgdpc)






