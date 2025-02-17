library(tidyverse)
library(tidygraph)
library(ggraph)

url <- "https://cfa-research.au.dk/courses/networks_F25/data/lektion1_zach_edges.txt"
zachary <- read_tsv(url) |>
  tbl_graph(edges = _)

write_csv(zachary, "Time 1/zachary.txt")

zachary |>
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  theme_graph()
