set.seed(07042025)

library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)


## Laver edgelist
edge_yrs <- read_csv("Projekt/alliance_v4.1_by_dyad_yearly.csv") |>
  rename(
    from = ccode1,
    to = ccode2,
    from_name = state_name1,
    to_name = state_name2,
    begin = dyad_st_year,
    end = dyad_end_year
  ) |>
  select(year, from, to, from_name, to_name, begin, end) |>
  filter(year >= 1950, year <= 2000) |>
  arrange()

codes <- edge_yrs |>
  select(from, to) |>
  pivot_longer(everything()) |>
  select(value) |>
  mutate(id = row_number())

# edge_yrs |>
#   left_join(codes)

head(edge_yrs)

## Laver nodes
node_yrs <- read_csv("Projekt/alliance_v4.1_by_dyad_yearly.csv") |>
  select(ccode1, state_name1, ccode2, state_name2) |>
  pivot_longer(cols = c(ccode1, ccode2), values_to = "ccode", names_to = "type") |>
  mutate(state_name = if_else(type == "ccode1", state_name1, state_name2)) |>
  select(ccode, state_name) |>
  distinct(ccode, .keep_all = TRUE)

n1 <- read_csv("Projekt/alliance_v4.1_by_dyad_yearly.csv") |>
  select(id = ccode1, names = state_name1, year)

n2 <- read_csv("Projekt/alliance_v4.1_by_dyad_yearly.csv") |>
  select(id = ccode2, names = state_name2, year)

n <- rbind(
  n1,
  n2
) |> distinct()

edge_yrs <- read_csv("Projekt/alliance_v4.1_by_directed_yearly.csv") |>
  rename(
    from = ccode1,
    to = ccode2,
    from_name = state_name1,
    to_name = state_name2,
    begin = dyad_st_year,
    end = dyad_end_year
  ) |>
  select(year, from, to, from_name, to_name, begin, end) |>
  filter(year >= 1950, year <= 2000) |>
  arrange()


head(node_yrs)

## Laver netværk
network_year <- 1980

network <- tbl_graph(
  edges = edge_yrs |>
    filter(year == network_year),
  nodes = edge_yrs |>
    filter(year == network_year) |>
    select(from, from_name),
  # nodes = node_yrs,
  node_key = "ccode",
  directed = FALSE
)

g <- make_graph(edges = as.vector(rbind(
  edge_yrs |>
    filter(year == network_year) |>
    pull(from),
  edge_yrs |>
    filter(year == network_year) |>
    pull(to)
)))

g |> as_tbl_graph() |> ggraph(layout = "kk") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(
    data = n,
    color = "steelblue"
  ) +
  theme_graph()

network |>
  ggraph(layout = "kk") +
  geom_edge_link(alpha = 0.3) +
  geom_node_point(
    color = "steelblue"
  ) +
  geom_node_label(
    aes(label = from_name),
    repel = TRUE,
    size = 3
  ) +
  theme_graph() +
  theme(
    legend.position = "bottom"
  )




















qog_std <- read_csv("Projekt/standard_qog.csv") |>
  filter(year < 2000) |>
  select(cname, year, ccode, ccodecow, gle_gdp, gle_rgdpc)






