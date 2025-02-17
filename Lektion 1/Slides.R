# Indlæser pakker
library(tidygraph)
library(ggraph)
library(tidyverse)

# Figur 1

n <- data.frame(name = c(1),
                cl = c(1),
                lab = "Hans")
g <- tbl_graph(nodes = n)

g %>%
  ggraph(layout = "kk") +
  geom_edge_link() +
  geom_node_point(size = 50,
                  fill = "white",
                  color = "#003D73") +
  geom_node_label(aes(label = lab), alpha = .5) +
  scale_color_discrete("") +
  theme_graph()

# Figur 2
n <- data.frame(
  name = c(1, 2, 3),
  cl = c(1, 1, 2),
  lab = c("Hans", "Inger", "Søren"),
  w = c(10, 85, 25)
)
e <- data.frame(from = c(1, 2), to = c(2, 3))
g <- tbl_graph(nodes = n, edges = e)

g %>%
  ggraph(layout = "stress") +
  geom_edge_link() +
  geom_node_point(aes(size = w, color = as.factor(cl))) +
  geom_node_label(aes(label = lab), alpha = .5, repel = TRUE) +
  scale_color_discrete("Gruppe") +
  scale_size_continuous("Social kapital") +
  theme_graph()

# Figur 3
n <- data.frame(
  name = c(1, 2, 3, 4, 5),
  cl = c(1, 1, 2, 2, 2),
  lab = c("Hans", "Inger", "Søren", "Svenning", "Petrea"),
  w = c(10, 85, 75, 25, 35)
)
e <- data.frame(from = c(1, 2, 3, 3, 4), to = c(2, 3, 4, 5, 5))
g <- tbl_graph(nodes = n, edges = e)

g %>%
  ggraph(layout = "stress") +
  geom_edge_link() +
  geom_node_point(aes(size = w, color = as.factor(cl))) +
  geom_node_label(aes(label = lab), alpha = .5, repel = TRUE) +
  scale_color_discrete("Gruppe") +
  scale_size_continuous("Social kapital") +
  theme_graph()

# Figur 4
n <- data.frame(
  name = c(1, 2, 3),
  cl = c(1, 1, 2),
  lab = c("Hans", "Inger", "Søren"),
  w = c(10, 85, 25)
)
e <- data.frame(from = c(1, 3, 2, 3), to = c(2, 1, 3, 2))
g <- tbl_graph(nodes = n, edges = e)

g %>%
  ggraph(layout = "stress") +
  geom_edge_fan(arrow = arrow(length = unit(3, 'mm')), end_cap = circle(2, 'mm')) +
  geom_node_point(aes(size = w, color = as.factor(cl))) +
  geom_node_label(aes(label = lab), alpha = .5, repel = TRUE) +
  scale_color_discrete("Gruppe") +
  scale_size_continuous("Social kapital") +
  theme_graph()

# Figur 5
n <- data.frame(
  name = c(1, 2, 3),
  cl = c(1, 1, 2),
  lab = c("Hans", "Inger", "Søren"),
  w = c(10, 85, 25)
)
e <- data.frame(
  from = c(1, 3, 2, 3),
  to = c(2, 1, 3, 2),
  w = c(3, 2, 2, 5)
)
g <- tbl_graph(nodes = n, edges = e)

g %>%
  ggraph(layout = "stress") +
  geom_edge_fan(
    aes(width = w),
    arrow = arrow(length = unit(4, 'mm')),
    end_cap = circle(2, 'mm'),
    alpha = .5
  ) +
  geom_node_point(aes(size = w, color = as.factor(cl))) +
  geom_node_label(aes(label = lab), alpha = .5, repel = TRUE) +
  scale_color_discrete("Gruppe") +
  scale_size_continuous("Social kapital") +
  theme_graph()
