set.seed(07042025)

library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(rnaturalearth)
library(sf)
library(geosphere)  # Add geosphere library for distance calculations
library(maps)  # Add maps package for capital coordinates


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
  # "German Federal Republic",
  # "German Democratic Republic",
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

# Define Soviet and Soviet bloc countries
soviet_countries <- c(
  "Russia",
  "Belarus",
  "Ukraine",
  "Estonia",
  "Latvia",
  "Lithuania",
  "Moldova",
  "Armenia",
  "Georgia",
  "Azerbaijan",
  "Kazakhstan",
  "Kyrgyzstan",
  "Tajikistan",
  "Turkmenistan",
  "Uzbekistan",
  "Poland",
  "Hungary",
  "Czech Republic",
  "Slovakia",
  "Bulgaria",
  "Romania",
  "Albania",
  "Yugoslavia",
  "Czechoslovakia"
)

## Pull countries of interest
node_list <- node_list |>
  filter(name %in% coi) |>
  filter(!name %in% western_countries) |>  # Remove Western countries
  filter(name %in% soviet_countries)  # Keep only Soviet and Soviet bloc countries

## Filter edgelist
edge_list <- edge_list |>
  filter(from_name %in% node_list$name,
         to_name %in% node_list$name)


## Create network for a specific year
create_year_network <- function(year) {
  # Filter edges for the specific year
  year_edges <- edge_list |>
    filter(year == !!year)

  # Add Freedom House data to node list
  fhouse <- freedomhouse::country_rating_status |>
    filter(year == !!year) |>
    select(name = country, year, political_rights, civil_liberties, status) |>
    mutate(
      # fhouse = political_rights + civil_liberties,
      # fhouse_std = 1 - (fhouse - min(fhouse)) / (max(fhouse) - min(fhouse)) # high value = nice
      fhouse = status,
      fhouse_std = status
    )

  # Get country borders and identify Russia's neighbors
  countries <- ne_countries(scale = "medium", returnclass = "sf")

  # Get Russia's border
  russia <- countries |>
    filter(name == "Russia") |>
    st_geometry()

  # Get other countries and create border indicator
  border_data <- countries |>
    filter(name %in% node_list$name) |>
    mutate(no_rus_border = 1 - as.numeric(st_touches(geometry, russia, sparse = FALSE))) |> # 0 if borders, 1 if not
    select(name, no_rus_border) |>
    st_drop_geometry()

  # Get Moscow coordinates and create a point
  moscow_point <- st_point(c(37.6173, 55.7558)) |>
    st_sfc(crs = 4326)

  # Create name mapping for countries
  name_mapping <- tribble(
    ~world_cities_name, ~node_list_name,
    "UK", "United Kingdom",
    "United Kingdom", "United Kingdom",
    "USA", "United States of America",
    "Canada", "Canada",
    "Denmark", "Denmark",
    "Norway", "Norway",
    "Germany", "Germany",
    "Luxembourg", "Luxembourg",
    "Netherlands", "Netherlands",
    "Belgium", "Belgium",
    "Spain", "Spain",
    "France", "France",
    "Italy", "Italy",
    "Portugal", "Portugal",
    "Poland", "Poland",
    "Belarus", "Belarus",
    "Hungary", "Hungary",
    "Czech Republic", "Czech Republic",
    "Slovakia", "Slovakia",
    "Croatia", "Croatia",
    "Albania", "Albania",
    "Bosnia and Herzegovina", "Bosnia and Herzegovina",
    "Greece", "Greece",
    "Cyprus", "Cyprus",
    "Malta", "Malta",
    "Bulgaria", "Bulgaria",
    "Moldova", "Moldova",
    "Romania", "Romania",
    "Russia", "Russia",
    "Estonia", "Estonia",
    "Latvia", "Latvia",
    "Lithuania", "Lithuania",
    "Ukraine", "Ukraine",
    "Armenia", "Armenia",
    "Georgia", "Georgia",
    "Azerbaijan", "Azerbaijan",
    "Finland", "Finland",
    "Iceland", "Iceland",
    "Yugoslavia", "Yugoslavia",
    "Czechoslovakia", "Czechoslovakia"
  )

  # Get capital coordinates for each country
  capitals <- world.cities |>
    filter(capital == 1) |>
    select(world_cities_name = country.etc, capital_lat = lat, capital_long = long) |>
    inner_join(name_mapping, by = "world_cities_name") |>
    select(name = node_list_name, capital_lat, capital_long) |>
    filter(name %in% node_list$name) |>
    # Ensure we only get one entry per country by taking the first occurrence
    group_by(name) |>
    slice(1) |>
    ungroup() |>
    mutate(
      capital_point = purrr::map2(capital_long, capital_lat, ~st_point(c(.x, .y))),
      capital_sfc = purrr::map(capital_point, ~st_sfc(.x, crs = 4326)),
      distance_to_moscow_km = purrr::map_dbl(capital_sfc, ~st_distance(.x, moscow_point) / 1000)  # Convert to kilometers
    ) |>
    select(name, distance_to_moscow_km)

  # Ensure node_list is unique
  node_list <- node_list |>
    distinct(name, .keep_all = TRUE) |>
    left_join(
      fhouse,
      by = c(
        "name" = "name"
      )
    ) |>
    left_join(
      border_data,
      by = c("name" = "name")
    ) |>
    left_join(
      capitals,
      by = "name"
    ) |>
    mutate(no_rus_border = if_else(name == "Czech Republic", 1, no_rus_border)) # Manually set Czech Republic to not border (1)

  # Create igraph object
  g <- graph_from_data_frame(
    d = select(year_edges, from, to),
    vertices = node_list,
    directed = FALSE
  )

  # Remove isolated vertices using igraph's delete_vertices
  g <- delete_vertices(g, which(degree(g) == 0))

  # Convert to tbl_graph
  as_tbl_graph(g)
}

# Example: Create network for 1980
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
ggraph(network, layout = "kk") +
  geom_edge_link(alpha = 0.2, width = 0.5) +
  geom_node_point(aes(size = centrality_degree(),
                      color = fhouse_std,
                      shape = case_when(
                        distance_to_moscow_km < 500 ~ "Very Close",
                        distance_to_moscow_km < 1000 ~ "Close",
                        distance_to_moscow_km < 1500 ~ "Medium",
                        TRUE ~ "Far"
                      )),
                  alpha = 0.7) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 2.5,
                 max.overlaps = 15) +
  scale_size_continuous(range = c(5, 15)) +
  scale_color_manual(values = c(
    "Free" = "#B3FFB3",
    "Partially Free" = "#FFE5B3",
    "Not Free" = "#FFB3B3"
  )) +
  scale_shape_manual(values = c(
    "Very Close" = 16,    # filled circle
    "Close" = 17,         # filled triangle
    "Medium" = 18,        # filled diamond
    "Far" = 15           # filled square
  )) +
  theme_graph(base_family = "sans") +
  labs(title = paste("Alliance Network in", network_year),
       subtitle = "Node size = number of alliances\nColor = Freedom House status\nShape = Distance to Moscow",
       size = "Number of\nAlliances",
       color = "Freedom\nHouse Status",
       shape = "Distance to\nMoscow") +
  theme(legend.position = "right",
        plot.subtitle = element_text(size = 10))

# Save the combined visualization
ggsave("network_combined.png", width = 12, height = 8, dpi = 300)

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

## ERGM Analysis
library(ergm)
library(network)
library(intergraph)

# Function to run ERGM analysis for a specific year
run_ergm_analysis <- function(year) {
  # Create network for the year
  network <- create_year_network(year)

  # Convert to network object for ERGM
  net <- asNetwork(network)

  # Get node attributes and handle missing values
  fhouse_std <- V(network)$fhouse_std
  distance_to_moscow_km <- V(network)$distance_to_moscow_km
  no_rus_border <- V(network)$no_rus_border

  # Replace NA values with median for numeric attributes
  fhouse_std[is.na(fhouse_std)] <- median(fhouse_std, na.rm = TRUE)
  distance_to_moscow_km[is.na(distance_to_moscow_km)] <- median(distance_to_moscow_km, na.rm = TRUE)
  no_rus_border[is.na(no_rus_border)] <- median(no_rus_border, na.rm = TRUE)

  # Add node attributes
  set.vertex.attribute(net, "fhouse_std", fhouse_std)
  set.vertex.attribute(net, "distance_to_moscow_km", distance_to_moscow_km)
  set.vertex.attribute(net, "no_rus_border", no_rus_border)

  # Fit ERGM model with nodematch and gwesp terms
  ergm_model <- ergm(
    formula = net ~ edges +
         # nodecov("fhouse_std") +
         nodematch("fhouse_std") +
         nodecov("distance_to_moscow_km") +
         nodematch("no_rus_border"), # +
         # gwesp(0.5, fixed = TRUE),
    control = control.ergm(
      MCMC.samplesize = 5000,
      MCMC.burnin = 5000,
      MCMC.interval = 500,
      MCMLE.maxit = 30,
      MCMC.prop.weights = "random",
      MCMC.prop.args = list(p0 = 0.5)
    )
  )

  return(ergm_model)
}

# Run ERGM analysis for years 1990-2000
ergm_results <- list()
for(year in 1990:2000) {
  cat("Running ERGM for year:", year, "\n")
  ergm_results[[as.character(year)]] <- try(run_ergm_analysis(year))
}

# Function to extract and summarize ERGM results
summarize_ergm_results <- function(ergm_results) {
  results_summary <- list()

  for(year in names(ergm_results)) {
    if(!inherits(ergm_results[[year]], "try-error")) {
      results_summary[[year]] <- summary(ergm_results[[year]])
    }
  }

  return(results_summary)
}

# Get summary of results
ergm_summary <- summarize_ergm_results(ergm_results)

# Print results for a specific year (e.g., 1992)
if(!is.null(ergm_summary[["1992"]])) {
  print(ergm_summary[["1992"]])
}

print(ergm_summary[["1994"]])
print(ergm_summary[["2000"]])

# Function to extract ERGM results into a tidy format
extract_ergm_results <- function(ergm_results) {
  # Create empty tibble to store results
  results_tibble <- tibble(
    year = integer(),
    term = character(),
    estimate = numeric(),
    std_error = numeric(),
    p_value = numeric()
  )

  # Extract results for each year
  for(year in names(ergm_results)) {
    if(!inherits(ergm_results[[year]], "try-error")) {
      # Get summary statistics
      summary_stats <- summary(ergm_results[[year]])

      # Extract coefficients and standard errors
      coefs <- coef(summary_stats)

      # Create tibble for this year's results
      year_results <- tibble(
        year = as.integer(year),
        term = rownames(coefs),
        estimate = coefs[, "Estimate"],
        std_error = coefs[, "Std. Error"],
        p_value = coefs[, "Pr(>|z|)"]
      )

      # Add to main results tibble
      results_tibble <- bind_rows(results_tibble, year_results)
    }
  }

  return(results_tibble)
}

# Extract results into tidy format
ergm_tidy <- extract_ergm_results(ergm_results)

# Custom shape and color mapping for significance (updated for 0.10 threshold)
get_shape <- function(p) {
  if (p < 0.05) return(15)      # Square
  if (p < 0.10) return(17)      # Triangle
  return(16)                    # Circle
}
get_color <- function(p) {
  if (p < 0.05) return("darkgreen")
  if (p < 0.10) return("orange")
  return("red")
}

plot_data <- ergm_tidy %>%
  filter(!term %in% c("edges", "nodematch.no_rus_border")) %>%
  mutate(
    shape = sapply(p_value, get_shape),
    color = sapply(p_value, get_color)
  )

# Faceted plot in the style of the example, with new color logic
ggplot(plot_data, aes(x = year, y = estimate, group = 1)) +
  geom_ribbon(aes(ymin = estimate - 1.96 * std_error, ymax = estimate + 1.96 * std_error),
              fill = "grey30", alpha = 0.3) +
  geom_line(color = "black", size = 1) +
  geom_point(aes(shape = factor(shape), color = color), size = 3, stroke = 1.2, fill = NA) +
  scale_shape_manual(
    values = c(`15` = 15, `16` = 16, `17` = 17),
    labels = c(`15` = "p < 0.05", `16` = "p ≥ 0.10", `17` = "0.05 ≤ p < 0.10")
  ) +
  scale_color_identity(guide = "none") +
  facet_wrap(~term, scales = "free_y") +
  labs(
    title = "ERGM Estimates Over Time (1992-2000)",
    x = "Year",
    y = "Log-Odds",
    shape = "Significance"
  ) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 22, hjust = 0.5),
    axis.title = element_text(face = "bold", size = 18),
    legend.position = "top"
  )

ggsave("ergm_estimates_faceted_styled.png", width = 15, height = 10, dpi = 300)

# Visualize the network with distance to Russia
ggraph(network, layout = "kk") +
  geom_edge_link(alpha = 0.2, width = 0.5) +
  geom_node_point(aes(size = centrality_degree(),
                      color = distance_to_moscow_km,
                      shape = case_when(
                        distance_to_moscow_km < 1000 ~ "Very Close",
                        distance_to_moscow_km < 2000 ~ "Close",
                        TRUE ~ "Far"
                      )),
                  alpha = 0.7) +
  geom_node_text(aes(label = name),
                 repel = TRUE,
                 size = 2.5,
                 max.overlaps = 15) +
  scale_size_continuous(range = c(5, 15)) +
  scale_color_gradient(low = "red", high = "blue",
                      name = "Distance to\nMoscow (km)") +
  scale_shape_manual(values = c(
    "Very Close" = 16,    # filled circle
    "Close" = 17,         # filled triangle
    "Far" = 18           # filled diamond
  )) +
  theme_graph(base_family = "sans") +
  labs(title = paste("Alliance Network in", network_year),
       subtitle = "Node size = number of alliances\nColor = Distance to Moscow\nShape = Distance to Moscow",
       size = "Number of\nAlliances",
       shape = "Distance to\nMoscow") +
  theme(legend.position = "right",
        plot.subtitle = element_text(size = 10))

# Save the distance-based visualization
ggsave("network_distance_to_russia.png", width = 12, height = 8, dpi = 300)
