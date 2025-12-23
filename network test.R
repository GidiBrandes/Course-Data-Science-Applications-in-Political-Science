rm(list=ls())
library(dplyr)
Intelligence <- read.csv("https://raw.githubusercontent.com/GidiBrandes/Course-Data-Science-Applications-in-Political-Science/refs/heads/main/Intelligence.csv"
                 , stringsAsFactors = FALSE, encoding = "UTF-8")



library(stringr)
library(dplyr)

Intelligence_clean <- Intelligence %>%
  mutate(
    desc_clean = description %>%
      str_to_lower() %>%
      str_replace_all("[^a-z0-9 ]", "") %>%  # remove punctuation
      str_squish()
  )

Intelligence_clean <- Intelligence_clean %>%
  mutate(
    incident_key_desc = paste(
      desc_clean,
      event_date,
      actor,
      sep = " | "
    )
  )

Intelligence_unique_desc <- Intelligence_clean %>%
  group_by(incident_key_desc) %>%
  slice(1) %>%      # keep first occurrence
  ungroup()



Data <- Intelligence_unique_desc
Data <- Data %>%
  select(year, actor_type, industry, event_subtype, motive, country, actor_country)
table(Data$industry)


library(dplyr)
library(stringr)

# Clean up industry names
Data <- Data %>%
  mutate(industry = case_when(
    # 1. Finance and insurance variations
    str_to_lower(industry) %in% c("finance and insurance") ~ "Finance and Insurance",
    
    # 2. Information variations
    str_detect(industry, regex("Information", ignore_case = TRUE)) ~ "Information",
    
    # 3. Manufacturing variations
    str_detect(industry, regex("Manufacturing", ignore_case = TRUE)) ~ "Manufacturing",
    
    # 4. Mining variations: normalize 'and' / '&'
    str_detect(industry, regex("Mining, Quarrying, and Oil [&]? Gas Extraction", ignore_case = TRUE)) ~ 
      "Mining, Quarrying, and Oil & Gas Extraction",
    
    # Default: keep as is
    TRUE ~ industry
  ))
table(Data$actor_type)
table(Data$motive)

library(dplyr)
library(tidyr)
library(stringr)

Data <- Data %>%
  # Separate the motive into two columns using comma as separator
  separate(motive, into = c("motive1", "motive2"), sep = ",", fill = "right", extra = "merge") %>%
  # Trim any leading/trailing whitespace
  mutate(
    motive1 = str_trim(motive1),
    motive2 = str_trim(motive2)
  )

# Normalize motive2 values
Data <- Data %>%
  mutate(
    motive2 = str_replace_all(motive2, fixed("Industrial Espionage"), "Industrial-Espionage")
  )

table(Data$motive1)
table(Data$motive2)




table(Data$event_subtype)
Data <- Data %>%
  # Separate the motive into two columns using comma as separator
  separate(event_subtype, into = c("event_type1", "event_type2", "event_type3", "event_type4"), sep = ",", fill = "right", extra = "merge") %>%
  # Trim any leading/trailing whitespace
  mutate(
    event_type1 = str_trim(event_type1),
    event_type2 = str_trim(event_type2),
    event_type3 = str_trim(event_type3),
    event_type4 = str_trim(event_type4)
  )




table(Data$country)
library(dplyr)
library(stringr)

Data <- Data %>%
  mutate(country = str_replace_all(country, c(
    "Viet Nam" = "Vietnam",
    "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
    "Korea \\(the Democratic People's Republic of\\)" = "North Korea",
    "Korea \\(the Republic of\\)" = "South Korea",
    "Bolivia \\(Plurinational State of\\)" = "Bolivia",
    "Iran \\(Islamic Republic of\\)" = "Iran",
    "Moldova \\(the Republic of\\)" = "Moldova",
    "Palestine, State of" = "Palestine",
    "Taiwan \\(Province of China\\)" = "Taiwan",
    "Venezuela \\(Bolivarian Republic of\\)" = "Venezuela"
  )))



table(Data$actor_country)
Data <- Data %>%
  mutate(actor_country = str_replace_all(actor_country, c(
    "Viet Nam" = "Vietnam",
    "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
    "Korea \\(the Democratic People's Republic of\\)" = "North Korea",
    "Korea \\(the Republic of\\)" = "South Korea",
    "Bolivia \\(Plurinational State of\\)" = "Bolivia",
    "Iran \\(Islamic Republic of\\)" = "Iran",
    "Palestine, State of" = "Palestine",
    "Venezuela \\(Bolivarian Republic of\\)" = "Venezuela",
    "Russian Federation" = "Russia",
    "Taiwan \\(Province of China\\)" = "Taiwan",
    "Kazakstan" = "Kazakhstan"
  )))


NetworkData <- Data
  

# Required packages
library(tidyverse)   # dplyr, tidyr, purrr, stringr, tibble
library(igraph) # network object and analysis
library(janitor)     # clean_names()

# 0) Make a safe copy and clean column names (turn "motive 1" -> "motive_1", etc.)
ND <- NetworkData %>% 
  janitor::clean_names()  # safe names: spaces -> underscores, lower case

# 1) List the columns we want to use as sources of nodes.
#    (adjust if your cleaned names are different; print names(ND) to check)
vars <- c("actor_type", "industry", "motive1", "motive2",
          "event_type1", "event_type2", "event_type3", "event_type4",
          "country", "actor_country")

# 2) Define values that should be treated as "present but not participating"
#    (case-insensitive). Add other strings you want ignored here.
exclude_values <- c("undetermined", "", "na", "n/a", "unknown")

# 3) For each row, create a list-column 'node_ids' containing the nodes that
#    should participate in the network. Each node_id keeps the variable name
#    as a prefix so same label in different columns are distinct nodes.
ND_nodes <- ND %>%
  # Keep original row id so we can trace back (use existing rownames or make one)
  mutate(.row = row_number()) %>%
  # Keep only the vars we need + the row id
  select(.row, all_of(vars)) %>%
  # For each row produce a list of node IDs
  rowwise() %>%
  mutate(
    node_ids = list(
      # iterate through each variable name and its value
      pmap_chr(
        list(var = vars, val = cur_data_all()[vars]),
        function(var, val) {
          # normalize value to a single string (trim, remove surrounding spaces)
          v <- if (is.na(val)) NA_character_ else str_trim(as.character(val))
          # ignore NA and exclude_values (case-insensitive)
          if (is.na(v)) return(NA_character_)
          if (tolower(v) %in% exclude_values) return(NA_character_)
          # construct node id keeping variable namespace
          paste0(var, ":", v)
        }
      ) %>% 
        # pmap_chr returns vector for all vars; drop NAs produced above
        discard(~ is.na(.x))
    )
  ) %>%
  ungroup()

# Quick check: how many rows have zero participating nodes? (should be rare)
zero_node_rows <- ND_nodes %>% filter(map_int(node_ids, length) == 0) %>% nrow()
message("Rows with zero participating nodes: ", zero_node_rows)

# 4) Build an edge list by taking all unordered pairs of node_ids within each row.
#    We generate pairs only when a row has >= 2 participating nodes.
edge_list <- ND_nodes %>%
  # keep only .row and node_ids
  select(.row, node_ids) %>%
  # keep rows with at least 2 nodes
  filter(map_int(node_ids, length) >= 2) %>%
  # for each row, compute combinations
  mutate(pairs = map(node_ids, ~ {
    n <- length(.x)
    if (n < 2) return(tibble(from=character(), to=character()))
    combos <- t(combn(.x, 2L))       # each row is a pair (unordered)
    tibble(from = combos[,1], to = combos[,2])
  })) %>%
  select(.row, pairs) %>%
  unnest(pairs) %>%
  # ensure pairs are consistently ordered so that (A,B) and (B,A) map to same pair
  mutate(
    pair_min = pmin(from, to),
    pair_max = pmax(from, to),
    from = pair_min,
    to   = pair_max
  ) %>%
  select(from, to)

# 5) Aggregate identical edges across all rows to compute edge weights
edges_weighted <- edge_list %>%
  count(from, to, name = "weight") %>%
  arrange(desc(weight))

# 6) Build node table from every unique node found (including nodes that never
#    got an edge because they only occurred alone in a row). We also compute:
#    - variable (the source column)
#    - label (original category text)
#    - freq_in_rows: how many rows the node appeared in (occurrence frequency)
nodes_from_rows <- ND_nodes %>%
  select(.row, node_ids) %>%
  unnest(node_ids) %>%
  distinct(.row, node_ids) %>%     # each node counted at most once per row
  count(node_ids, name = "freq_in_rows") %>%
  # split node_ids back into variable and label
  separate(node_ids, into = c("variable", "label"), sep = ":", extra = "merge") %>%
  arrange(desc(freq_in_rows))

# Also ensure that nodes that never participated in any edge (isolates) are included:
all_nodes <- tibble(node_id = unique(c(edges_weighted$from, edges_weighted$to, 
                                       paste0(nodes_from_rows$variable, ":", nodes_from_rows$label)))) %>%
  left_join(
    nodes_from_rows %>% mutate(node_id = paste0(variable, ":", label)),
    by = "node_id"
  ) %>%
  # fill NA frequencies with 0
  mutate(freq_in_rows = replace_na(freq_in_rows, 0)) %>%
  select(node_id, variable, label, freq_in_rows)

# 7) Create an igraph object from the edge list (undirected), using weights.
g <- graph_from_data_frame(
  d = edges_weighted %>% rename(from = from, to = to),
  directed = FALSE,
  vertices = all_nodes %>% rename(name = node_id)
)
# attach freq_in_rows as vertex attribute (already included if vertices was used)
V(g)$freq_in_rows <- all_nodes$freq_in_rows[match(V(g)$name, all_nodes$node_id)]
V(g)$variable <- all_nodes$variable[match(V(g)$name, all_nodes$node_id)]
V(g)$label    <- all_nodes$label[match(V(g)$name, all_nodes$node_id)]

# 8) Basic summaries and checks
message("Number of nodes: ", gorder(g))
message("Number of edges: ", gsize(g))
message("Top 10 edges by weight:")
print(head(edges_weighted, 10))

#: save outputs for later
network_edges <- edges_weighted
network_nodes <- all_nodes








library(dplyr)

# Degree: number of connections
V(g)$degree <- degree(g)

# Strength: sum of edge weights
V(g)$strength <- strength(g, weights = E(g)$weight)

# Betweenness centrality
V(g)$betweenness <- betweenness(g, directed = FALSE, weights = 1/E(g)$weight)

# Closeness centrality
V(g)$closeness <- closeness(g, normalized = TRUE)

# Eigenvector centrality
V(g)$eigencentral <- eigen_centrality(g, weights = E(g)$weight)$vector

# Community detection using Louvain algorithm
comm <- cluster_louvain(g, weights = E(g)$weight)
V(g)$community <- comm$membership






nodes_summary <- tibble(
  node = V(g)$name,
  variable = V(g)$variable,
  label = V(g)$label,
  freq_in_rows = V(g)$freq_in_rows,
  degree = V(g)$degree,
  strength = V(g)$strength,
  betweenness = V(g)$betweenness,
  closeness = V(g)$closeness,
  eigencentral = V(g)$eigencentral,
  community = V(g)$community
) %>%
  arrange(desc(strength))  # sort by most connected


head(nodes_summary, 20)





community_summary <- nodes_summary %>%
  group_by(community) %>%
  summarise(
    n_nodes = n(),
    total_strength = sum(strength),
    avg_degree = mean(degree),
    top_nodes = paste(label[order(-strength)][1:5], collapse = ", ")
  ) %>%
  arrange(desc(total_strength))
print(community_summary)


library(dplyr)
library(purrr)

predict_variable <- function(row_nodes, candidates, edges) {
  
  # remove any candidate nodes accidentally already present
  row_nodes <- setdiff(row_nodes, candidates)
  
  scores <- map_dbl(candidates, function(cand) {
    sum(edges$weight[
      (edges$from == cand & edges$to %in% row_nodes) |
        (edges$to == cand & edges$from %in% row_nodes)
    ], na.rm = TRUE)
  })
  
  if (all(scores == 0)) {
    return(NA_character_)
  }
  
  candidates[which.max(scores)]
}



get_candidates <- function(var_name, nodes_summary) {
  nodes_summary %>%
    filter(variable == var_name) %>%
    pull(node)
}

vars_to_predict <- c(
  "actor_type",
  "industry",
  "event_type1",
  "motive1",
  "country",
  "actor_country"
)


ND_predicted <- ND_nodes

for (v in vars_to_predict) {
  
  message("Predicting undetermined values for: ", v)
  
  # rows where this variable is undetermined
  und_idx <- ND_predicted[[v]] == "Undetermined"
  
  # candidate nodes
  candidates <- get_candidates(v, nodes_summary)
  
  ND_predicted[und_idx, paste0("predicted_", v)] <-
    map_chr(
      ND_predicted$node_ids[und_idx],
      ~ predict_variable(.x, candidates, edges_weighted)
    )
}




library(dplyr)
library(purrr)
library(gt)



make_pred_table <- function(x, var_name) {
  as.data.frame(table(x), stringsAsFactors = FALSE) %>%
    rename(
      Category = 1,
      Count    = 2
    ) %>%
    filter(!is.na(Category)) %>%
    mutate(Variable = var_name) %>%
    arrange(desc(Count))
}



pred_tables <- bind_rows(
  make_pred_table(ND_predicted$predicted_actor_type, "Actor type"),
  make_pred_table(ND_predicted$predicted_industry, "Industry"),
  make_pred_table(ND_predicted$predicted_event_type1, "Event type"),
  make_pred_table(ND_predicted$predicted_motive1, "Motive"),
  make_pred_table(ND_predicted$predicted_country, "Target country"),
  make_pred_table(ND_predicted$predicted_actor_country, "Actor country")
)



library(gt)

pred_tables %>%
  mutate(Category = sub("^[^:]+:", "", Category)) %>%
  gt(groupname_col = "Variable") %>%
  tab_header(
    title = "Predicted Categories for Undetermined Cyber Incidents",
    subtitle = "Network-based inference based on structural proximity"
  ) %>%
  cols_label(
    Category = "Predicted category",
    Count = "Number of incidents"
  )


