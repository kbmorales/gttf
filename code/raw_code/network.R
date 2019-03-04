

# Setup -------------------------------------------------------------------


library(tidyverse)
library(tidygraph)
library(ggraph)
library(viridis)

load(here::here("data/tidy_data",
                "mdcs_network_data.rda"))


# Network filters ---------------------------------------------------------


cops <- cops %>% 
  filter(last_name != "Clewell") %>%
  arrange(last_name)

cops_v <- toupper(str_c(cops$last_name, cops$first_name, sep = ", "))

# Reduce size somehow
# Try to filter for between death of Freddie Gray and arrest
filtered_cases <- mdcs_cops_df %>% 
  filter(date >= "2015-04-19")

mdcs_network_df %>% count(connection_type)

mdcs_network_df %>% filter(connection_type %in% c("COMPLAINANT", 
                                                  "COMPLAINANT/POLICE OFFICER",
                                                  "WITNESS/POLICE OFFICER"))

test_net <- mdcs_network_df %>% 
  filter(connection_name %in% cops_v) %>% 
  semi_join(filtered_cases)

taylor_net <- mdcs_network_df %>%
  filter(connection_name == "TAYLOR, MARCUS")


# Node df -----------------------------------------------------------------


sources <- test_net %>%
  count(connection_name) %>%
  rename(label = connection_name)

destinations <- test_net %>%
  count(defendant_name) %>%
  rename(label = defendant_name)

nodes <- full_join(sources, destinations, by = "label") %>% 
  rowid_to_column("id") %>%
  mutate(n = case_when(
    !is.na(n.x) ~ n.x,
    TRUE ~ n.y
  )) %>%
  select(-n.x, -n.y)

# %>%
#   mutate(gttf_yn = case_when(
#     str_detect(label, "^ALLERS|^GONDO|^HENDRIX|^HERSL|^JENKINS|^RAYAM|^TAYLOR, MARCUS|^WARD, MAURICE") ~ "Yes",
#     TRUE ~ "No")
#   )

# nodes$gttf_yn <- factor(nodes$gttf_yn, levels = c("No", "Yes"))

connects <- test_net %>%  
  group_by(defendant_name, connection_name) %>%
  summarise(weight = n()) %>% 
  ungroup() %>%
  arrange(desc(weight)) %>%
  mutate(gttf_cop = case_when(
    str_detect(connection_name, "^ALLERS") ~ "Allers",
    str_detect(connection_name, "^GONDO") ~ "Gondo",
    str_detect(connection_name, "^HENDRIX") ~ "Hendrix",
    str_detect(connection_name, "^HERSL") ~ "Hersl",
    str_detect(connection_name, "^JENKINS") ~ "Jenkins",
    str_detect(connection_name, "^RAYAM") ~ "Rayam",
    str_detect(connection_name, "^TAYLOR") ~ "Taylor",
    str_detect(connection_name, "^WARD") ~ "Taylor",
    TRUE ~ "Other"
  ))

connects$gttf_cop <- factor(connects$gttf_cop, levels = c("Allers",
                                                          "Gondo",
                                                          "Hendrix",
                                                          "Hersl",
                                                          "Jenkins",
                                                          "Rayam",
                                                          "Taylor",
                                                          "Ward",
                                                          "Other")
                            )

edges <- connects %>% 
  left_join(nodes, by = c("defendant_name" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("connection_name" = "label")) %>% 
  rename(to = id)

edges <- edges %>% select(from, to, weight, gttf_cop)

test <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

# Set up palette for gttf_cop
test_pal <- viridis(8, option = "D")
test_pal[9] <- "#000000"

# 1
ggraph(test) + 
  geom_edge_link(aes(width = weight,
                     color = gttf_cop), 
                 alpha = 0.6) + 
  geom_node_point(size = 1,
                  alpha = 0.8) + 
  theme_graph()

ggraph(test, layout = "kk") + 
  geom_edge_fan(aes(color = gttf_cop),
                 alpha = 0.4) + 
  geom_node_point(aes(size = n),
                  alpha = 1) +
  geom_node_text(aes(label = label),
                 size = 2,
                 repel = TRUE) +
  # scale_edge_color_viridis(discrete = TRUE) +
  theme_graph() +
  theme(legend.position = "none")

ggraph(test, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Cases") +
  theme_graph()

ggraph(test, layout = 'dendrogram', circular = TRUE) + 
  geom_edge_diagonal(color = gttf_cop) + 
  geom_node_point() + 
  coord_fixed()
