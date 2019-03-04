

# Setup -------------------------------------------------------------------


library(tidyverse)
library(tidygraph)
library(ggraph)




# Network filters ---------------------------------------------------------


cops <- cops %>% 
  filter(last_name != "Clewell") %>%
  arrange(last_name)

cops_v <- toupper(str_c(cops$last_name, cops$first_name, sep = ", "))

# Reduce size somehow
mdcs_network_df %>% count(connection_type)

mdcs_network_df %>% filter(connection_type %in% c("COMPLAINANT", 
                                                  "COMPLAINANT/POLICE OFFICER",
                                                  "WITNESS/POLICE OFFICER"))

test_net <- mdcs_network_df %>% filter(connection_name %in% cops_v)

taylor_net <- mdcs_network_df %>%
  filter(connection_name == "TAYLOR, MARCUS")


# Node df -----------------------------------------------------------------




sources <- test_net %>%
  distinct(connection_name) %>%
  rename(label = connection_name)

destinations <- test_net %>%
  distinct(defendant_name) %>%
  rename(label = defendant_name)

nodes <- full_join(sources, destinations, by = "label") %>% 
  rowid_to_column("id")

connects <- test_net %>%  
  group_by(defendant_name, connection_name) %>%
  summarise(weight = n()) %>% 
  ungroup() %>%
  arrange(desc(weight))

edges <- connects %>% 
  left_join(nodes, by = c("defendant_name" = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c("connection_name" = "label")) %>% 
  rename(to = id)

edges <- edges %>% select(from, to, weight)

test <- tbl_graph(nodes = nodes, edges = edges, directed = FALSE)

ggraph(test) + geom_edge_link() + geom_node_point() + theme_graph()

ggraph(test, layout = "graphopt") + 
  geom_node_point(size = 1,
                  alpha = 0.8) +
  geom_edge_link(aes(width = weight), alpha = 0.6) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), 
                 repel = TRUE,
                 size = 2) +
  labs(edge_width = "Cases") +
  theme_graph()

ggraph(test, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Cases") +
  theme_graph()
