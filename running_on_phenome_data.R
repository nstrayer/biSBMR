
##### TEST 1 - Southern Women #####
network_data <- read_rds('data/testing_network_data.rds')



edges <- network_data$edges
types <- network_data$vertices$name %>% str_detect('case') %>% ifelse(1, 2)
### call to the function: by default deg.corr=1 and iter = 10.
node_clusters <- biSBM(data = edges, nodeType = types, ka = 5, kb = 3, deg.corr = 1, iter = 3);


clustered_vertices <- network_data$vertices %>% 
  mutate(
    type = ifelse(str_detect(name,'case'), 'case', 'phenotype'),
    cluster = as.integer(node_clusters)
  ) %>% 
  select(index, name, type, cluster, snp_status, tooltip) 
  
clustered_vertices %>% 
  filter(type == 'case') %>% 
  arrange(cluster) %>% 
  group_by(cluster) %>% 
  summarise(average_snp_status = mean(snp_status))

index <- 150

get_phenotype_vec <- function(index){
  network_data$edges %>% 
    filter(source == index) %>% 
    mutate(target = as.integer(target)) %>% 
    left_join(network_data$vertices, by = c('target' = 'id')) %>% 
    select(source, phenotype = name)
}

phenotypes_by_cluster <- clustered_vertices %>% 
  filter(type == 'case') %>%
  .$index %>%    
  purrr::map_df(get_phenotype_vec) %>% 
  mutate(source = as.integer(source)) %>% 
  inner_join(clustered_vertices %>% select(index, cluster), by = c('source' = 'index'))
  

phenotypes_by_cluster %>% 
  ggplot(aes(x = phenotype)) +
  geom_bar() +
  facet_grid(cluster~.)


colors <- c('#8dd3c7','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5')
new_node_colors <- purrr::map_chr(node_clusters, ~colors[.])

list(
  edges = edges,
  vertices = network_data$vertices %>% mutate(color = new_node_colors)
) %>% 
  jsonlite::toJSON() %>%
  r2d3::r2d3(
    script = 'd3_plots/network_2d.js',
    container = 'div',
    dependencies = "d3-jetpack",
    options = list(
      just_snp = FALSE, 
      msg_loc = 'message'
    )
  )