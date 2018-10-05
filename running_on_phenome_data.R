library(tidyverse)
source("cpp_code/biSBM.R"); ### loading and compiling biSBM function

##### TEST 1 - Southern Women #####
network_data <- read_rds('data/testing_network_data.rds')



edges <- network_data$edges
types <- network_data$vertices$name %>% str_detect('case') %>% ifelse(1, 2)
### call to the function: by default deg.corr=1 and iter = 10.
g <- biSBM(data = edges, nodeType = types, ka = 5, kb = 3, deg.corr = 1, iter = 3);



colors <- c('#8dd3c7','#bebada','#fb8072','#80b1d3','#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5')
new_node_colors <- purrr::map_chr(g, ~colors[.])

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