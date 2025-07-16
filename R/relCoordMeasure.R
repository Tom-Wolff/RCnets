relCoordMeasure <- function(data,
                            wave = 1,
                            this_city = 1,
                            type = NULL,
                            threshold = NULL,
                            keep_weight = FALSE) {


  # browser()

  data_subset <- data %>%
    dplyr::filter(city == this_city) %>%
    dplyr::filter(round == wave) %>%
    select(CASEID, alter, dplyr::starts_with("weight"), dplyr::everything()) %>%
    dplyr::mutate(CASEID = as.character(CASEID))


  if (!is.null(type)) {
    data_subset <- data_subset %>%
      dplyr::filter(var == type)
  }

  baselines <- data.frame(avg_wt = mean(data_subset$weight, na.rm = TRUE),
                          median_wt = median(data_subset$weight, na.rm = TRUE),
                          sd_wt = sd(data_subset$weight, na.rm = TRUE))
  baselines$backbone1 <- baselines$avg_wt + baselines$sd_wt
  baselines$backbone2 <- baselines$avg_wt + 2*baselines$sd_wt




  if (is.numeric(threshold)) {
    cut_val <- threshold
    data_subset <- data_subset %>% dplyr::filter(weight >= cut_val)

    if (isFALSE(keep_weight)) {
      data_subset <- data_subset %>% dplyr::mutate(weight = 1)
    }

  } else if (is.character(threshold)) {
    if (threshold == "mean") {
      cut_val = baselines$avg_wt
    } else if (threshold == "median") {
      cut_val = baselines$median_wt
    } else if (threshold == "1sd") {
      cut_val = baselines$backbone1
    } else if (threshold == "2sd") {
      cut_val = baselines$backbone2
    } else {
      cut_val = threshold
    }

    data_subset <- data_subset %>% dplyr::filter(weight >= cut_val)

    if (isFALSE(keep_weight)) {
      data_subset <- data_subset %>% dplyr::mutate(weight = 1)
    }

  } else {
    cut_val <- threshold
  }

  this_list <- make_bipartite_list(data = data_subset,
                                   data_type = "edgelist",
                                   i_elements = data_subset$CASEID,
                                   j_elements = data_subset$alter,
                                   weight = data_subset$weight)

  this_igraph <- bi_igraph(this_list)

  nodelist <- this_list$nodelist %>%
    dplyr::left_join(bi_degree(this_list)) %>%
    dplyr::left_join(bi_betweenness(this_list,
                                    weight_type = "frequency")) %>%
    dplyr::left_join(bi_closeness(this_list,
                                  weight_type = "frequency")) %>%
    dplyr::left_join(bi_eigen(this_list))


  # Add in city and wave info
  nodelist$city <- this_city
  nodelist$wave <- wave

  nodelist <- nodelist %>% dplyr::select(id, city, wave, dplyr::everything())


  # Gini coefficients on centrality measures
  system_level_measures <- nodelist %>%
    dplyr::group_by(mode) %>%
    dplyr::summarize(herf_degree = ideanet:::herfindahl(degree),
                     herf_norm_degree = ideanet:::herfindahl(norm_degree),
                     herf_weighted_degree = ideanet:::herfindahl(weighted_degree),
                     herf_betweenness = ideanet:::herfindahl(betweenness),
                     herf_norm_betweenness = ideanet:::herfindahl(norm_betweenness),
                     herf_closeness = ideanet:::herfindahl(closeness),

                     gini_degree = gini_manual(degree),
                     gini_norm_degree = gini_manual(norm_degree),
                     gini_weighted_degree = gini_manual(weighted_degree),
                     gini_betweenness = gini_manual(betweenness),
                     gini_norm_betweenness = gini_manual(norm_betweenness),
                     gini_closeness = gini_manual(closeness)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(city = this_city,
                  wave = wave) %>%
    dplyr::select(city, wave, dplyr::everything())

  system_level_measures$herf_eigen <- ideanet:::herfindahl(nodelist$eigen_centrality)
  system_level_measures$gini_eigen <- gini_manual(nodelist$eigen_centrality)

  # Density
  system_level_measures$density <- bi_density(this_list)

  return(list(node_level_measures = nodelist,
              system_level_measures = system_level_measures,
              threshold = cut_val))

}
