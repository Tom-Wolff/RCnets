# User-facing function that can handle multiple cities, waves, and types
relCoordMeasures <- function(data,
                             types = NULL,
                             cities = 1,
                             waves = 1,
                             threshold = NULL,
                             keep_weight = FALSE) {

  # browser()

  if (is.null(types)) {
    types <- "NULL"
  }

  # Create a reference sheet of all possible city/wave combinations over which to iterate
  combos <- expand.grid(city = cities, wave = waves, type = types, threshold = threshold)

  for (i in 1:nrow(combos)) {

    this_type <- NULL

    if (combos[i, "type"] != "NULL") {
      this_type <- as.character(combos[i, "type"])
    }

    # Run relCoordMeasure for this city/wave combination
    this_data <- suppressWarnings(
      suppressMessages(
        relCoordMeasure(data = data,
                        wave = combos[i, "wave"],
                        this_city = combos[i, "city"],
                        type = this_type,
                        threshold = combos[i, "threshold"],
                        keep_weight = keep_weight)
      ))

    # Extract node measures and system measures dataframes
    node_df <- this_data$node_level_measures %>%
      dplyr::mutate(type = ifelse(is.null(this_type), "Full Scale", this_type),
                    threshold = combos[i, "threshold"]) %>%
      dplyr::select(id:mode, type, threshold, dplyr::everything())
    system_df <- this_data$system_level_measures %>%
      dplyr::mutate(type = ifelse(is.null(this_type), "Full Scale", this_type),
                    threshold = combos[i, "threshold"]) %>%
      dplyr::select(city, wave, type, threshold, dplyr::everything())


    if (i == 1) {
      node_level_long <- node_df
      system_level_long <- system_df
    } else {
      node_level_long <- dplyr::bind_rows(node_level_long, node_df)
      system_level_long <- dplyr::bind_rows(system_level_long, system_df)
    }
  }

  return(list(node_level = node_level_long,
              system_level = system_level_long))

}





# Core function for handling one city/wave/type combination
relCoordMeasure <- function(data,
                            wave = 1,
                            this_city = 1,
                            type = NULL,
                            threshold = NULL,
                            keep_weight = FALSE) {
  # browser()


  if (!is.null(type)) {
    data_subset <- data %>%
      dplyr::filter(city == this_city) %>%
      dplyr::filter(round == wave) %>%
      select(CASEID, alter, var, dplyr::starts_with("weight"), dplyr::everything()) %>%
      dplyr::mutate(CASEID = as.character(CASEID)) %>%
      dplyr::filter(var == type)
  } else {
    data_subset <- data %>%
      dplyr::filter(city == this_city) %>%
      dplyr::filter(round == wave) %>%
      select(CASEID, alter, dplyr::starts_with("weight"), dplyr::everything()) %>%
      dplyr::mutate(CASEID = as.character(CASEID))
  }

  baselines <- data.frame(avg_wt = mean(data_subset$weight, na.rm = TRUE),
                          median_wt = median(data_subset$weight, na.rm = TRUE),
                          sd_wt = sd(data_subset$weight, na.rm = TRUE))
  baselines$backbone1 <- baselines$avg_wt + baselines$sd_wt
  baselines$backbone2 <- baselines$avg_wt + 2*baselines$sd_wt


  # Before we backbone, take inventory of full nodelist and identify isolates
  full_nodelist1 <- data_subset %>%
    dplyr::group_by(CASEID, WORKGROUP) %>%
    dplyr::summarize(mean_wt = mean(weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.nan(mean_wt)) %>%
    dplyr::mutate(mode = 1) %>%
    dplyr::select(-mean_wt) %>%
    dplyr::rename(id = CASEID)

  full_nodelist2 <- data_subset %>%
    dplyr::group_by(alter) %>%
    dplyr::summarize(mean_wt = mean(weight, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!is.nan(mean_wt)) %>%
    dplyr::mutate(mode = 2) %>%
    dplyr::select(-mean_wt) %>%
    dplyr::rename(id = alter) %>%
    dplyr::mutate(WORKGROUP = id)

  full_nodelist <- dplyr::bind_rows(full_nodelist1, full_nodelist2)

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

  # Replace generated nodelist with full_nodelist just in case
  this_list$nodelist <- full_nodelist

  this_igraph <- bi_igraph(this_list)

  # Get degree counts for within/across-group ties
  group_match <- data_subset %>%
    dplyr::mutate(within_group = alter == WORKGROUP,
                  across_group = alter != WORKGROUP)
  # Mode 1
  mode1_match <- group_match %>%
    dplyr::group_by(CASEID) %>%
    dplyr::summarize(within_degree = sum(within_group),
                     across_degree = sum(across_group)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mode = 1)

  ### Normalizing
  mode1_denoms <- full_nodelist %>%
    dplyr::group_by(mode) %>%
    summarize(count = dplyr::n()-1) %>%
    dplyr::ungroup() %>%
    dplyr::filter(mode == 2) %>%
    dplyr::mutate(mode = 1)

  mode1_match <- mode1_match %>%
    dplyr::left_join(mode1_denoms, by = "mode") %>%
    dplyr::mutate(norm_within_degree = within_degree/1,
                  norm_across_degree = across_degree/count) %>%
    dplyr::select(-count) %>%
    dplyr::select(id = CASEID, dplyr::contains("within_degree"), dplyr::contains("across_degree"))

  # Mode 2
  mode2_match <- group_match %>%
    dplyr::group_by(alter) %>%
    dplyr::summarize(within_degree = sum(within_group),
                     across_degree = sum(across_group)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(mode = 2)

  ### Normalizing
  mode2_denomsA <- full_nodelist %>%
    dplyr::filter(mode == 2)

  mode2_denomsB <- full_nodelist %>%
    dplyr::filter(mode == 1) %>%
    dplyr::mutate(nrow = dplyr::n()) %>%
    dplyr::group_by(WORKGROUP, nrow) %>%
    summarize(within_count = dplyr::n(),
              across_count = nrow - within_count) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::rename(id = WORKGROUP)

  mode2_denoms <- mode2_denomsA %>%
    dplyr::left_join(mode2_denomsB, by = "id") %>%
    dplyr::mutate(nrow = max(nrow, na.rm = TRUE),
                  within_count = ifelse(is.na(within_count), 0, within_count),
                  across_count = ifelse(is.na(across_count), nrow, across_count)) %>%
    select(id, dplyr::contains("count"))

  mode2_match <- mode2_match %>%
    dplyr::rename(id = alter) %>%
    dplyr::left_join(mode2_denoms, by = "id") %>%
    dplyr::mutate(norm_within_degree = within_degree/within_count,
                  norm_across_degree = across_degree/across_count) %>%
    dplyr::select(id, dplyr::contains("within_degree"), dplyr::contains("across_degree"))


  group_degree <- dplyr::bind_rows(mode1_match, mode2_match)




  nodelist <- this_list$nodelist %>%
    dplyr::left_join(bi_degree(this_list)) %>%
    dplyr::left_join(group_degree, by = "id") %>%
    dplyr::left_join(bi_betweenness(this_list,
                                    weight_type = "frequency")) %>%
    dplyr::left_join(bi_closeness(this_list,
                                  weight_type = "frequency")) %>%
    dplyr::left_join(bi_eigen(this_list))


  # Add in city and wave info
  nodelist$city <- this_city
  nodelist$wave <- wave

  nodelist <- nodelist %>% dplyr::select(id, city, wave, dplyr::everything())

  # browser()
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
