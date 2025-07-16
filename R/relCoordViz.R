relCoordViz <- function(data,
                        waves = 1,
                        this_city = 1,
                        type = NULL,
                        threshold = "1sd",
                        within_threshold = NULL,
                        ego = NULL,
                        color_node = TRUE,
                        fix_layout = 0,
                        layout_fun = igraph::layout_with_fr,
                        isolate_dist = .25) {

  # browser()

  data_subset <- data %>%
    dplyr::filter(city == this_city) %>%
    dplyr::filter(round %in% waves)

  # If we're only doing one wave, remove all cases that aren't present in the
  # network at this time:
  if (length(waves) == 1) {
    data_subset <- data_subset %>%
      dplyr::filter(!is.na(weight))
  }

  data_subset <- data_subset %>% tidyr::pivot_wider(names_from = round,
                       values_from = weight,
                       names_prefix = "weight") %>%
    dplyr::select(CASEID, alter, dplyr::starts_with("weight"), dplyr::everything()) %>%
    dplyr::select(-weight_label) %>%
    dplyr::mutate(CASEID = as.character(CASEID))

  if (!is.null(type)) {
    data_subset <- data_subset %>%
      dplyr::filter(var == type)
  }




  # Identify isolates at each timepoint (likely indicates that this person wasn't
  # present in the network at the time)
  if (1 %in% waves) {
    isolate1 <- data_subset %>%
      dplyr::group_by(CASEID) %>%
      dplyr::summarize(mean_weight = mean(weight1, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(isolate1 = is.nan(mean_weight)) %>%
      dplyr::select(id = CASEID, isolate1)
  }

  if (2 %in% waves) {
    isolate2 <- data_subset %>%
      dplyr::group_by(CASEID) %>%
      dplyr::summarize(mean_weight = mean(weight2, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(isolate2 = is.nan(mean_weight)) %>%
      dplyr::select(id = CASEID, isolate2)
  }

  if (3 %in% waves) {
    isolate3 <- data_subset %>%
      dplyr::group_by(CASEID) %>%
      dplyr::summarize(mean_weight = mean(weight3, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(isolate3 = is.nan(mean_weight)) %>%
      dplyr::select(id = CASEID, isolate3)
  }

  if (4 %in% waves) {
    isolate4 <- data_subset %>%
      dplyr::group_by(CASEID) %>%
      dplyr::summarize(mean_weight = mean(weight4, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(isolate4 = is.nan(mean_weight)) %>%
      dplyr::select(id = CASEID, isolate4)
  }


  data_subset[is.na(data_subset)] <- 0

  # Make nodelist
  data_nodelist1 <- data_subset %>%
    dplyr::group_by(CASEID, WORKGROUP) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(id = CASEID, WORKGROUP) %>%
    dplyr::mutate(mode = 1,
                  id = as.character(id))

  data_nodelist2 <- data_subset %>%
    dplyr::group_by(alter) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(id = alter) %>%
    dplyr::mutate(WORKGROUP = id,
                  mode = 2)

  this_nodelist <- dplyr::bind_rows(data_nodelist1,
                                    data_nodelist2)

  if (1 %in% waves) {
    this_nodelist <- this_nodelist %>%
      dplyr::left_join(isolate1, by = "id")
  }

  if (2 %in% waves) {
    this_nodelist <- this_nodelist %>%
      dplyr::left_join(isolate2, by = "id")
  }

  if (3 %in% waves) {
    this_nodelist <- this_nodelist %>%
      dplyr::left_join(isolate3, by = "id")
  }

  if (4 %in% waves) {
    this_nodelist <- this_nodelist %>%
      dplyr::left_join(isolate4, by = "id")
  }

  this_nodelist[is.na(this_nodelist)] <- FALSE

  this_list <- list(edgelist = data_subset,
                    nodelist = this_nodelist)


  # Need to adapt to be flexible for whatever minimum time point is
  if (min(waves) == 1) {

    baseline_vec <- data_subset$weight1[data_subset$weight1 > 0]

    baselines <- data.frame(avg_wt = mean(baseline_vec, na.rm = TRUE),
                            median_wt = median(baseline_vec, na.rm = TRUE),
                            sd_wt = sd(baseline_vec, na.rm = TRUE))
    baselines$backbone1 <- baselines$avg_wt + baselines$sd_wt
    baselines$backbone2 <- baselines$avg_wt + 2*baselines$sd_wt
  } else if (min(waves) == 2) {
    baseline_vec <- data_subset$weight2[data_subset$weight2 > 0]

    baselines <- data.frame(avg_wt = mean(baseline_vec, na.rm = TRUE),
                            median_wt = median(baseline_vec, na.rm = TRUE),
                            sd_wt = sd(baseline_vec, na.rm = TRUE))
    baselines$backbone1 <- baselines$avg_wt + baselines$sd_wt
    baselines$backbone2 <- baselines$avg_wt + 2*baselines$sd_wt
  } else if (min(waves) == 3) {
    baseline_vec <- data_subset$weight3[data_subset$weight3 > 0]

    baselines <- data.frame(avg_wt = mean(baseline_vec, na.rm = TRUE),
                            median_wt = median(baseline_vec, na.rm = TRUE),
                            sd_wt = sd(baseline_vec, na.rm = TRUE))
    baselines$backbone1 <- baselines$avg_wt + baselines$sd_wt
    baselines$backbone2 <- baselines$avg_wt + 2*baselines$sd_wt
  } else {
    baseline_vec <- data_subset$weight4[data_subset$weight4 > 0]

    baselines <- data.frame(avg_wt = mean(baseline_vec, na.rm = TRUE),
                            median_wt = median(baseline_vec, na.rm = TRUE),
                            sd_wt = sd(baseline_vec, na.rm = TRUE))
    baselines$backbone1 <- baselines$avg_wt + baselines$sd_wt
    baselines$backbone2 <- baselines$avg_wt + 2*baselines$sd_wt
  }

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

  if (is.null(within_threshold)) {
    within_cut <- cut_val
  } else {

    if (within_threshold == "mean") {
      within_cut = baselines$avg_wt
    } else if (within_threshold == "median") {
      within_cut = baselines$median_wt
    } else if (within_threshold == "1sd") {
      within_cut = baselines$backbone1
    } else if (within_threshold == "2sd") {
      within_cut = baselines$backbone2
    } else {
      within_cut = within_threshold
    }

  }

  this_list$edgelist <- this_list$edgelist %>%
    dplyr::mutate(cut_thresh = ifelse(alter == WORKGROUP, within_cut, cut_val),
                  within_group = alter == WORKGROUP)

  # Create igraph object
  this_igraph <- bi_igraph(this_list)

  # Extract ego network if applicable
  if (!is.null(ego)) {
    this_igraph <- igraph::make_ego_graph(this_igraph, nodes = ego)[[1]]
  }

  # Generate initial layout
  if (fix_layout == 1) {
    layout_graph <- igraph::delete_edges(this_igraph, which(igraph::E(this_igraph)$weight1 < igraph::E(this_igraph)$cut_thresh))
    this_layout <- layout_iso(layout_graph, isolate_dist = isolate_dist,
                              layout_fun = layout_fun)
  } else if (fix_layout == 2) {
    layout_graph <- igraph::delete_edges(this_igraph, which(igraph::E(this_igraph)$weight2 < igraph::E(this_igraph)$cut_thresh))
    this_layout <- layout_iso(layout_graph, isolate_dist = isolate_dist,
                              layout_fun = layout_fun)
  } else if (fix_layout == 3) {
    layout_graph <- igraph::delete_edges(this_igraph, which(igraph::E(this_igraph)$weight3 < igraph::E(this_igraph)$cut_thresh))
    this_layout <- layout_iso(layout_graph, isolate_dist = isolate_dist,
                              layout_fun = layout_fun)
  } else if (fix_layout == 4) {
    layout_graph <- igraph::delete_edges(this_igraph, which(igraph::E(this_igraph)$weight4 < igraph::E(this_igraph)$cut_thresh))
    this_layout <- layout_iso(layout_graph, isolate_dist = isolate_dist,
                              layout_fun = layout_fun)
  } else {
    this_layout <- NULL
  }


  # Update node color if necessary
  if (!is.null(color_node)) {

    # Get color variable values
    color_vec <- data.frame(value = igraph::V(this_igraph)$WORKGROUP)

    # Create data frame of color links
    color_link <- data.frame(value = c("parole", "family_social", "research", "city_govt",
                                       "health", "youth_out", "police", "school", "probation",
                                       "corrections", "da_office", "emp_devel", "faith_based"),
                             color = c("#ED90A4", "#E3997F", "#D0A45F", "#B3AF4F", "#8DB85E",
                                       "#5BBE7E", "#00C1A1", "#00C0C2", "#3DB9DB", "#84ADEA",
                                       "#B79FEB", "#D993DE", "#EA8DC5"))

    # Merge in color values
    color_vec <- color_vec %>%
      dplyr::left_join(color_link, by = "value")

    igraph::V(this_igraph)$color <- color_vec$color
  }


  # Plot as needed
  if (length(waves) == 2) {
    par(mfrow = c(1, 2))
  }

  if (length(waves) > 2) {
    par(mfrow = c(2, 2))
  }


  # Create unique graphs for waves as needed
  if (1 %in% waves) {
    graph1 <- igraph::delete_edges(this_igraph, which(igraph::E(this_igraph)$weight1 < igraph::E(this_igraph)$cut_thresh))

    if (fix_layout == 0) {
      plot(graph1,
           layout = layout_iso(graph1, isolate_dist = isolate_dist,
                               layout_fun = layout_fun),
           vertex.color = ifelse(igraph::V(graph1)$isolate1, NA, igraph::V(graph1)$color),
           edge.color = ifelse(igraph::E(this_igraph)$within_group, "darkblue", "grey"),
           # edge.width = igraph::E(graph1)$weight1,
           main = "Wave 1",
           sub = paste("Threshold weight:", round(cut_val, digits = 2), sep = " "))
    } else {
      plot(graph1,
           layout = this_layout,
           vertex.color = ifelse(igraph::V(graph1)$isolate1, NA, igraph::V(graph1)$color),
           edge.color = ifelse(igraph::E(this_igraph)$within_group, "darkblue", "grey"),
           # edge.width = igraph::E(graph1)$weight1,
           main = "Wave 1",
           sub = paste("Threshold weight:", round(cut_val, digits = 2), sep = " "))
    }
  }

  if (2 %in% waves) {
    graph2 <- igraph::delete_edges(this_igraph, which(igraph::E(this_igraph)$weight2 < igraph::E(this_igraph)$cut_thresh))

    if (fix_layout == 0) {
      plot(graph2,
           layout = layout_iso(graph2, isolate_dist = isolate_dist,
                               layout_fun = layout_fun),
           vertex.color = ifelse(igraph::V(graph2)$isolate2, NA, igraph::V(graph2)$color),
           edge.color = ifelse(igraph::E(this_igraph)$within_group, "darkblue", "grey"),
           # edge.width = igraph::E(graph2)$weight2,
           main = "Wave 2",
           sub = paste("Threshold weight:", round(cut_val, digits = 2), sep = " "))
    } else {
      plot(graph2,
           layout = this_layout,
           vertex.color = ifelse(igraph::V(graph2)$isolate2, NA, igraph::V(graph2)$color),
           edge.color = ifelse(igraph::E(this_igraph)$within_group, "darkblue", "grey"),
           # edge.width = igraph::E(graph2)$weight2,
           main = "Wave 2",
           sub = paste("Threshold weight:", round(cut_val, digits = 2), sep = " "))
    }
  }

  if (3 %in% waves) {
    graph3 <- igraph::delete_edges(this_igraph, which(igraph::E(this_igraph)$weight3 < igraph::E(this_igraph)$cut_thresh))

    if (fix_layout == 0) {
      plot(graph3,
           layout = layout_iso(graph3, isolate_dist = isolate_dist,
                               layout_fun = layout_fun),
           vertex.color = ifelse(igraph::V(graph3)$isolate3, NA, igraph::V(graph3)$color),
           edge.color = ifelse(igraph::E(this_igraph)$within_group, "darkblue", "grey"),
           # edge.width = igraph::E(graph3)$weight3,
           main = "Wave 3",
           sub = paste("Threshold weight:", round(cut_val, digits = 2), sep = " "))
    } else {
      plot(graph3,
           layout = this_layout,
           vertex.color = ifelse(igraph::V(graph3)$isolate3, NA, igraph::V(graph3)$color),
           edge.color = ifelse(igraph::E(this_igraph)$within_group, "darkblue", "grey"),
           # edge.width = igraph::E(graph3)$weight3,
           main = "Wave 3",
           sub = paste("Threshold weight:", round(cut_val, digits = 2), sep = " "))
    }
  }

  if (4 %in% waves) {
    graph4 <- igraph::delete_edges(this_igraph, which(igraph::E(this_igraph)$weight4 < igraph::E(this_igraph)$cut_thresh))

    if (fix_layout == 0) {
      plot(graph4,
           layout = layout_iso(graph4, isolate_dist = isolate_dist,
                               layout_fun = layout_fun),
           vertex.color = ifelse(igraph::V(graph4)$isolate4, NA, igraph::V(graph4)$color),
           edge.color = ifelse(igraph::E(this_igraph)$within_group, "darkblue", "grey"),
           # edge.width = igraph::E(graph4)$weight4,
           main = "Wave 4",
           sub = paste("Threshold weight:", round(cut_val, digits = 2), sep = " "))
    } else {
      plot(graph4,
           layout = this_layout,
           vertex.color = ifelse(igraph::V(graph4)$isolate4, NA, igraph::V(graph4)$color),
           edge.color = ifelse(igraph::E(this_igraph)$within_group, "darkblue", "grey"),
           # edge.width = igraph::E(graph4)$weight4,
           main = "Wave 4",
           sub = paste("Threshold weight:", round(cut_val, digits = 2), sep = " "))
    }
  }

  relcoord_graphViz <- recordPlot()

  dev.off()

  return(relcoord_graphViz)

}
