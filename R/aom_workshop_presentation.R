# Wishlist


# a.1. Time 1, City A
cityA_Wave1 <- relCoordViz(data = rcScaled,
                              waves = 1,
                              this_city = 2,
                              threshold = 4.0000001)
cityA_Wave1

# a.2. Time 2, City B
cityB_Wave1 <- relCoordViz(data = rcScaled,
                           waves = 1,
                           this_city = 1,
                           threshold = 4.0000001)
cityB_Wave1




# b. Cities A + B, all Waves, full scale

## City A
### Viz, overall scale > 4
cityA_allWaves <- relCoordViz(data = rcScaled,
                              waves = 1:4,
                              this_city = 2,
                              threshold = 4.0000001)
cityA_allWaves

## City B
### Viz, overall scale > 4
cityB_allWaves <- relCoordViz(data = rcScaled,
                              waves = 1:4,
                              this_city = 1,
                              threshold = 4.0000001)
cityB_allWaves




# c.1. City A, knowYourWork, Wave 1
cityA_knowWork4 <- relCoordViz(data = rcLong,
                               waves = 1,
                               this_city = 2,
                               type = "knowYourWork",
                               threshold = 4.0000001)
cityA_knowWork4



# c.2. City A, timelyComm, Wave 1
cityA_timelyComm4 <- relCoordViz(data = rcLong,
                               waves = 1,
                               this_city = 2,
                               type = "commTimely",
                               threshold = 4.0000001)
cityA_timelyComm4

# d.1. Density, just full measure
scaled_measures <- relCoordMeasures(data = rcScaled,
                                    cities = 1:2,
                                    waves = 1:4,
                                    threshold = c(4.0000001))

dimension_measures <- relCoordMeasures(data = rcLong,
                                       cities = 1:2,
                                       waves = 1:4,
                                       types = c("commTimely", "knowYourWork"),
                                       threshold = 4.0000001)



full_system_data <- dplyr::bind_rows(scaled_measures$system_level,
                                     dimension_measures$system_level) %>%
  dplyr::mutate(type = factor(type, levels = c("Full Scale", "commTimely", "knowYourWork"), ordered = TRUE),
                city_relabel = ifelse(city == 1, "B", "A"))


full_system_data %>%
  dplyr::group_by(city, wave, type) %>%
  dplyr::slice(1) %>%
  dplyr::filter(type == "Full Scale") %>%
  # ggplot2::ggplot(ggplot2::aes(x = wave, y = density, color = as.factor(city_relabel),
  #                              linetype = type)) +
  ggplot2::ggplot(ggplot2::aes(x = wave, y = density, color = as.factor(city_relabel))) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Network Density (Threshold Weight = 4)",
                color = "City",
                # linetype = "Measure",
                x = "Wave",
                y = "Density")


# d.2. Gini, just full measure
full_system_data %>%
  dplyr::mutate(mode_label = ifelse(mode == 1, "Respondents", "Role Groups")) %>%
  dplyr::filter(type == "Full Scale") %>%
  # ggplot2::ggplot(ggplot2::aes(x = wave, y = gini_degree, color = as.factor(city_relabel),
  #                              linetype = type)) +
  ggplot2::ggplot(ggplot2::aes(x = wave, y = gini_degree, color = as.factor(city_relabel))) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Gini Coefficient, Degree (Threshold Weight = 4)",
                color = "City",
                # linetype = "Measure",
                x = "Wave",
                y = "Gini Coefficient") +
  ggplot2::facet_grid(cols = vars(mode_label))


# e.1. Full density plot (supplementary)
full_system_data %>%
  dplyr::group_by(city, wave, type) %>%
  dplyr::slice(1) %>%
  ggplot2::ggplot(ggplot2::aes(x = wave, y = density, color = as.factor(city_relabel),
                               linetype = type)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Network Density (Threshold Weight = 4)",
                color = "City",
                linetype = "Measure",
                x = "Wave",
                y = "Density")


# e.2. Full Gini plot (supplementary)
full_system_data %>%
  dplyr::mutate(mode_label = ifelse(mode == 1, "Respondents", "Role Groups")) %>%
  ggplot2::ggplot(ggplot2::aes(x = wave, y = gini_degree, color = as.factor(city_relabel),
                                linetype = type)) +
  ggplot2::geom_point() +
  ggplot2::geom_line() +
  ggplot2::theme_classic() +
  ggplot2::labs(title = "Gini Coefficient, Degree (Threshold Weight = 4)",
                color = "City",
                linetype = "Measure",
                x = "Wave",
                y = "Gini Coefficient") +
  ggplot2::facet_grid(cols = vars(mode_label))


# f. City A, Time 1, fully dense
cityA_Wave1 <- relCoordViz(data = rcScaled,
                           waves = 1,
                           this_city = 2,
                           threshold = 0)
cityA_Wave1




### Viz, shared knowledge > 4
cityA_knowWork4 <- relCoordViz(data = rcLong,
                              waves = 1:4,
                              this_city = 1,
                              type = "knowYourWork",
                              threshold = 4.0000001)
city1_knowWork4

### Viz, timely communication > 4
city1_commTimely4 <- relCoordViz(data = rcLong,
                             waves = 1:4,
                             this_city = 1,
                             type = "commTimely",
                             threshold = 4.0000001)
city1_commTimely4


## City 2
### Viz, overall scale > 4
city2_allWaves <- relCoordViz(data = rcScaled,
                              waves = 1:4,
                              this_city = 2,
                              threshold = 4.0000001)
city2_allWaves

### Viz, shared knowledge > 4
city2_knowWork4 <- relCoordViz(data = rcLong,
                               waves = 1:4,
                               this_city = 2,
                               type = "knowYourWork",
                               threshold = 4.0000001)
city2_knowWork4

### Viz, timely communication > 4
city2_commTimely4 <- relCoordViz(data = rcLong,
                                 waves = 1:4,
                                 this_city = 2,
                                 type = "commTimely",
                                 threshold = 4.0000001)
city2_commTimely4


