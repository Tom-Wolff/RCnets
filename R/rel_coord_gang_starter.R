#####################################
#    D A T A   R E S H A P I N G    #
#####################################

library(tidyverse)
library(ideanet)

# Read in Relational Coordination Data
load("~/Desktop/ICPSR_37453/DS0002/37453-0002-Data.rda")

# Don't like the dataset name here, renaming to `raw_data`
raw_data <- da37453.0002

# This dataset is basically a bunch of two-mode adjacency matrices stitched
# together in a long dataset. The code below shows how we would extract the first
# of these two-mode adjacency matrices and rename variables and values to
# ensure aggregation into a proper adjacency matrix
data <- raw_data %>%
  mutate(city = (raw_data$CASEID - raw_data$CASEID %% 100)/100,
         WORKGROUP = case_when(stringr::str_detect(WORKGROUP, "(01)") ~ "research",
                               stringr::str_detect(WORKGROUP, "(02)") ~ "city_govt",
                               stringr::str_detect(WORKGROUP, "(03)") ~ "corrections",
                               stringr::str_detect(WORKGROUP, "(04)") ~ "da_office",
                               stringr::str_detect(WORKGROUP, "(05)") ~ "emp_devel",
                               stringr::str_detect(WORKGROUP, "(06)") ~ "faith_based",
                               stringr::str_detect(WORKGROUP, "(07)") ~ "family_social",
                               stringr::str_detect(WORKGROUP, "(08)") ~ "health",
                               stringr::str_detect(WORKGROUP, "(09)") ~ "parole",
                               stringr::str_detect(WORKGROUP, "(10)") ~ "police",
                               stringr::str_detect(WORKGROUP, "(11)") ~ "probation",
                               stringr::str_detect(WORKGROUP, "(12)") ~ "school",
                               stringr::str_detect(WORKGROUP, "(13)") ~ "youth_out",
                               TRUE ~ NA),
         intervention2 = INTERVENTION2 == "(01) Yes",
         intervention3 = INTERVENTION3 == "(1) Yes",
         intervention4 = INTERVENTION3 == "(1) Yes"
  ) %>%
  dplyr::select(city, CASEID, WORKGROUP, intervention2, intervention3, intervention4, dplyr::everything())

names(data)

# The first number, with Q prefix, is the type of question
names(data) <- stringr::str_replace(names(data), "Q3_", "VAR_commFreq_")
names(data) <- stringr::str_replace(names(data), "Q5_", "VAR_commTimely_")
names(data) <- stringr::str_replace(names(data), "Q7_", "VAR_commAccurately_")
names(data) <- stringr::str_replace(names(data), "Q9_", "VAR_blameOthers_")
names(data) <- stringr::str_replace(names(data), "Q10_", "VAR_shareGoals_")
names(data) <- stringr::str_replace(names(data), "Q11_", "VAR_knowYourWork_")
names(data) <- stringr::str_replace(names(data), "Q12_", "VAR_respectYourWork_")

# The second number is the role being asked about
names(data) <- stringr::str_replace(names(data), "_1_", "_cityGovt_")
names(data) <- stringr::str_replace(names(data), "_2_", "_corrections_")
names(data) <- stringr::str_replace(names(data), "_3_", "_daOffice_")
names(data) <- stringr::str_replace(names(data), "_4_", "_empDevel_")
names(data) <- stringr::str_replace(names(data), "_5_", "_faithBased_")
names(data) <- stringr::str_replace(names(data), "_6_", "_familySocial_")
names(data) <- stringr::str_replace(names(data), "_7_", "_health_")
names(data) <- stringr::str_replace(names(data), "_8_", "_parole_")
names(data) <- stringr::str_replace(names(data), "_9_", "_police_")
names(data) <- stringr::str_replace(names(data), "_10_", "_probation_")
names(data) <- stringr::str_replace(names(data), "_11_", "_research_")
names(data) <- stringr::str_replace(names(data), "_12_", "_school_")
names(data) <- stringr::str_replace(names(data), "_13_", "_youthOut_")
names(data)

data2 <- data %>%
  dplyr::select(city, CASEID, WORKGROUP, intervention2, intervention3, intervention4,
                dplyr::starts_with("VAR_")) %>%
  tidyr::pivot_longer(cols = dplyr::starts_with("VAR_"),
                      names_to = "fullname",
                      values_to = "weight") %>%
  dplyr::mutate(fullname = stringr::str_remove(fullname, "^VAR_"),
                var = stringr::str_extract(fullname, "^[^_]+"),
                alter = sub("_.*", "", sub("^[^_]+_", "", fullname)),
                round = as.numeric(stringr::str_extract(fullname, "\\d$")),
                weight2 = as.numeric(stringr::str_extract(weight, "\\d"))
  ) %>%
  dplyr::select(-fullname) %>%
  dplyr::select(city, round, CASEID, WORKGROUP, intervention2, intervention3, intervention4,
                alter, var, weight = weight2, weight_label = weight) %>%
  dplyr::mutate(alter = dplyr::case_when(alter == "cityGovt" ~ "city_govt",
                                         alter == "daOffice" ~ "da_office",
                                         alter == "empDevel" ~ "emp_devel",
                                         alter == "faithBased" ~ "faith_based",
                                         alter == "familySocial" ~ "family_social",
                                         alter == "youthOut" ~ "youth_out",
                                         TRUE ~ alter))

write.csv(data2, "~/Desktop/ICPSR_37453/RC_long_edgelist.csv")


###############################################################################
#    C R E A T I N G   A V E R A G E D ,   ' S C A L E D '   W E I G H T S    #
###############################################################################

data3 <- data2 %>%
  dplyr::filter(!is.na(weight)) %>%
  dplyr::group_by(city, round, CASEID, WORKGROUP, alter) %>%
  dplyr::summarize(weight = mean(weight, na.rm = TRUE),
                   num_responses = dplyr::n()) %>%
  dplyr::ungroup()

write.csv(data3, "~/Desktop/ICPSR_37453/RC_scaled_edgelist.csv")


###############################################################
#    C R E A T E   A N D   C O M P A R E   N E T W O R K S    #
###############################################################

# Function for generating visualizations







test <- relCoordMeasure(data = data3,
                wave = 3,
                this_city = 1,
                threshold = "1sd")

View(test$node_level_measures)
View(test$system_level_measures)

test2 <- relCoordMeasure(data = data3,
                        wave = 2,
                        this_city = 1,
                        threshold = "1sd",
                        keep_weight = TRUE)

View(test2$node_level_measures)

View(test$system_level_measures)



city1_allWaves <- relCoordViz(data = data3,
                              waves = c(1, 2, 3, 4),
                              this_city = 1,
                              threshold = "1sd",
                              layout_fun = igraph::layout_as_bipartite)
city1_allWaves <- relCoordViz(data = data3,
                              waves = c(1, 2, 3, 4),
                              this_city = 1,
                              threshold = "1sd")
city1_allWaves


city2_allWaves <- relCoordViz(data = data3,
                              waves = c(1, 2, 3, 4),
                              this_city = 2)
city2_allWaves

city3_allWaves <- relCoordViz(data = data3,
                              waves = c(1, 2, 3, 4),
                              this_city = 3)
city3_allWaves

city4_allWaves <- relCoordViz(data = data3,
                              waves = c(1, 2, 3, 4),
                              this_city = 4)
city4_allWaves

##########################################################



# Record column numbers for select variable
q3 <- 7:19
q5 <- which(stringr::str_detect(names(data), "^Q5") & stringr::str_detect(names(data), "1$"))
q7 <- which(stringr::str_detect(names(data), "^Q7") & stringr::str_detect(names(data), "1$"))
q9 <- 46:58
q10 <- which(stringr::str_detect(names(data), "^Q10") & stringr::str_detect(names(data), "1$"))
q11 <- which(stringr::str_detect(names(data), "^Q11") & stringr::str_detect(names(data), "1$"))
q12 <- which(stringr::str_detect(names(data), "^Q12") & stringr::str_detect(names(data), "1$"))
# Q5 - comm_timely
# Q7 - comm_accurately
# Q9 - blame_others
# Q10 - share_goals
# Q11 - know_your_work
# Q12 - respect_your_work

# Q23 - formal working relationships


# Q3 - comm_freq

q3 <- data[, c(1:6, q3)]

institution_order <- c("city_govt", "corrections", "da_office",
                       "emp_devel", "faith_based", "family_social",
                       "health", "parole", "police", "probation",
                       "research", "school", "youth_out")

# Extracting columns for the first two-mode adjmat
colnames(q3)[7:19] <- institution_order

# Functions for calculating various weights
### Number of people within institution reporting a relationship with the
### receiving institution
sum_not_na <- function(x) {
  return(sum(!is.na(x)))
}

### Proportion of people within institution reporting a relationship with the
### receiving institution
prop_not_na <- function(x) {
  return(sum(!is.na(x))/length(x))
}

### Among of people within institution reporting a relationship with the
### receiving institution, average value reported
mean_not_na <- function(x) {
  return(mean(as.numeric(x), na.rm = TRUE))
}

make_el <- function(qdata, qlabel) {
  # browser()
  # Aggregate responses in two-mode matrix to create one-mode matrix
  ### Number of people within institution reporting a relationship with the
  ### receiving institution
  sum_edgelist <- qdata %>%
    # select(-CASEID) %>%
    group_by(CASEID) %>%
    # summarize_all(sum_not_na) %>%
    # summarize(across(city_govt:youth_out, sum_not_na))
    ungroup() %>%
    pivot_longer(city_govt:youth_out, names_to = "alter", values_to = "weight") %>%
    filter(!is.na(weight)) %>%
    mutate(weight = as.numeric(weight))
    # mutate(weight = as.numeric(weight),
    #        weight2 = weight,
    #        type = "sum")

  # ### Proportion of people within institution reporting a relationship with the
  # ### receiving institution
  # prop_edgelist <- qdata %>%
  #   # select(-CASEID) %>%
  #   group_by(WORKGROUP) %>%
  #   # ummarize_all(prop_not_na) %>%
  #   across(city_govt:youth_out)
  #   ungroup() %>%
  #   pivot_longer(city_govt:youth_out, names_to = "alter", values_to = "weight") %>%
  #   mutate(weight2 = weight*10,
  #          type = "prop") %>%
  #   filter(weight > 0)
  #
  #
  # ### Among of people within institution reporting a relationship with the
  # ### receiving institution, average value reported
  # mean_edgelist <- qdata %>%
  #   select(-CASEID) %>%
  #   group_by(WORKGROUP) %>%
  #   summarize_all(mean_not_na) %>%
  #   ungroup() %>%
  #   pivot_longer(city_govt:youth_out, names_to = "alter", values_to = "weight") %>%
  #   mutate(weight2 = weight,
  #          type = "mean_val") %>%
  #   filter(!is.nan(weight2))


  # Merge together edgelists to pass through `netwrite`
  # edgelist <- dplyr::bind_rows(sum_edgelist,
  #                              prop_edgelist,
  #                              mean_edgelist) %>%
  edgelist <- sum_edgelist %>%
    dplyr::mutate(label = qlabel)
  return(edgelist)
}

q3_el <- make_el(q3, "comm_freq")


# Q5 - comm_timely
q5 <- data[, c(1:6, q5)]

institution_order <- c("city_govt", "corrections", "da_office",
                       "emp_devel", "faith_based", "family_social",
                       "health", "parole", "police", "probation",
                       "research", "school", "youth_out")

# Extracting columns for the first two-mode adjmat
colnames(q5)[7:19] <- institution_order

q5_el <- make_el(q5, "comm_timely")

# Q7 - comm_accurately
q7 <- data[, c(1:6, q7)]

institution_order <- c("city_govt", "corrections", "da_office",
                       "emp_devel", "faith_based", "family_social",
                       "health", "parole", "police", "probation",
                       "research", "school", "youth_out")

# Extracting columns for the first two-mode adjmat
colnames(q7)[7:19] <- institution_order

q7_el <- make_el(q7, "comm_accurately")

# Q9 - blame_others
q9 <- data[, c(1:6, q9)]

institution_order <- c("city_govt", "corrections", "da_office",
                       "emp_devel", "faith_based", "family_social",
                       "health", "parole", "police", "probation",
                       "research", "school", "youth_out")

# Extracting columns for the first two-mode adjmat
colnames(q9)[7:19] <- institution_order

q9_el <- make_el(q9, "blame_others")

# Q10 - share_goals
q10 <- data[, c(1:6, q10)]

institution_order <- c("city_govt", "corrections", "da_office",
                       "emp_devel", "faith_based", "family_social",
                       "health", "parole", "police", "probation",
                       "research", "school", "youth_out")

# Extracting columns for the first two-mode adjmat
colnames(q10)[7:19] <- institution_order

q10_el <- make_el(q10, "share_goals")

# Q11 - know_your_work
q11 <- data[, c(1:6, q11)]

institution_order <- c("city_govt", "corrections", "da_office",
                       "emp_devel", "faith_based", "family_social",
                       "health", "parole", "police", "probation",
                       "research", "school", "youth_out")

# Extracting columns for the first two-mode adjmat
colnames(q11)[7:19] <- institution_order

q11_el <- make_el(q11, "know_your_work")

# Q12 - respect_your_work
q12 <- data[, c(1:6, q12)]

institution_order <- c("city_govt", "corrections", "da_office",
                       "emp_devel", "faith_based", "family_social",
                       "health", "parole", "police", "probation",
                       "research", "school", "youth_out")

# Extracting columns for the first two-mode adjmat
colnames(q12)[7:19] <- institution_order

q12_el <- make_el(q12, "respect_your_work")

# Q23 - formal working relationships

################################################################################
# Merge into a broader long edgelist
long_el <- dplyr::bind_rows(q3_el,
                            q5_el,
                            q7_el,
                            q9_el,
                            q10_el,
                            q11_el,
                            q12_el) %>%
  select(city, CASEID, WORKGROUP, alter, dplyr::everything())

# Create mean "scale" of weights
mean_scale <- long_el %>%
  dplyr::group_by(CASEID, alter) %>%
  dplyr::summarize(mean_scale = mean(weight, na.rm = TRUE),
                   n_values = dplyr::n()) %>%
  dplyr::ungroup()


# Make wide version
wide_el <-  long_el %>%
  pivot_wider(names_from = label,
              values_from = weight) %>%
  select(city, CASEID, WORKGROUP, alter, dplyr::everything()) %>%
  dplyr::left_join(mean_scale, by = c("CASEID", "alter"))


################################################################################
# Extract Data for Each City
get_city_data <- function(el_data = wide_el,
                          city_num = 1) {

# Filter edgelist to city
el_filter <- el_data %>%
  dplyr::filter(city == city_num)

# Get nodelist
nodelist <- el_filter %>%
  dplyr::group_by(CASEID) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(city, CASEID, WORKGROUP, dplyr::starts_with("intervention")) %>%
  dplyr::mutate(CASEID = as.character(CASEID))

# Get edgelist
edgelist <- el_filter %>%
  dplyr::select(CASEID, alter, comm_freq:n_values) %>%
  dplyr::mutate(CASEID = as.character(CASEID),
                alter = as.character(alter))

# Make second nodelist for organization mode
org_nodelist <- data.frame(city = unique(nodelist$city),
                           CASEID = unique(edgelist$alter),
                           WORKGROUP = unique(edgelist$alter))
### Merge into full nodelist
nodelist <- dplyr::bind_rows(nodelist, org_nodelist) %>%
  dplyr::select(CASEID, dplyr::everything())

return(list(nodelist = nodelist, edgelist = edgelist))
}

city1 <- get_city_data(city_num = 1)
city2 <- get_city_data(city_num = 2)
city3 <- get_city_data(city_num = 3)
city4 <- get_city_data(city_num = 4)

################################################################################
# Create Bipartite igraph objects

bi_igraph <- function(city) {
  # Make standard igraph object from data frames
  regular_graph <- igraph::graph_from_data_frame(city$edgelist,
                                                 directed = FALSE,
                                                 vertices = city$nodelist)
  # Identify modes
  bi_map <- igraph::bipartite_mapping(regular_graph)

  igraph::V(regular_graph)$type <- bi_map$type
  igraph::V(regular_graph)$shape <- ifelse(bi_map$type, "square", "circle")
  igraph::V(regular_graph)$color <- ifelse(bi_map$type, "salmon", "lightblue")

  return(regular_graph)

}

city1_graph <- bi_igraph(city1)
city2_graph <- bi_igraph(city2)
city3_graph <- bi_igraph(city3)
city4_graph <- bi_igraph(city4)

################################################################################
# Generating Visualizations with Specifications
bi_plot <- function(bigraph,
                    edge_type = NULL,
                    min_weight = NULL,
                    max_weight = NULL,
                    layout = "fr",
                    color_node = NULL) {

  # browser()

  # Confirm max_weight greater than or equal to min_weight
  if (!is.null(max_weight) & !is.null(min_weight)) {
      if (max_weight < min_weight) {
        stop("Minimum weight value exceeds maximum weight value")
      }
  }

  # Select Edge Type
  if (!is.null(edge_type)) {
    bigraph <- igraph::delete_edges(bigraph,
                                    which(is.na(
                                      eval(parse(
                                        text = paste("igraph::E(bigraph)$", edge_type, sep = "")))
                                    )
                                    )
    )
  }

  # Remove anything below minimum weight value
  if (!is.null(min_weight)) {
    bigraph <- igraph::delete_edges(bigraph,
                                    which(
                                      eval(parse(
                                        text = paste("igraph::E(bigraph)$", edge_type, " <= ", min_weight, sep = "")))

                                    )
    )
  }

  # Remove anything above maximum weight value
  if (!is.null(max_weight)) {
    bigraph <- igraph::delete_edges(bigraph,
                                    which(
                                      eval(parse(
                                        text = paste("igraph::E(bigraph)$", edge_type, " >= ", max_weight, sep = "")))

                                    )
    )
  }

  # Determine layout
  if (layout == "bipartite") {
    save_layout <- igraph::layout.bipartite(bigraph)
  } else {
    save_layout <- igraph::layout.fruchterman.reingold(bigraph)
  }


  # Update node color if necessary
  if (!is.null(color_node)) {

    # Get color variable values
    color_vec <- data.frame(value = eval(parse(text = paste("igraph::V(bigraph)$", color_node, sep = ""))))

    # Create data frame of color links
    color_link <- data.frame(value = unique(color_vec$value),
                             color = colorspace::qualitative_hcl(length(unique(color_vec$value)),
                                                                 palette = "Set 2"))

    # Merge in color values
    color_vec <- color_vec %>%
      dplyr::left_join(color_link, by = "value")

    igraph::V(bigraph)$color <- color_vec$color

    }


  # Plot updated graph
  plot(bigraph, layout = save_layout)

}

################################################################################
# Creating one-mode projection edgelists with customizable calculation
projection_el <- function(city,
                          within_fun = mean,
                          agg_fun = sum,
                          directed = FALSE) {

  # Store a starting edgelist
  el1 <- city$edgelist
  ### Pivot longer
  el1 <- el1 %>%
    tidyr::pivot_longer(comm_freq:n_values,
                        names_to = "type",
                        values_to = "weight")
  colnames(el1) <- paste(names(el1), 1, sep = "")
  colnames(el1)[2] <- "alter"
  colnames(el1)[3] <- "type"


  el2 <- el1
  colnames(el2) <- stringr::str_replace_all(colnames(el2), "1", "2")

  full_el <- el1 %>%
    dplyr::left_join(el2, by = c("alter", "type"),
                     relationship = "many-to-many") %>%
    dplyr::select(CASEID1, alter, CASEID2, dplyr::everything()) %>%
    dplyr::filter(CASEID1 != CASEID2) %>%
    dplyr::filter(!is.na(weight1) & !is.na(weight2))

  if (is.null(within_fun)) {
    full_el$weight3 <- 1
  } else {

    if (is.character(within_fun)) {
      agg_fun <- eval(parse(text = within_fun))
    }

    full_el$weight3 <- mapply(within_fun, full_el$weight1, full_el$weight2)
  }


  if (is.character(agg_fun)) {
    agg_fun <- eval(parse(text = agg_fun))
  }

  agg_el <- full_el %>%
    dplyr::group_by(CASEID1, CASEID2, type) %>%
    dplyr::summarize(weight = agg_fun(weight3)) %>%
    dplyr::ungroup()

  # Create dyad IDs
  agg_el <- agg_el %>%
    dplyr::mutate(node1 = ifelse(CASEID1 <= CASEID2, CASEID1, CASEID2),
                  node2 = ifelse(CASEID1 <= CASEID2, CASEID2, CASEID1)) %>%
    dplyr::group_by(node1, node2) %>%
    dplyr::mutate(dyad_id = dplyr::cur_group_id()) %>%
    dplyr::ungroup()

  if (isTRUE(directed)) {
    agg_el <- agg_el %>%
      dplyr::select(CASEID1, CASEID2, dyad_id, dplyr::everything()) %>%
      dplyr::select(-node1, -node2)
  } else {
    agg_el <- agg_el %>%
      dplyr::filter(CASEID1 < CASEID2) %>%
      dplyr::select(CASEID1, CASEID2, dyad_id, dplyr::everything()) %>%
      dplyr::select(-node1, -node2)
  }

  return(agg_el)

}


bi_plot(city1_graph,
        edge_type = "blame_others",
        min_weight = 4,
        # max_weight = 3,
        #layout = "bipartite"
        color_node = "WORKGROUP"
        )

bi_plot(city1_graph,
        edge_type = "mean_scale",
        min_weight = 3,
        color_node = "WORKGROUP")

bi_plot(city1_graph,
        color_node = "WORKGROUP",
        layout = "bipartite")

plot(city4_graph)


igraph::E(test1)$mean_scale > 3

test1_3 <- igraph::delete_edges(test1, which(igraph::E(test1)$mean_scale < 3))

plot(test1_3,
     layout = igraph::layout.fruchterman.reingold(test1_3))

plot(test1_3,
     layout = igraph::layout.bipartite(test1_3, type = igraph::V(test1_3)$mode))

test1_4 <- igraph::delete_edges(test1, which(igraph::E(test1)$mean_scale < 4))

plot(test1_4,
     layout = igraph::layout.bipartite(test1_4, type = igraph::V(test1_4)$mode))

plot(test1_4)
     #layout = igraph::layout.bipartite(test1_4, type = igraph::V(test1_4)$mode))


plot(test1)

################################################################################
# Pass through `netwrite`

q3_prop_nw <- netwrite(i_elements = q3_edgelist$WORKGROUP,
         j_elements = q3_edgelist$alter,
         weights = q3_edgelist$weight2,
         type = q3_edgelist$type,
         directed = TRUE)

# Visualize networks
### Number of people within institution reporting a relationship with the
### receiving institution
plot(q3_prop_nw$igraph_list$prop,
     edge.arrow.size = .5,
     edge.width = igraph::E(q3_prop_nw$igraph_list$prop)$weight/5,
     vertex.label = igraph::V(q3_prop_nw$igraph_list$prop)$attr)

### Proportion of people within institution reporting a relationship with the
### receiving institution
plot(q3_prop_nw$igraph_list$sum,
     edge.arrow.size = .5,
     edge.width = igraph::E(q3_prop_nw$igraph_list$sum)$weight/5,
     vertex.label = igraph::V(q3_prop_nw$igraph_list$sum)$attr)

### Among of people within institution reporting a relationship with the
### receiving institution, average value reported
plot(q3_prop_nw$igraph_list$mean_val,
     edge.arrow.size = .5,
     edge.width = igraph::E(q3_prop_nw$igraph_list$mean_val)$weight/2.5,
     vertex.label = igraph::V(q3_prop_nw$igraph_list$mean_val)$attr)
