austraits_weighted_means <- function(austraits, traits) {
  
  library(austraits)
  
  
  # any data that is a mean, median or raw, create a site mean
  data_means <- 
    (austraits %>% join_locations)$traits %>% 
    filter(trait_name %in% traits) %>%
    filter(value_type %in% c("mean", "raw", "median")) %>%
    mutate(replicates = 1) %>%
    group_by(taxon_name, trait_name, dataset_id, location_id) %>% 
    summarise(
      mean = mean(as.numeric(value)),
      median = median(as.numeric(value)),
      min = min(as.numeric(value)),
      max = max(as.numeric(value)),
      `latitude (deg)` = first(`latitude (deg)`),
      `longitude (deg)` = first(`longitude (deg)`),
      location_name = first(location_name),
      all_replicates = sum(replicates),
      location_replicates = 1
    ) %>%
    ungroup() %>%
    distinct(taxon_name, trait_name, dataset_id, location_id, mean, median, min, max, all_replicates, location_replicates, location_name, `latitude (deg)`, `longitude (deg)`)
  
  # any data that is a max or a min (range) and basically from a flora, create a mean value
  flora_means <-
    austraits$traits %>%
    filter(trait_name %in% traits) %>%
    filter(value_type %in% c("minimum", "maximum"), basis_of_record %in% c("preserved_specimen", "literature"))
  
  if (nrow(flora_means > 0)) {
    flora_means <- flora_means %>%
      mutate(replicates = 1) %>%
      group_by(taxon_name, trait_name, dataset_id, observation_id) %>% 
      summarise(
        mean = mean(as.numeric(value)),
        median = median(as.numeric(value)),
        min = min(as.numeric(value)),
        max = max(as.numeric(value)),
        all_replicates = sum(replicates),
        location_replicates = 0,
      ) %>%
      ungroup() %>%
      distinct(taxon_name, trait_name, dataset_id, mean, median, min, max, observation_id, all_replicates, location_replicates)
  }
  
  means <-
    if (nrow(flora_means > 0)) {
      bind_rows(
        data_means %>% mutate(across(any_of(c("mean", "median", "min", "max")), ~as.numeric(.x))),
        flora_means %>% mutate(across(any_of(c("mean", "median", "min", "max")), ~as.numeric(.x)))
      )
    } else {
      data_means
    }
  
  means <- means %>%
    group_by(taxon_name, trait_name) %>%
    summarise(
      mean = mean(mean),
      median = median(median),
      min = min(min),
      max = max(max),
      n = sum(all_replicates),
    ) %>%
    ungroup()
  
  means
}
