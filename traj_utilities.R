### trajectory calculation utilities script
# integrate functions in other scripts with command: source(".../traj_utilities.R")
require(sp)
require(rworldmap)
require(tidyverse)
require(geosphere)


## function to allocate countrynames to trajectory point positions as percentage values
## returns a tibble with countrys and their respective percentage values
get_country_tibble <- function(tibble,
                               sample_id_row_name = sample_id,
                               traj_id_row_name = traj_id,
                               ens_id_row_name = ens_id,
                               lat_row_name = lat,
                               lon_row_name = lon,
                               group_by_row_name = sample_id,
                               country_label = ISO3){
  
  tibble <- enquo(tibble)
  sample_id_row_name <- enquo(sample_id_row_name)
  traj_id_row_name <- enquo(traj_id_row_name)
  ens_id_row_name <- enquo(ens_id_row_name)
  lat_row_name <- enquo(lat_row_name)
  lon_row_name <- enquo(lon_row_name)
  group_by_row_name <- enquo(group_by_row_name)
  
  country_label <- enquo(country_label)
  
  tibble_country <- rlang::eval_tidy(tibble)
  tibble_country <- tibble_country %>%
    select(!!sample_id_row_name, !!traj_id_row_name, !!ens_id_row_name, !!lat_row_name, !!lon_row_name)
  
  # The single argument to this function, points, is a data.frame in which:
  #   - column 1 contains the longitude in degrees
  #   - column 2 contains the latitude in degrees
  coords_country <- function(lon, lat){
    
    # resolution = 'high' much slower and not necessarily better
    countries_SP <- getMap(resolution = 'low')
    
    #setting CRS directly to that from rworldmap
    points_SP = SpatialPoints(as.data.frame(cbind(lon, lat)) , proj4string = sp::CRS(proj4string(countries_SP)))  
    
    # use 'over' to get indices of the Polygons object containing each point 
    indices = over(points_SP, countries_SP)

    # indices$ISO3 # returns the ISO3 code 
    # indices$continent   # returns the continent (6 continent model)
    # indices$REGION   # returns the continent (7 continent model)
    # indices$ABBREV # returns the abbrevation
    # indices$ADMIN # returns long name
    # indices$ADM0_A3 # returns short code (3 letters)
    
    return(as.vector(indices$ADMIN, mode = "character"))
  }
  
  tibble_country <- mutate(tibble_country, country = coords_country(lon = !!lon_row_name, lat = !!lat_row_name)) %>%
    mutate(country = case_when(is.na(country) ~ "NA",
                               TRUE ~ country))
  
  ## summation of directions
  for(i in unique(tibble_country$country)){
    tibble_country <- tibble_country %>% mutate(!!i := case_when(country == !!i ~ 1,
                                                                 TRUE ~ 0))
  }
  
  first_country <- toString(unique(tibble_country$country)[1])
  last_country <- unique(tibble_country$country)[length(unique(tibble_country$country))]
  
  tibble_country <- tibble_country %>%
    ## change to get different summations, e.g. sample_id, traj_id
    group_by(!!group_by_row_name) %>% 
    summarise_at(vars(!!first_country:!!last_country), ~sum(.)) %>%
    ungroup() %T>% 
    {start_zeile_summe_id <<- str_which(colnames(.), pattern = first_country)[1]} %>% {
      ## gesamt summe
      mutate(., sum = rowSums(.[start_zeile_summe_id:ncol(.)])) %>%
        ## prozent wert
        mutate_at(., vars(!!first_country:!!last_country), ~ (round((. / sum), digits = 3) * 100) ) %>%
        select(., -sum)
    }
  
  return(tibble_country)
}

## function to allocate directions to trajectory point positions measured according to the starting point as percentage values
## returns a tibble with defined directions and their respective percentage values
# example for directions vector: directions <- c("n", "ne", "e", "se", "s", "sw", "w", "nw")
# example for tibble manual direction: tibble_manual_direction <- as_tibble(list(direction = c("n", "e", "s", "w"), start = c(315, 45, 135, 225), end = c(44, 134, 224, 314)))
get_directions_tibble <- function(tibble,
                                  directions_vec,
                                  tibble_manual_direction,
                                  calculate_directions_deg = TRUE,
                                  start_deg = NULL,
                                  sample_id_row_name = sample_id,
                                  traj_id_row_name = traj_id,
                                  ens_id_row_name = ens_id,
                                  lat_row_name = lat,
                                  lon_row_name = lon,
                                  group_by_row_name = sample_id){
  
  tibble <- enquo(tibble)
  sample_id_row_name <- enquo(sample_id_row_name)
  traj_id_row_name <- enquo(traj_id_row_name)
  ens_id_row_name <- enquo(ens_id_row_name)
  lat_row_name <- enquo(lat_row_name)
  lon_row_name <- enquo(lon_row_name)
  group_by_row_name <- enquo(group_by_row_name)
  
  tibble_direction <- rlang::eval_tidy(tibble)

  
  if(calculate_directions_deg == TRUE){
    diff_start_end <- 0.00001
    if(is.null(start_deg)){
      directions_vec <- enquo(directions_vec)
      directions_vec <- rlang::eval_tidy(directions_vec)
      
      # assuming nord is first direction
      start_deg <- 360 * (1 - (1/length(directions_vec))/2)
    }
    
    tibble_assign_direction <- tibble(direction = directions_vec, 
                                      start = c(1:length(directions_vec)), end = c(1:length(directions_vec))) %>%
      mutate(start = map_dbl(start, ~ (!!start_deg + 360/length(!!directions_vec) * (. - 1))%%360)) %>%
      mutate(end = map_dbl(end, ~ (!!start_deg + 360/length(!!directions_vec) * (.))%%360 - diff_start_end))
    
  } else {
    tibble_manual_direction <- enquo(tibble_manual_direction)
    tibble_assign_direction <- rlang::eval_tidy(tibble_manual_direction)
  }
  
  first_direction <- tibble_assign_direction$direction[1]
  last_direction <- tibble_assign_direction$direction[nrow(tibble_assign_direction)]
  
  
  ## function for assigning direction to specific bearing angle
  fun_assign_direction <- function(vec_bearing){
    
    vec_direction <- na_if(vector(mode = "character", length = length(vec_bearing)), "")
    
    for(i in seq_len(nrow(tibble_assign_direction))){
      start <- tibble_assign_direction$start[i]
      end <-  tibble_assign_direction$end[i]
      direction <- tibble_assign_direction$direction[i]
      ## get decimal place from difference in direction table
      round_decimal <- formatC((start - end) %% 1, format = "e")
      round_decimal <- as.integer(str_sub(round_decimal, start = length(round_decimal) - 3))
      
      if(start > end){ 
        vec_direction <- ifelse(between(round(vec_bearing, digits = round_decimal), start, 360), direction, vec_direction)
        vec_direction <- ifelse(between(round(vec_bearing, digits = round_decimal), 0, end), direction, vec_direction)
      } else {
        vec_direction <- ifelse(between(round(vec_bearing, digits = round_decimal), start, end), direction, vec_direction)
      }
    }
    
    return(vec_direction)
  }
  
  lon_name_str <- str_remove(rlang::expr_text(lon_row_name), "~")
  lat_name_str <- str_remove(rlang::expr_text(lat_row_name), "~")
  
  tibble_direction <- tibble_direction %>% group_by(!!traj_id_row_name, !!ens_id_row_name) %>%
    mutate(bearing_row = pmap_dbl(list(.data[[lon_name_str]][1], .data[[lat_name_str]][1], !!lon_row_name, !!lat_row_name), 
                              ~ case_when((..1 == ..3) & (..2 == ..4) ~ NA_real_,
                                          TRUE ~ geosphere::bearing(c(..1, ..2), c(..3, ..4))))) %>%
    ## make negative bearing positive
    mutate(bearing_row = case_when(bearing_row < 0 ~ bearing_row + 360,
                               TRUE ~ bearing_row)) %>%
    mutate(direction = fun_assign_direction(bearing_row))
  
  ## summation of directions
  for(i in tibble_assign_direction$direction){
    tibble_direction <- tibble_direction %>% mutate(!!i := case_when(direction == !!i ~ 1,
                                                                     TRUE ~ 0))
  }
  
  tibble_direction <- tibble_direction %>% 
    ## change to get different summations, e.g. sample_id, traj_id
    group_by(!!group_by_row_name) %>% 
    summarise_at(vars(!!first_direction:!!last_direction), ~ sum(.)) %>%
    ungroup() %T>% 
    {start_zeile_summe_id <<- str_which(colnames(.), pattern = first_direction)[1]} %>% {
      ## gesamt summe
      mutate(., sum = rowSums(.[start_zeile_summe_id:ncol(.)])) %>%
        ## prozent wert
        mutate_at(., vars(!!first_direction:!!last_direction), ~ (round((. / sum), digits = 3) * 100) ) %>%
        select(., -sum)
    }
  
  return(tibble_direction)
}