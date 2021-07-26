# trajectory_utilities
Assign country names and directions to trajectory point positions as percentage values


## function to assign country names to trajectory point positions as percentage values
## returns a tibble with countrys and their respective percentage values

get_country_tibble(tibble = traj_test_tibble,
                   sample_id_row_name = sample_id,
                   traj_id_row_name = traj_id,
                   ens_id_row_name = ens_id,
                   lat_row_name = lat,
                   lon_row_name = lon,
                   group_by_row_name = sample_id)
       
       
## function to assign directions to trajectory point positions measured according to the starting point as percentage values
## returns a tibble with defined directions and their respective percentage values
# example for directions vector: directions <- c("n", "ne", "e", "se", "s", "sw", "w", "nw")
# example for tibble manual direction: tibble_manual_direction <- as_tibble(list(direction = c("n", "e", "s", "w"), start = c(315, 45, 135, 225), end = c(44, 134, 224, 314)))

directions <- c("n", "ne", "e", "se", "s", "sw", "w", "nw")

get_directions_tibble(tibble = traj_test_tibble,
                      directions_vec = directions,
                      sample_id_row_name = sample_id,
                      traj_id_row_name = traj_id,
                      ens_id_row_name = ens_id,
                      lat_row_name = lat,
                      lon_row_name = lon,
                      group_by_row_name = sample_id)
