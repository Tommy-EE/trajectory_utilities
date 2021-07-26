# integrate functions in other scripts with command: source(".../traj_utilities.R")
source("traj_utilities.R")

traj_test_tibble <- read_csv("example/example_traj.csv")

directions <- c("n", "ne", "e", "se", "s", "sw", "w", "nw")

get_directions_tibble(tibble = traj_test_tibble,
                      directions_vec = directions,
                      sample_id_row_name = sample_id,
                      traj_id_row_name = traj_id,
                      ens_id_row_name = ens_id,
                      lat_row_name = lat,
                      lon_row_name = lon,
                      group_by_row_name = sample_id)

## for country:
# indices$ISO3 # returns the ISO3 code 
# indices$continent   # returns the continent (6 continent model)
# indices$REGION   # returns the continent (7 continent model)
# indices$ABBREV # returns the abbrevation
# indices$ADMIN # returns long name
# indices$ADM0_A3 # returns short code (3 letters)


get_country_tibble(tibble = traj_test_tibble,
                   sample_id_row_name = sample_id,
                   traj_id_row_name = traj_id,
                   ens_id_row_name = ens_id,
                   lat_row_name = lat,
                   lon_row_name = lon,
                   group_by_row_name = sample_id)
