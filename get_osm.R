###########################################################################
## PROJECT: Climate Change <> DA
## 
## SCRIPT PURPOSE: 
##    - Functions to download OSM data for the whole country
##    
## DATE: 10-04-17
## AUTHOR: Simon Kassel
###########################################################################



# HELPER FUNCTIONS --------------------------------------------------------

packages <- function(
  package_vector = c('sf', 'osmdata', 'plyr', 'USAboundaries', 'dplyr')) {
  # Check to see if packages are installed locally, if so, load them, 
  #   otherwise download from CRAN mirror before loading
  #
  # Args:
  #   package_vector: string vector, packages to load
  #
  # Returns:
  #   What the function returns
  #   
  
  for (lib in package_vector) {
    if (!requireNamespace(lib, quietly = TRUE))
      install.packages(lib, repos = "https://cran.rstudio.com/")
    suppressMessages(library(lib, character.only = TRUE))
  } 
}



kv_pairs_init <- function() {
  # Return sample key-value pairs
  #
  # Args:
  #   NA
  #
  # Returns:
  #   a list of ('key', 'value') vectors, input format for 
  #     `get_all_kv_pairs()`
  #   
  
  list(
    c('amenity', 'clinic'),
    c('amenity', 'hospital'),
    c('healthcare', 'clinic'),
    c('healthcare', 'centre'),
    c('healthcare', 'hospice'),
    c('healthcare', 'hosptial'),
    c('healthcare', 'blood_bank'),
    c('healthcare', 'blood_donation')
  ) %>%
    return
}



select_vars <- function(df) {
  # Select appropriate variables from overpass output
  #
  # Args:
  #   df: an object of class `sf`
  #
  # Returns:
  #   An object of class `sf` in a standardized format 
  #     so that it can be iteratively bound to other sf objects
  #   
  
  # libraries
  packages()
  
  # check if there are any features in df
  if (nrow(df) == 0) {
    return(NULL)
  } else {
    
    # specify whether this was originally a POINT or POLYGON
    df['geom_type_original'] <- df %>%
      st_geometry_type %>%
      first
    
    # variables to keep
    vars <- c('osm_id', 'amenity', 'healthcare', 'name', 
              'geom_type_original', 'addr.city', 
              'addr.housenumber', 'addr.postcode', 
              'addr.state', 'addr.unit', 'addr.country', 
              'geometry')
    
    # add variables in case they're missing
    for (v in vars) {
      if (!v %in% names(df)) {
        df[v] <- NA
      }
    }
    
    # keep variables
    df %>% 
      select(one_of(vars)) %>%
      return
  }
}



# CALLABLE FUNCTIONS ------------------------------------------------------


get_state <- function(state, key, value) {
  # Get OSM points for one key-value pair of one state
  #
  # Args:
  #   state: string, name of the state to search
  #   key: string, OSM key value
  #   value: string, OSM value
  #
  # Returns:
  #   An sf object with all points returned from the api call and the 
  #     centroids of all polygons from the same call
  #   
  
  # libraries
  packages()
  
  # query api, returning list of all matching spatial objects
  osm_dat <- USAboundaries::us_states() %>%
    dplyr::mutate(name = tolower(name)) %>% 
    dplyr::filter(name == tolower(state)) %>%
    sf::st_as_sf() %>%
    st_bbox %>%
    opq %>% 
    add_osm_feature(key = key, 
                    value = value, 
                    value_exact = FALSE,
                    match_case = FALSE) %>%
    osmdata_sf 
  
  # extract points and polygons, selecting appropriate variables
  # and finding polygon centroids
  osm_pts <- select_vars(osm_dat$osm_points)
  osm_ply <- select_vars(osm_dat$osm_polygons) 
    
  # ensure that the query didn't return an empty polygon set
  if (!is.null(osm_ply)) {
    osm_ctr <- st_centroid(osm_ply)
    osm <- rbind(osm_pts, osm_ctr)
  } else {
    osm <- osm_pts
  }
  
  # add search state, key, and value variables
  if (!is.null(osm)) {
    osm <- osm %>%
      mutate(search_state = state, search_key = key, 
             search_value = value)
  }
  return(osm)
}



get_all_states <- function(kv_pair) {
  # Find all matches of an OSM key-value pair accross the whole US;
  #   apply `get_state()` over all 50 states
  #
  # Args:
  #   kv_pair: string vector of length two, containing the key and value
  #     to query - eg.: 
  #       `c('healthcare', 'hospital')`
  #
  # Returns:
  #   An sf object with all points returned from the api call and the 
  #     centroids of all polygons from the same call, for all 50 states
  #   
  
  # libraries
  packages()
  
  # get key-value pair
  key <- unlist(kv_pair)[1]
  value <- unlist(kv_pair)[2]
  
  # update progress
  print("Searching...")
  print(paste0("Key: ", key))
  print(paste0("Value: ", value))
  
  # apply get_state function over each state
  points <- ldply(USAboundaries::us_states()$name, 
                  get_state, 
                  key = key, 
                  value = value, 
                  .progress = 'text') %>%
    
    # remove duplicates resulting from overlapping bouding boxes
    distinct(osm_id, .keep_all = TRUE)
  
  # print the number of points for this kv pair
  print(paste0("Found ", nrow(points)," points from kv pair [",
               key,":",value,"]."))
  
  return(points)
} 



get_all_kv_pairs <- function(kv_pairs, fpath = NULL) {
  # Find nationwide dataset of a series of key value pairs; apply 
  #   `get_all_states()` over a list of kv pairs
  #
  # Args:
  #   kv_pairs: list of kv-pair vectors (defined in `get_all_states()`),
  #     to query - eg.: 
  #       `list( c('healthcare', 'hospital'), c('amenity', 'clinic') )`
  #   fpath: filepath string, if non-NULL writes the resulting df to as a csv,
  #     default='NULL`
  #      
  #
  # Returns:
  #   A nationwide sf object with all points returned for all 
  #     key-value pairs 
  #   
  
  # libraries
  packages()
  
  # apply function over kv-pairs
  all_dat <- ldply(kv_pairs, 
                   get_all_states, 
                   .progress = 'text') %>%
    
    # remove duplicates resulting from overlapping bouding boxes
    distinct(osm_id, .keep_all = TRUE)
  
  
  # write.csv
  if (!is.null(fpath)) {
    write.csv(all_dat, fpath)
  }
  
  if (interactive()) {
    return(all_dat)
  }
}



csv_to_list <- function(path, keys = 'key', values = 'value', query = FALSE) {
  # Convert a csv to correct list format and optionally run OSM query for
  #   those kv pairs
  #
  # Args:
  #   path: string, file path of a csv with each row representing a 
  #     key: value pair and includes one column each for key and value
  #     designations
  #   keys: string, name of key field in csv (defaults to 'key')
  #   values: string, name of value field in csv (defaults to 'value')
  #   query: boolean, do you want to go ahead and get all osm features 
  #     for the whole country and the kv-pairs included `TRUE`, or just
  #     return the list object and run the query in a separate command
  #     `FALSE` (defaults to `FALSE`)
  #
  # Returns:
  #   if `query == FALSE`:
  #     a list of kv pairs, formatted for `get_all_kv_pairs()`
  #   if `query == TRUE`:
  #     a data frame of OSM features for the kv-pairs in the csv (output
  #     from `get_all_kv_pairs()`), and writes that df to a csv in the 
  #     same location as input csv and the same filename with 
  #     '_OSM_OUTPUT.csv' appended.
  #   
  
  # libraries
  packages()
  
  # read csv and convert to correct list format
  kvp_list <- read.csv(path, stringsAsFactors = FALSE) %>%
    select(one_of(c(keys, values))) %>%
    as.matrix %>%
    alply(1, function(x) {
      return(unname(c(x[1], x[2])))
    }) %>%
    unname
  
  # should this should be queried
  if (query) {
    # create a filepath in same location as csv, query and return
    out_path <- gsub('.csv', '_OSM_OUTPUT.csv', path)
    get_all_kv_pairs(kvp_list, out_path) %>%
      return
  } else {
    
    # otherwise just returns the list
    return(kvp_list)
  }
}


# RUN FROM COMMAND LINE ---------------------------------------------------


check_args <- function(args) {
  # Check for appropriate number of command line argments
  #

  # test if there is at least one argument: if not, return an error
  if (length(args) < 3) {
    stop("[ error ] You must supply 3 arguments: 'path', 'key', 'value' ", call.=FALSE)
  } else if (length(args) > 3) {
    # default output file
    print("More than three arguments supplied, dropping any extra")
  }
  print("Searching OSM for key:value pairs supplied")
  print(paste0("Writing to directory of '", args[1], "'"))
}



main <- function() {
  # Description of the function
  #
  # Args:
  #   x: an object of type _____, more details about it
  #
  # Returns:
  #   What the function returns
  #   
  
  args <- commandArgs(trailingOnly=TRUE)
  check_args(args)
  csv_to_list(args[1], args[2], args[3], query = TRUE)
}



if (!interactive()) {
  main()
}

