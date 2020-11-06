
### FastGPS project with SDRP comparing dolphin Fastloc to boat-based GPS
### Use crawl to interpolate the boat tracks and estimate locations every second. 
### October 2020
###############################################################################

library(tidyverse)
library(janitor)
library(devtools)
#devtools::install_github('NMML/crawl@devel')
library(crawl)


# Load & clean data
# ----------------------------------
load('Data/FastGPS_data_updated.RData')
tracks <- FastGPS_data[[2]] %>% as.data.frame() # 2nd list is the boat tracks
tracks <- tracks %>% mutate(id = as.numeric(as.factor(Date))) #assign track id based on date

# there are 2 tracks on 6 June. Need to separate their ids.
six <- which(tracks$id==2)
head(six);tail(six)
tracks[11409:16809,6] <- 8 # change the other track to a different id number

# add in error for the locations
tracks <- tracks %>% dplyr::mutate(error_semi_major_axis = 50,
                                   error_semi_minor_axis = 50,
                                   error_ellipse_orientation = 0)

#convert from geographic coordinate system and project data
sf_locs <- sf::st_as_sf(tracks,coords = c('lon','lat')) %>% 
  sf::st_set_crs(4326) %>% 
  sf::st_transform(26917)


## Interpolate full boat path using crawl
## -------------------------------------------------------------------
library(purrr)
library(furrr)

future::plan(multisession)

sf_locs <- sf_locs %>% 
  dplyr::group_by(id) %>% dplyr::arrange(datetime) %>% 
  tidyr::nest() %>% 
  dplyr::mutate(data = furrr::future_map(data,sf::st_as_sf))

# create ellipse error columns
sf_locs <- sf_locs %>% 
  dplyr::mutate(
    diag = purrr::map(data, ~ crawl::argosDiag2Cov(
      .x$error_semi_major_axis, 
      .x$error_semi_minor_axis, 
      .x$error_ellipse_orientation)),
    data = purrr::map2(data,diag,dplyr::bind_cols)) %>% 
  dplyr::select(-diag)

# set parameters
fixPar <- c(1,1,NA,NA)
constr = list(lower=c(rep(log(1500),3), rep(-Inf,2)), upper=rep(Inf,5))

# function to fit crawl model
fit_func <- function(data, fixPar) {
  suppressWarnings(
    crawl::crwMLE(
      mov.model =  ~ 1,
      err.model = list(
        x =  ~ ln.sd.x - 1,
        y =  ~ ln.sd.y - 1,
        rho =  ~ error.corr),
      fixPar = fixPar,
      data = data,
      Time.name = "datetime",
      
      attempts = 40,
      control = list(maxit=2000, trace=0, REPORT=1),
      initialSANN = list(maxit=200, trace=0, REPORT=1)
    )  
  )
}

# fit the model
sf_locs <- sf_locs %>% 
  dplyr::mutate(fit = purrr::map(data,~fit_func(data = .x,fixPar = fixPar)))

sf_locs$fit #look at parameter output

## Predict locations every second
sf_locs <- sf_locs %>% 
  dplyr::filter(map_lgl(fit, ~inherits(.x,"crwFit"))) %>% 
  dplyr::mutate(pred_pts = purrr::map(fit, 
                                      ~ crawl::crwPredict(.x, predTime = "1 sec")))

# convert predicted points to sf tracks
sf_locs <- sf_locs %>% 
  dplyr::mutate(
    pts_sf = purrr::map(pred_pts, ~ crawl::crw_as_sf(.x, ftype = "POINT",
                                                     locType ="p"))
  )
sf_locs

# get predicted points out of Tibble
sf_pred_pts <- sf_locs %>% tidyr::unnest(pts_sf) %>% 
  select(-data,-fit,-pred_pts)

pred_pts <- sf::st_coordinates(sf_pred_pts$geometry)
sf_pred_pts <- as.data.frame(sf_pred_pts)

predPts <- cbind(sf_pred_pts,pred_pts)
long <- coordinates(predPts$geometry)[,1]
lat <- coordinates(predPts$geometry)[,2]

write.csv(predPts,'predictedBoatTracks.csv',row.names=FALSE)
