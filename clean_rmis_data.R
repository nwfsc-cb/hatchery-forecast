library(tidyverse)
library(lubridate)
# load data
d <- readRDS("data/joined_releases_recoveries.rds")

# process dates, remove missing recovery/brood years
indx <- which(nchar(d$recovery_date) < 6)
years <- d$recovery_date[indx]
d$recovery_date <- lubridate::parse_date_time(d$recovery_date, orders = "ymd")
d$recovery_year <- year(d$recovery_date)
d$recovery_year[indx] <- years
d <- dplyr::filter(d, !is.na(recovery_year), !is.na(brood_year))

# sum cwt counts
d$release_count <- 0
d$release_count[which(!is.na(d$cwt_1st_mark_count))] <- d$cwt_1st_mark_count[which(!is.na(d$cwt_1st_mark_count))]
d$release_count[which(!is.na(d$cwt_2nd_mark_count))] <- d$release_count[which(!is.na(d$cwt_2nd_mark_count))] + 
  d$cwt_2nd_mark_count[which(!is.na(d$cwt_2nd_mark_count))]

# Summarize releases by hatchery to be used as filtering. Only include
# - hatcheries that have 15 years of brood years for the last 30 years
# this leaves about 80 in all
yr_max <- max(d$brood_year,na.rm=T)
yr_min <- yr_max - 30 + 1
releases_by_hatchery <- dplyr::group_by(d, release_location_name) %>%
  dplyr::summarise(tot_release = sum(release_count),
                   n_yrs = length(which(unique(brood_year) %in% yr_min:yr_max))) %>%
  dplyr::filter(n_yrs >= 15) %>%
  dplyr::arrange(-tot_release)
d <- dplyr::filter(d, release_location_name %in% unique(releases_by_hatchery$release_location_name))

# summarize releases and returns by brood year
# estimated number is a rough expanded count correcting for sampling, effort, etc
# most estimated numbers very close to 1 (mean ~ 2, dist is skewed)
# Q: grouping could also be done spatially to look at recoveries in specific areas
grouped_dat <- dplyr::group_by(d, brood_year, release_location_name) %>%
  dplyr::summarise(n = n(),
                   estimated_number = sum(estimated_number, na.rm=T),
                   tot_release = sum(release_count)) 

# The majority of these time series are non-stationary -- this is driven by (1) 
# hatchery releases generally increasing to the late 1980s, then declining, and (2)
# broood years being differentially affected by changing environments. So instead
# od modeling raw counts, we first difference the releases and recoveries
grid <- expand.grid(brood_year = unique(grouped_dat$brood_year), 
                    release_location_name = unique(grouped_dat$release_location_name),
                    n = NA, estimated_number = NA)
missing <- which(paste(grid$brood_year,grid$release_location_name) %in% paste(grouped_dat$brood_year,grouped_dat$release_location_name) == FALSE)
grouped_dat <- rbind(grouped_dat, grid[missing,])

grouped_dat <- dplyr::group_by(grouped_dat, release_location_name) %>%
  dplyr::arrange(release_location_name, brood_year) %>%
  dplyr::mutate(ln_estimated_number = log(estimated_number),
                ln_tot_release = log(tot_release),
                diff_ln_number = c(NA,diff(ln_estimated_number)),
                diff_ln_release = c(NA,diff(ln_tot_release)))


summaries <- dplyr::group_by(d, release_location_name) %>%
  dplyr::summarise(release_location_rmis_basin = release_location_rmis_basin[1],
                   release_location_rmis_region = release_location_rmis_region[1],
                   stock_location_name = stock_location_name[1],
                   release_location_state = release_location_state[1],
                   max_releases = max(release_count, na.rm=T))
grouped_dat <- dplyr::left_join(grouped_dat,summaries)


saveRDS(grouped_dat,"cleaned_data_for_modeling.rds")







