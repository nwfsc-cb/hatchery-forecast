#example of modeling
library(glmmTMB)
library(dplyr)

dat <- readRDS("cleaned_data_for_modeling.rds")
# filter missing values and years with > 10 releases
dat <- dplyr::filter(dat,
                             is.finite(diff_ln_release),
                             is.finite(diff_ln_number),
                             !is.na(diff_ln_number),
                             !is.na(diff_ln_release),
                     brood_year %in% 1972:2020,
                     release_location_rmis_basin!="")

# initial model -- global intercept, slopes vary by hatchery
fit_1 <- glmmTMB(diff_ln_number ~ (-1+diff_ln_release)|release_location_name,
        data=dat)
dat$pred_all <- predict(fit_1)

# also could do this with future cross validation
dat$pred_cv <- NA
for(y in 2020:2011) {
  train <- dplyr::filter(dat, brood_year < y)
  fit <- glmmTMB(diff_ln_number ~ (-1+diff_ln_release)|release_location_name,
                   data=train)
  test <- which(dat$brood_year == y)
  dat$pred_cv[test] <- predict(fit, newdata = dat[test,])
}

# look at which hatcheries have good predictive skill
cv_performance <- dplyr::filter(dat, !is.na(pred_cv)) %>%
dplyr::group_by(release_location_name) %>%
  dplyr::summarise(rho = cor(pred_cv, diff_ln_number),
                   release_location_rmis_region = release_location_rmis_region[1],
                   release_location_rmis_basin = release_location_rmis_basin[1],
                   release_location_state = release_location_state[1]) %>%
  dplyr::arrange(-rho)




