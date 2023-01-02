###########################
## Model annual detectability of Mariana Fruit Dove using wg's eBird data
## 
## This script is meant to be sourced from main_MAFD.R
## 
## author: Willson Gaul willson.gaul@gmail.com
## created: 1 Jan. 2023
## last modified: 3 Jan 2023
###########################
library(mgcv)

# null model
mafd_detect_gam_H0 <- gam(Ptilinopus_roseicapilla ~ 1, 
                            family = ziP(), 
                            data = data.frame(wg_ebird_wide))
gam.check(mafd_detect_gam_H0)

# Day of year smoother with no location random effect
mafd_detect_gam_m1 <- gam(Ptilinopus_roseicapilla ~ 1 + 
                          s(day_of_year, k = 12, bs = "cc"), 
                        data = wg_ebird_wide, method = "REML", 
                        family = ziP(), 
                        knots = list(day_of_year=c(1, 365)))
gam.check(mafd_detect_gam_m1)
plot(mafd_detect_gam_m1)

# Group level smoother with random effect letting intercept vary by location (G)
mafd_detect_gamG <- gam(Ptilinopus_roseicapilla ~ 1 + 
                          s(day_of_year, k = 12, bs = "cc") + 
                          s(Location_ID, k = 75, bs = "re"), 
                        data = wg_ebird_wide, method = "REML", 
                        family = ziP(), 
                        knots = list(day_of_year=c(1, 365)))
gam.check(mafd_detect_gamG)
plot(mafd_detect_gamG)
wg_ebird_wide$pred_gamG <- predict(mafd_detect_gamG, type = "response")
ggplot(data = wg_ebird_wide, aes(x = pred_gamG, y = Ptilinopus_roseicapilla)) + 
  geom_point() + 
  facet_wrap(~Location_ID) + 
  ggtitle("Predicted v. observed")

ggplot(data = wg_ebird_wide, aes(x = day_of_year, y = pred_gamG)) + 
  geom_point() + 
  facet_wrap(~Location_ID) + 
  ggtitle("Predicted v. day of year")


# Full model (G plus all other potentially relevant covariates)
mafd_detect_gamFull <- gam(Ptilinopus_roseicapilla ~ 1 + 
                             s(day_of_year, k = 12, bs = "cc") + 
                             s(Location_ID, k = 75, bs = "re") + 
                             s(tod_sec, k = 10, bs = "cc") + 
                             Protocol + Duration_Min + 
                             Distance_Traveled_km + 
                             s(Number_of_Observers, k = 8, bs = "tp"), 
                           data = wg_ebird_wide, method = "REML", 
                           family = ziP(), 
                           knots = list(day_of_year=c(1, 365)))
gam.check(mafd_detect_gamFull)
plot(mafd_detect_gamFull)

# No day of year, but all other potentially relevant covariates)
mafd_detect_gamNoDOY <- gam(Ptilinopus_roseicapilla ~ 1 + 
                              s(Location_ID, k = 75, bs = "re") + 
                              s(tod_sec, k = 10, bs = "cc") + 
                              Protocol + Duration_Min + 
                              Distance_Traveled_km + 
                              s(Number_of_Observers, k = 8, bs = "tp"), 
                            data = wg_ebird_wide, method = "REML", 
                            family = ziP(), 
                            knots = list(day_of_year=c(1, 365)))
gam.check(mafd_detect_gamNoDOY)
plot(mafd_detect_gamNoDOY)


### model comparison
AIC(mafd_detect_gam_H0, mafd_detect_gam_m1, mafd_detect_gamG, 
    mafd_detect_gamFull, mafd_detect_gamNoDOY)

summary(mafd_detect_gamG)
summary(mafd_detect_gamFull)
summary(mafd_detect_gamNoDOY)

ggplot(data = wg_ebird_wide, aes(x = day_of_year, y = Distance_Traveled_km, 
                                 color = Protocol)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = wg_ebird_wide, aes(x = day_of_year, y = Duration_Min, 
                                 color = Protocol)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = wg_ebird_wide, aes(x = day_of_year, y = Time, 
                                 color = Protocol)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = wg_ebird_wide, aes(x = day_of_year, y = Number_of_Observers, 
                                 color = Protocol)) + 
  geom_point() + 
  geom_smooth()

# global smoother plus group-level smoothers that have the same wiggliness (GS)
mafd_detect_gamGS <- gam(Ptilinopus_roseicapilla ~ 1 + 
                           s(month, k = 10, bs = "cc") + 
                           s(month, Location_ID, k = 10, 
                             bs = "fs", m = 2), 
                         data = wg_ebird_wide, method = "REML", 
                         family = ziP(), 
                         knots = list(day_of_year=c(0, 12)))
gam.check(mafd_detect_gamGS)
plot(mafd_detect_gamGS)
wg_ebird_wide$pred_gamGS <- predict(mafd_detect_gamGS, type = "response")
