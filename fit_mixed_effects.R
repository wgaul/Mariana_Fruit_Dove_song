#####################
## Fit mixed effects models for Mariana Fruit Dove songs
## 
## author: Willson Gaul  willson.gaul@gmail.com
## created: 12 July 2022
## last modified: 12 July 2022
######################

margo_dove$island <- factor(as.character(margo_dove$island), 
                            levels = c("Saipan", "Rota", "Aguiguan", 
                                       "Guam"), 
                            labels = c("Saipan", "Rota", "Aguiguan", 
                                       "Guam"), 
                            ordered = FALSE)

ggplot(data = margo_dove, aes(x = Freq_peak_S1)) + 
  geom_histogram() + 
  facet_wrap(~island, ncol = 1)


# fit linear model
freq_lmod <- glm(Freq_peak_S1 ~ island, data = margo_dove, family = gaussian)
plot(freq_lmod)
summary(freq_lmod)

# fit mixed effects model
freq_mmod <- lmer(Freq_peak_S1 ~ island + (1 | filename), data = margo_dove)
summary(freq_mmod)
confint(freq_mmod)
