# This code is to determine if there is a way to decrease the sampling effort
# of the Bloody Hill bladderpod sampling SOP.

# option to load HTLNbladderpod from github:
# install.packages("pak")
# pak::pak("kinziebailey/HTLNbladderpod")


# Libraries ----
library(HTLNbladderpod)
library(DBI)
library(odbc)
library(tidyverse)
library(car)
# library(ordinal)
library(brms)
library(bayesplot)
library(tidybayes)
library(loo)
library(lme4)

# library(MuMIn)

# getting bloodyhill data ----
importData(instance = 'local',
           path = "C:/Users/kbailey/Documents/HTLN/HTLN Database/",
           name = "MoBlad_BloodyHill_v3.66.accdb",  # "MoBlad_Glades_v1.8.accdb"
           new_env = TRUE)

# merging data
bh_counts <- VIEWS_HTLN_BLDP$tbl_5m_MoBladCount
bh_period <- VIEWS_HTLN_BLDP$tlu_PeriodID
bh_core <- VIEWS_HTLN_BLDP$tlu_5m_Grid_Core
density_class <- VIEWS_HTLN_BLDP$tlu_DensityClasses
bh_accuracy <- VIEWS_HTLN_BLDP$tbl_5m_Accuracy
bh_cedar1 <- VIEWS_HTLN_BLDP$tbl_5m_Cedar

# stem density/basal area per unit area
bh_cedar <- bh_cedar1 |>
  mutate(basal_tree = pi * (dbh/2)^2) |> # cm2
  summarise(count = n(),
            ba_total = sum(basal_tree, na.rm = TRUE),
            .by = c("Period_ID",
                    "Location_ID")) |>
  mutate(ba_m2 = ba_total / 25,
         stemd_m2 = count / 25)


bh_data1 <- bh_counts |>
  dplyr::left_join(bh_period,
                   by = "Period_ID") |>
  dplyr::left_join(bh_core) |>
  dplyr::left_join(bh_accuracy,
                   by = c("Period_ID",
                          "Location_ID")) |>
  dplyr::left_join(density_class,
                   by = "DensityClass") |>
  dplyr::left_join(bh_cedar,
                   by = c("Location_ID",
                          "Period_ID"))

# adding core/non-core column
bh_data <- bh_data1 |>
  # core vs non-core
  mutate(type = if_else(is.na(Core_ID), "non-core", "core")) |>
  # converting classes
  mutate(Date = as.Date(StartDate, format = "%m/%d/%Y"),
         year = year(Date),
         DensityClass_f = factor(DensityClass,
                                 levels = c("0",
                                            "1",
                                            "2",
                                            "3",
                                            "4",
                                            "5",
                                            "6"),
                                 ordered = TRUE),
         min_year = min(year),
         year_cat = year - min_year,
         Location_IDf = factor(Location_ID)) |>
  select(Location_ID,
         Location_IDf,
         DensityClass,
         DensityClass_f,
         ParkCode,
         Date,
         year_cat,
         year,
         CalYr,
         AreaSampled,
         type,
         ba_m2,
         stemd_m2,
         PeriodDescriptor,
         Count,
         DensityClass_Est,
         DensityClass_Actual,
         LowerBound,
         UpperBound,
         Midpoint,
         LowerBound50,
         UpperBound50)

bh_dc_count <- bh_data |>
  dplyr::count(DensityClass,
               year)

bh_dc_count_core <- bh_data |>
  dplyr::filter(type == "core") |>
  dplyr::count(DensityClass,
               year)

bh_dc_avg <- bh_data |>
  dplyr::summarise(mean_dc = mean(DensityClass),
                   .by = year)
bh_dc_avg_type <- bh_data |>
  dplyr::summarise(mean_dc = mean(DensityClass),
                   .by = c(year,
                           type))

# Can we drop non-core sampling years ----
ggplot() +
  geom_line(data = bh_dc_count,
            aes(year,
                n,
                color = factor(DensityClass)))

ggplot() +
  geom_line(data = bh_dc_count_core,
            aes(year,
                n,
                color = factor(DensityClass)))

ggplot() +
  geom_line(data = bh_dc_avg,
            aes(year,
                mean_dc))

ggplot() +
  geom_line(data = bh_dc_avg_type,
            aes(year,
                mean_dc,
                color = type))

# Data exploration ----

# density classes hist
ggplot() +
  geom_bar(data = bh_data,
               aes(DensityClass_f))
# right skewed, lots of "zeros"

bh_data_core <- bh_data |>
  filter(type == "core")

ggplot() +
  geom_bar(data = bh_data_core,
           aes(DensityClass_f)) +
  facet_wrap(~year)

# number of plots in density classes per year
bh_year <- bh_data |>
  count(year,
        DensityClass_f,
        name = "count")

ggplot() +
  geom_boxplot(data = bh_year,
               aes(x = DensityClass_f,
                   y = count,
                   fill = DensityClass_f))

# number of plots in density classes per year for core vs non-core
bh_year_type <- bh_data |>
  count(year,
        DensityClass_f,
        type,
        name = "count")

ggplot() +
  geom_boxplot(data = bh_year_type,
               aes(x = DensityClass_f,
                   y = count,
                   fill = DensityClass_f)) +
  facet_wrap(~type)

# Density plots of Accuracy Counts
ggplot() +
  geom_density(data = bh_data,
               aes(x = Count)) +
  facet_wrap(~DensityClass_Actual,
             scales = "free")

# density plot accuracy 2025
data_2026 <- bh_data |>
  filter(year == "2022")

ggplot() +
  geom_density(data = data_2026,
               aes(x = Count)) +
  facet_wrap(~DensityClass_Actual,
             scales = "free")

# How do accuracy counts compare to estimated density class
ggplot() +
  geom_histogram(data = bh_data,
                 aes(x = DensityClass_Est)) +
  geom_vline(data = bh_data,
             aes(xintercept = DensityClass_Actual)) +
  facet_wrap(~DensityClass_Actual,
             scales = "free")

# Creating models ----
## BRMS ----

# potentially need spline (probably has year to year variability), but they are
# only interested in trends...not year to year var...yet
model_brms <- brm(formula = DensityClass_f ~ year_cat + (1|Location_IDf),
                  data    = bh_data,
                  family  = cumulative(link = "logit"),   # or "probit"
                  chains  = 4,
                  cores = 4,
                  iter = 1000)

# saving model fit (took 30min to run)
saveRDS(model_brms,
        "model_brms.rds")

# Loading model fit
# readRDS("model_brms.rds")

## Posterior Analysis ----

summary(model_brms) # Rhat = 1.01 okay

# CONVERGENCE DIAGNOSTICS (good)

# Trace plots
plot(model_brms)

# effective sample size
summary(neff_ratio(model_brms))

# Divergent transitions
nuts_params(model_brms)

# all samples with divergent transitions
model_brms |>
  tidybayes::tidy_draws() |>
  dplyr::select(ends_with("__")) |>
  filter(divergent__ == TRUE) # no divergent transitions

# CONDITIONAL EFFECTS PLOTS
conditional_effects(model_brms,
                    categorical = TRUE)

# POSTERIOR DISTRIBUTIONS
mcmc_combo(model_brms)

# POSTERIOR PREDICTIVE CHECKS (over-dispersion)
pp_check(model_brms,
         type = "bars",
         ndraws = 100)

# TRANSITIONS (between thresholds for ordered data)
post_draws <- as_draws_df(model_brms)

thresholds <- post_draws[, grepl("^b_Intercept",
                                 colnames(post_draws))]

# posterior probability that order of thresholds is as follows:
# theta0 < theta1 < theta2 < ...
mean(apply(thresholds, 1,
           function(x) all(diff(x) > 0))) # 1 = excellent

spacing <- apply(thresholds, 1, diff)
colnames(spacing) <- paste0("spacing_",
                            seq_len(ncol(spacing)))



# plotting threshold posteriors (looking for overlap or drift)
mcmc_areas(thresholds)





loo(model_brms)







# POSTERIOR PREDICTIVE DISTRIBUTIONS: THIS NEEDS WORK!!!!!!!!!!!!!
new_data <- expand.grid(year_cat = seq(min(bh_data$year_cat,
                                           na.rm = T),
                                       max(bh_data$year_cat,
                                           na.rm = T),
                                       length.out = 50))

pred_samples <- posterior_predict(model_brms,
                                  newdata = new_data,
                                  re_formula = NA)
prob <- fitted(model_brms, newdata = new_data, re_formula = NA)

# Plot with uncertainty
pred_df <- new_data %>%
  mutate(mean = apply(pred_samples, 2, mean),
         lower = apply(pred_samples, 2, quantile, 0.025),
         upper = apply(pred_samples, 2, quantile, 0.975))

ggplot(pred_df,
       aes(x = year_cat)) +
  geom_ribbon(aes(ymin = lower,
                  ymax = upper),
              alpha = 0.3) +
  geom_line(aes(y = mean),
            size = 1) +
  geom_point(data = bh_data,
             aes(y = DensityClass_f))

# Random effects plot
model_brms |>
  spread_draws(r_Location_IDf[Location_IDf,]) |>
  ggplot(aes(x = r_Location_IDf,
             y = Location_IDf)) +
  stat_halfeye()



# model_brms1 <- brm(formula = DensityClass_f ~ s(year_cat, k = 3) + ba_m2 + (1|Location_IDf),
#                    data    = bh_data,
#                    family  = cumulative(link = "logit"),   # or "probit"
#                    chains  = 3,
#                    cores = 4,
#                    iter = 500)
#
# summary(model_brms1)

# subset core only and core+noncore (full dataset)
set.seed(123) # for reproducibility

remove_frac <- 0.5

bh_halfnoncore <- rbind(bh_data |>
                          filter(type == "core"),
                        bh_data |>
                          filter(type == "non-core") |>
                          slice_sample(prop = 1 - remove_frac))

model_brms_halfnoncore <- brm(formula = DensityClass_f ~ year_cat + ba_m2 + (1|Location_IDf),
                       data    = bh_halfnoncore,
                       family  = cumulative(link = "logit"),   # or "probit"
                       chains  = 4,
                       cores = 4,
                       iter = 1000)

summary(model_brms_halfnoncore)

# Comparing models
library(nlme)

BIC(model_brms,
    model_brms_core)


loo1 <- loo(model_brms)
loo2 <- loo(model_brms_core)

loo_compare(loo1,
            loo2)




## CLMM stem density vs basal area

mod_basal <- clmm(DensityClass_f ~ year_cat + ba_m2 + type + (1|Location_IDf),
                  data = bh_data)

summary(mod_basal)

mod_stem <- clmm(DensityClass_f ~ year_cat + stemd_m2 + type + (1|Location_IDf),
                 data = bh_data)

summary(mod_stem)

AIC(mod_basal,
    mod_stem)








# fitting cumulative link mixed model ----
# A cumulative link mixed model (CLMM) will estimate:
# Whether plot group (core vs. 5‑year) shifts the probability of being in higher density classes
# Whether year affects density class
# Whether ceder basal area per are will affect density class
# Whether trends differ between plot groups
# How much plot‑to‑plot variation exists

# mod_null <- clmm(DensityClass_f ~ 1 + (1|Location_ID) + (1|CalYr),
#                  data = bh_data)

mod_full <- clmm(DensityClass_f ~ year_cat + ba_m2 + type + (1|Location_IDf),
                 data = bh_data)

mod_norm <- clm(DensityClass_f ~ year_cat + ba_m2 + type,
                 data = bh_data)

anova(mod_full,
      mod_norm)

summary(mod_full)
summary(mod_norm)

# model is pretty good,convergedcleanly and shows strong interpretable effects of
# year, basal area, and type. Random effects indicate large spatial heterogeneity
# and minimal year-to-year variation. threshold structure is non-equidistant
# meaning classes are unevenly spaced.
# logLik = -2402.10 AIC = 4826.21
# max.grad = 3.5e-4 (convergance good)
# cond.H = 1.6e4 high but okay

# Interpritation:
#   year_cat: each increase in year increases the odds of being in a higher
#             density class (0.0297, p = 0.03)
#   ba_m2: higher basal area is associated with lower density classes (-0.0728,
#                                                                      p <.000001)
#   type = non-core: Non-core have lower odds of being in higher density classes
#                     (-5.091,p<2e-16) *This is because the non-core are at the edge.

#   Location: density class varies substantially across locations V = 6.6, Sd = 2.5
#   Year: Very little variation between years.

# ggeffects does ignore the random effects, thus predictions are marginal not
# conditional on group. (only population level estimates)
library(ggeffects)

plot(ggpredict(mod_full,
               terms = "ba_m2",
               type = "random"))

ggemmeans(mod_full)

library(emmeans)

emeans <- emmeans(mod_full,
                  ~ year_cat + ba_m2 + type,
                  mode = "linear.predictor")


# Convert to probabilities
probabilities <- rating.emmeans(emeans, type = "prob")
cumprobs <- rating.emmeans(emeans, type = "cumprob")
class1 <- rating.emmeans(emeans, type = "class1")



## GLMMTMB ----
library(glmmTMB)
library(MASS)

fit_ord <- glmmTMB(DensityClass_f ~ year + ba_m2 + (1|type) + (1|Location_IDf),
                   data = bh_data,
                   family = ordinal)












# Use covstruct = list(plot = "ar1") in glmmTMB (depending on version) or a similar structure.

## GLMMTMB part two ----
# Binary model for zero vs non-zero
dat$nonzero <- as.integer(response > 0)

fit_zero <- glmmTMB(
  nonzero ~ year + core_status + basal_area + (1 | plot),
  data   = dat,
  family = binomial(link = "logit")
)
# Ordinal model for nonzero levels only
dat_nonzero <- subset(dat, response > 0)

fit_ord_nonzero <- glmmTMB(
  response ~ year + core_status + basal_area + (1 | plot),
  data   = dat_nonzero,
  family = ordinal(link = "logit")
)

## BRMS ----
library(brms)

fit_brms <- brm(
  response ~ year + core_status + basal_area +
    (1 | plot),
  data   = dat,
  family = cumulative(link = "logit"),
  chains = 4, cores = 4
)


# # Checking threshold/proportional-odds diagnostics
# mod_flex <- clmm(DensityClass_f ~ year_cat + ba_m2 + type + (1|Location_IDf) + (1|CalYr),
#                  data = bh_data,
#                  threshold = "flexible")
#
# mod_equ <- clmm(DensityClass_f ~ year_cat + ba_m2 + type + (1|Location_IDf) + (1|CalYr),
#                  data = bh_data,
#                  threshold = "equidistant")
#
# AIC(mod_flex,
#     mod_equ)
#
# summary(mod_flex)

plot(fitted(mod_full))

mod_full$alpha
mod_full$beta
mod_full$edf
mod_full$y.levels
mod_full$xlevels
mod_full$terms
mod_full$na.action
mod_full$contrasts
mod_full$fitted.values
mod_full$tJac
mod_full$control
mod_full$gfList
mod_full$formula
mod_full$call
mod_full$threshold
mod_full$start
mod_full$link
mod_full$nAGQ
mod_full$Hessian
mod_full$gradient
mod_full$condVar
mod_full$ranef
mod_full$Zt
mod_full$L
mod_full$optRes
mod_full$u
mod_full$dims
mod_full$logLik
mod_full$ST
mod_full$coefficients



# Fixed effects
beta <- mod_full$beta

# thresholds
theta <- mod_full$Theta

# random effects standard deviation
stand_dev_location <- mod_full$ST$Location_IDf
stand_dev_year <- mod_full$ST$CalYr


# Using CLMM2 to visualize the effects
mod_clmm2 <- clmm2(DensityClass_f ~ year_cat + ba_m2 + type,
                   random = Location_IDf,
                   Hess = TRUE,
                   data = bh_data)

# not worrying about the converged warning because this isn't technically the model?

summary(mod_clmm2)

# Creating predicted values
new_data <- expand.grid(year_cat = unique(bh_data$year_cat),
                        type = unique(bh_data$type),
                        ba_m2 = mean(bh_data$ba_m2, na.rm = TRUE))

pred_values <- predictor(mod_full,
                       data = new_data)

# combine new and predicted values
pred_data <- cbind(new_data,
                   pred_values)


# plotting ----
# Fixed effects
beta <- mod_full$beta

# thresholds
theta <- mod_full$Theta

# X-axis range
x_axis <- seq(min(beta),
              max(beta),
              length.out = 400)

# Function to compute category probabilities
prob_fun <- function(x, theta) {
  # cumulative probabilities
  cumprobs <- plogis(theta - x)
  # prepend 0 and append 1 for differences
  probs <- c(cumprobs, 1) - c(0, cumprobs)
  return(probs)
}

# Build long data frame
df_plot <- map_dfr(x_axis, function(x) {
  tibble(x = x,
         category = factor(1:(length(theta) + 1)),
         prob = prob_fun(x, theta)
  )
})

ggplot(df_plot, aes(x = x, y = prob, color = category)) +
  geom_line(size = 1.2) +
  labs(
    x = "Latent predictor (x)",
    y = "Probability",
    color = "Category",
    title = "Predicted Category Probabilities from CLMM"
  ) +
  theme_minimal(base_size = 16)


xlimNas = c(min(beta),
            max(beta))
ylimNas = c(0,1)

plot(0,0,
     xlim = xlimNas,
     ylim = ylimNas,
     type = "n",
     ylab = expression(Probability),
     xlab = "",
     xaxt = "n",
     main = "Predicted curves",
     cex = 2,
     cex.lab = 1.5,
     cex.main = 1.5,
     cex.axis = 1.5)
axis(side = 1,
     at = c(0 , beta),
     labels = levels(bh_data$year_cat), # not sure what is supposed to go here....
     las = 2,
     cex = 2,
     cex.lab = 1.5,
     cex.axis = 1.5)
xsNas = seq(xlimNas[1],
            xlimNas[2],
            length.out = 100)
lines(xsNas, plogis(mod_full$Theta[0] - xsNas), col = 'black')
lines(xsNas, plogis(mod_full$Theta[1] - xsNas) - plogis(mod_full$Theta[0] - xsNas), col = 'red')
lines(xsNas, plogis(mod_full$Theta[2] - xsNas) - plogis(mod_full$Theta[1] - xsNas), col = 'green')
lines(xsNas, plogis(mod_full$Theta[3] - xsNas) - plogis(mod_full$Theta[2] - xsNas), col = 'orange')
lines(xsNas, plogis(mod_full$Theta[4] - xsNas) - plogis(mod_full$Theta[3] - xsNas), col = 'yellow')
lines(xsNas, plogis(mod_full$Theta[5] - xsNas) - plogis(mod_full$Theta[4] - xsNas), col = 'pruple')
lines(xsNas, plogis(mod_full$Theta[6] - xsNas) - plogis(mod_full$Theta[5] - xsNas), col = 'pink')
lines(xsNas, 1-(plogis(mod_full$Theta[6] - xsNas)), col = 'blue')
abline(v = c(0, mod_full$beta),
       lty = 3)
abline(h = 0,
       lty = "dashed")
abline(h = 0.2,
       lty = "dashed")
abline(h = 0.4,
       lty = "dashed")
abline(h = 0.6,
       lty = "dashed")
abline(h = 0.8,
       lty = "dashed")
abline(h = 1,
       lty = "dashed")







pred_data <- expand.grid(year_cat = unique(bh_data$year_cat),
                         type = unique(bh_data$type),
                         ba_m2 = mean(bh_data$ba_m2, na.rm = TRUE),
                         CalYr = NA,
                         Location_ID = NA)


model_values <- predict(mod_full,
                        newdata = pred_data,
                        type = "prob")


# bh_data$Location_ID <- as.factor(bh_data$Location_ID)
# mod_clmm2 <- clmm2(DensityClass_f ~ year_cat + ba_m2 + type,
#                    random = Location_ID,
#                    Hess = TRUE,
#                    data = bh_data)
#
# predictions <- predict(mod_full,
#                        newdata = subset(bh_data, select = -DensityClass_f))$fit

# Interpreting Random effects
#      - larger variance = more variability betweeen groups
# Interpreting Coefficients:
#    - negative estimate means lower categoires more likely (e.g. increase in
#      cedar = smaller density classes more probable)
# Thresholds:
#   - The latent cut-point (e.g. latent density class value of 0.3 would be in
#       density class 2)


# Predicted probabilities
new_data<- expand.grid(year_cat = unique(bh_data$year_cat),
                       type = unique(bh_data$type),
                       ba_m2 = mean(bh_data$ba_m2, na.rm = TRUE))

ln_pred <- model.matrix(formula(DensityClass_f ~ year_cat + ba_m2 + type)[-2],
                        new_data) %*% mod_full$beta

# plotting predicted probabilities
pred_df <- as.data.frame(pred_mod_full,
                         response = "all")

ggplot(data = pred_df,
       aes(x = year_cat,
           y = prob,
           color = response,
           group = response)) +
  geom_line() +
  geom_point()

library(effects)


pred_df <- as.data.frame(pred_mod_full)


ggplot(pred_df,
       aes(x = year_cat,
           y = prob)) +
  geom_col(position = "stack") +
  facet_wrap(~type)


random <- as.data.frame(ranef())


# Plotting to visualize the effects
par(oma = c(1, 0, 0, 3),
    mgp = c(2, 1, 0))

# x and y axis
xlim_dc = c(min(mod_full$beta),
            max(mod_full$beta))

ylim_dc = c(0,1)

# plot
plot(0,0,
     xlim = xlim_dc,
     ylim = ylim_dc,
     type = "n",
     ylab = expression(Probability),
     xlab = "",
     xaxt = "n",
     main = "Predicted curves - Density Class",
     cex = 2,
     cex.lab = 1.5,
     cex.main = 1.5,
     cex.axis = 1.5)

axis(side = 1,
     at = c(0 , mod_full$beta),
     labels = levels(bh_data$year_cat),
     las = 2,
     cex = 2,
     cex.lab = 1.5,
     cex.axis = 1.5)

xs_dc = seq(xlim_dc[1],
            xlim_dc[2],
            length.out = 100)

# Predicted curves
lines(xs_dc, plogis(mod_full$Theta[1] - xs_dc), col='black')
lines(xs_dc, plogis(mod_full$Theta[2] - xs_dc)-plogis(mod_full$Theta[1] - xs_dc), col = 'red')
lines(xs_dc, plogis(mod_full$Theta[3] - xs_dc)-plogis(mod_full$Theta[2] - xs_dc), col = 'green')
lines(xs_dc, plogis(mod_full$Theta[4] - xs_dc)-plogis(mod_full$Theta[3] - xs_dc), col = 'orange')
lines(xs_dc, plogis(mod_full$Theta[5] - xs_dc)-plogis(mod_full$Theta[4] - xs_dc), col = 'purple')
lines(xs_dc, plogis(mod_full$Theta[6] - xs_dc)-plogis(mod_full$Theta[5] - xs_dc), col = 'yellow')
lines(xs_dc, 1-(plogis(mod_full$Theta[4] - xs_dc)), col = 'blue')
abline(v = c(0, mod_full$beta), lty = 3)

# Probabilty lines
abline(h = 0, lty = "dashed")
abline(h = 0.2, lty = "dashed")
abline(h = 0.4, lty = "dashed")
abline(h = 0.6, lty = "dashed")
abline(h = 0.8, lty = "dashed")
abline(h = 1, lty = "dashed")

legend(par('usr')[2],
       par('usr')[4],
       bty = 'n',
       xpd = NA,
       lty = 1,
       col = c("black",
               "red",
               "green",
               "orange",
               "blue"),
       legend = c("0",
                  "1",
                  "2",
                  "3",
                  "4",
                  "5",
                  "6"),
       cex = 0.75)


set.seed(12)

b <- 1000 # bootstrap iterations



mod1 <- clmm(DensityClass_f ~ year_cat + ba_m2 + type + (1|Location_ID) + (1|CalYr),
             data = bh_data)

mod2 <- clmm2(DensityClass_f ~ year_cat + ba_m2 + type, random = Location_ID,# + (1|CalYr),
             data = bh_data)

# Assumptions
# Proportional adds (fit with no random effects due to clm)
mod_clm <- clm(DensityClass_f ~ year_cat + ba_m2 + type + Location_ID,
               data = bh_data)

nominal_test(mod_clm)

scale_test(mod_clm)


# mod2 <- clmm(DensityClass ~ CalYr + type + (CalYr|Location_ID),
#              data = bh_all)
#
# mod3 <- clmm(DensityClass ~ CalYr * type + (1|Location_ID),
#              data = bh_all)

# If you want to look at if years are different (dont forget to set year as
# factor), friedman test. This will tell us if the distribution of classes
# has shifted across years (or treatments)




# Accuracy Data --------
# I need the estimated density class, count, % correct classification, % errors
# within 1 density class, % errors within 2 density classes

perc_correct <- bh_accuracy |>
  count()


# bad plots ----
# counts hist
ggplot() +
  geom_bar(data = bh_data |>
             filter(!DensityClass == 0 & ! DensityClass == 1),
           aes(Count))

ggplot() +
  geom_density(data = bh_data |>
             filter(!DensityClass == 0 & ! DensityClass == 1),
           aes(Count))

ggplot() +
  geom_bar(data = bh_accuracy,
               aes(Count,
                   fill = factor(DensityClass_Actual)),
           width = 5) +
  scale_y_continuous(limits = c(0, 20)) +
  scale_x_continuous(limits = c(0, 500))

ggplot() +
  geom_point(data = bh_data,
             aes(x = year,
                 y = Count,
                 color = DensityClass_f)) +
  geom_smooth(data = bh_data,
              aes(x = year,
                  y = Count,
                  color = DensityClass_f),
              se = FALSE)


ggplot() +
  geom_density(data = bh_accuracy |>
                 filter(!DensityClass_Actual < 2),
               aes(Count,
                   color = factor(DensityClass_Actual)))

ggplot() +
  geom_point(data = bh_data,
             aes(x = year,
                 y = DensityClass,
                 color = type))

# Boxplots of type
ggplot() +
  geom_boxplot(data = bh_data,
               aes(x = DensityClass_f,
                   y = Count,
                   color = type))


bh_sum <- bh_data |>
  count(year,
        DensityClass_f,
        type,
        name = "count")

ggplot() +
  geom_boxplot(data = bh_sum,
               aes(x = year,
                   y = count,
                   fill = DensityClass_f))
ggplot() +
  geom_point(data = bh_year,
             aes(x = year,
                 y = count,
                 color = DensityClass_f)) +
  geom_smooth(data = bh_year,
             aes(x = year,
                 y = count,
                 color = DensityClass_f),
             se = FALSE)



ggplot() +
  geom_point(data = bh_year_type,
             aes(x = year,
                 y = count,
                 color = DensityClass_f)) +
  geom_smooth(data = bh_year_type,
              aes(x = year,
                  y = count,
                  color = DensityClass_f),
              se = FALSE) +
  facet_wrap(~type)


# plot count data distributions



# data is right skewed
# With how the data are collected and placed into density classes, we can really
# only determine the estimated probability of each class in a given year,
# estimated cumulative probabilities, expected/typical class,
# and trends over time of the distribution classes.

# bh_data <- bh_counts |>
#   dplyr::left_join(bh_period) |>
#   # dplyr::left_join(bh_core) |>
#   dplyr::left_join(density_class) |>
#   dplyr::left_join(bh_accuracy) |>
#   select(ParkCode,
#          Location_ID,
#          Period_ID,
#          StartDate,
#          EndDate,
#          CalYr,
#          DensityClass,
#          LowerBound,
#          UpperBound,
#          Midpoint,
#          AreaSampled,
#          Count,
#          DensityClass_Est,
#          DensityClass_Actual)




# class 1, 2, 4, 5, 6 are pretty right skewed


