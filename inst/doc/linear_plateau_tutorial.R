## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=6, 
  fig.height=4
)

## ----setup--------------------------------------------------------------------
library(soiltestcorr)

## ----warning=FALSE, message=FALSE---------------------------------------------
# Install if needed 
library(ggplot2) # Plots
library(dplyr) # Data wrangling
library(tidyr) # Data wrangling
library(purrr) # Mapping


## -----------------------------------------------------------------------------
# Native fake dataset from soiltestcorr package
corr_df <- soiltestcorr::data_test

## ----warning=TRUE, message=TRUE-----------------------------------------------

linear_plateau(corr_df, STV, RY, tidy = TRUE)

## ----warning=TRUE, message=TRUE-----------------------------------------------

linear_plateau(corr_df, STV, RY, tidy = FALSE)

## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_vectors_tidy <- linear_plateau(stv = corr_df$STV, ry = corr_df$RY)

fit_vectors_list <- linear_plateau(stv = corr_df$STV, ry = corr_df$RY, tidy = FALSE)

## ----warning=T, message=F-----------------------------------------------------
# Example 1. Fake dataset manually created
data_1 <- data.frame("RY"  = c(65,80,85,88,90,94,93,96,97,95,98,100,99,99,100),
                     "STV" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
  
# Example 2. Native fake dataset from soiltestcorr package
data_2 <- soiltestcorr::data_test


# Example 3. Native dataset from soiltestcorr package, Freitas et al.  (1966), used by Cate & Nelson (1971)
data_3 <- soiltestcorr::freitas1966 %>% 
  rename(STV = STK)

data.all <- bind_rows(data_1, data_2, data_3, .id = "id")

## ----warning=T, message=F-----------------------------------------------------

# Run multiple examples at once with purrr::map()
data.all %>%
  nest(data = c("STV", "RY")) %>% 
  mutate(model = map(data, ~ linear_plateau(stv = .$STV, ry = .$RY))) %>%
  unnest(model)


## ----warning=T, message=F-----------------------------------------------------

data.all %>% 
  group_by(id) %>% 
  group_modify(~ linear_plateau(data = ., STV, RY))


## -----------------------------------------------------------------------------
set.seed(123)

boot_lp <- boot_linear_plateau(corr_df, STV, RY, n = 500) # only 500 for sake of speed

boot_lp %>% head(n = 5)

# CSTV Confidence Interval
quantile(boot_lp$CSTV, probs = c(0.025, 0.5, 0.975))

# Plot
boot_lp %>% 
  ggplot(aes(x = CSTV))+
  geom_histogram(color = "grey25", fill = "#9de0bf", bins = 10)

## ----warning=F, message=F-----------------------------------------------------
data_3 <- soiltestcorr::freitas1966

plot_lp <- linear_plateau(data = data_3, STK, RY, plot = TRUE)

plot_lp

## ----warning=F, message=F-----------------------------------------------------
plot_lp +
  # Main title
  ggtitle("My own plot title")+
  # Axis titles
  labs(x = "Soil Test K (ppm)",
       y = "Cotton RY(%)") +
  # Axis scales
  scale_x_continuous(limits = c(20,220),
                     breaks = seq(0,220, by = 10)) +
  # Axis limits
  scale_y_continuous(limits = c(30,100),
                     breaks = seq(30,100, by = 10))

## ----warning=F, message=F-----------------------------------------------------

# Residuals plot

linear_plateau(data_3, STK, RY, resid = TRUE)


