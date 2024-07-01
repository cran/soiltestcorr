## ----include = FALSE----------------------------------------------------------
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

# Type = 1, no restriction (3 parameters)
mitscherlich(corr_df, STV, RY, type = 1)
# Type = 2, fixed asymptote value at 100 (2 parameters)
mitscherlich(corr_df, STV, RY, type = 2)
# Type = 3, fixed origin at 0 and asymptote at 100 (1 parameters)
mitscherlich(corr_df, STV, RY, type = 3)


## ----warning=TRUE, message=TRUE-----------------------------------------------

# Using dataframe argument, tidy = FALSE -> return a LIST
mitscherlich(data = corr_df, STV, RY, target = 90,  tidy = FALSE)

## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_vectors_list <-mitscherlich(stv = corr_df$STV,
                                ry = corr_df$RY,
                                tidy = FALSE)

fit_vectors_tidy <-mitscherlich(stv = corr_df$STV,
                                ry = corr_df$RY,
                                tidy = TRUE)

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

# Run multiple examples at once with map()
data.all %>%
  nest(data = c("STV", "RY")) %>% 
  mutate(model = map(data, ~ mitscherlich(stv = .$STV, ry = .$RY))) %>%
  unnest(model)

## ----warning=T, message=F-----------------------------------------------------

data.all %>% 
  group_by(id) %>% 
  group_modify(~ soiltestcorr::mitscherlich(data = ., STV, RY))


## -----------------------------------------------------------------------------
set.seed(123)
boot_mits <- boot_mitscherlich(corr_df, STV, RY, target = 90, n = 200)

boot_mits %>% head(n = 5)

# CSTV Confidence Interval
quantile(boot_mits$CSTV, probs = c(0.025, 0.5, 0.975), na.rm = TRUE)

# Plot
boot_mits %>% 
  ggplot2::ggplot(aes(x = CSTV))+
  geom_histogram(color = "grey25", fill = "#9de0bf", bins = 10)

## ----warning=F, message=F-----------------------------------------------------
data_3 <- soiltestcorr::freitas1966

plot_mit <- mitscherlich(data_3, STK, RY, plot = TRUE)

plot_mit

## ----warning=F, message=F-----------------------------------------------------
plot_mit +
  # Main title
  ggtitle("My own plot title")+
  # Axis titles
  labs(x = "Soil Test K (ppm)",
       y = "Cotton RY(%)") +
  # Axis scales
  scale_x_continuous(limits = c(20,220),
                     breaks = seq(0,220, by = 10))

## ----warning=F, message=F-----------------------------------------------------

# Residuals plot
mitscherlich(data_3, STK, RY, resid = TRUE)


