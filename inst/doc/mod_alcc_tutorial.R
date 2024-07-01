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
corr_df <- soiltestcorr::freitas1966

## ----warning=TRUE, message=TRUE-----------------------------------------------
# Using dataframe argument, tidy = FALSE -> return a LIST
mod_alcc(data = corr_df, ry = RY, stv = STK, target=90, confidence = 0.95,
         tidy = TRUE)


## ----warning=TRUE, message=TRUE-----------------------------------------------
# Using dataframe argument, tidy = FALSE -> return a LIST
mod_alcc(data = corr_df, ry = RY, stv = STK, target=90, confidence = 0.95, tidy = TRUE)


## ----warning=TRUE, message=TRUE-----------------------------------------------
fit_vectors_tidy <- mod_alcc(ry = corr_df$RY,
                             stv = corr_df$STK,
                             target = 90,
                             confidence = 0.95)

fit_vectors_list <- mod_alcc(ry = corr_df$RY,
                             stv = corr_df$STK,
                             target = 90,
                             confidence = 0.95,
                             tidy = FALSE)

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
  mutate(model = map(data, ~ mod_alcc(stv = .$STV, ry = .$RY, target = 90))) %>%
  unnest(model)

## ----warning=T, message=F-----------------------------------------------------

data.all %>% 
  group_by(id) %>% 
  group_modify(~ mod_alcc(data = ., STV, RY, target = 90, confidence = 0.95))


## ----warning=T, message=F-----------------------------------------------------
set.seed(123)
boot_alcc <- boot_mod_alcc(data = corr_df,
                           stv = STK, ry = RY,
                           target = 90, n = 500)

boot_alcc %>% head(n = 5)

# CSTV Confidence Interval
quantile(boot_alcc$CSTV, probs = c(0.025, 0.5, 0.975))

# Plot
boot_alcc %>% 
  ggplot2::ggplot(aes(x = CSTV))+
  geom_histogram(color = "grey25", fill = "steelblue", bins = 10)

## ----warning=F, message=F-----------------------------------------------------
plt_alcc <- mod_alcc(data = corr_df,
                     ry = RY, 
                     stv = STK, 
                     target = 95,
                     plot = TRUE)

plt_alcc

## ----warning=F, message=F-----------------------------------------------------
plt_alcc +
  # Main title
  ggtitle("My own plot title")+
  # Axis titles
  labs(x = "Soil Test K (ppm)",
       y = "Cotton RY(%)") +
  # Axis scales
  scale_x_continuous(limits = c(20,220),
                     breaks = seq(0,220, by = 25)) +
  # Axis limits
  scale_y_continuous(limits = c(30,100),
                     breaks = seq(20,100, by = 20))

## ----warning=F, message=F-----------------------------------------------------
fit_3 <- mod_alcc(data = corr_df, ry = RY, stv = STK, target = 90)
# Extract SMA regression fit and residuals from fit_3 (data_3, (Freitas et al., 1966))
SMA_freitas_1966 <- fit_3$SMA %>% as.data.frame()
 
SMA_freitas_1966 %>% 
  ggplot(aes(x = arc_RY, y = ln_STV))+
  ggtitle("SMA Regression. Dataset 3")+
  geom_point(shape=21, fill = "orange", size = 3, alpha = 0.75)+
  #SMA Line
  geom_path(aes(x=arc_RY, y = SMA_line, linetype = "SMA_fit"), linewidth = 1.5, col = "grey25")+
  scale_linetype_manual(name="", values = c("solid"))+
  #Critical value
  geom_vline(xintercept = 0, col = "grey10", size = 1.25, linetype = "dashed")+
  theme_bw()+
  # Axis titles
  labs(y = "ln_STV", y = "asin(sqrt(RY))-centered")

## ----warning=F, message=F-----------------------------------------------------

# Residuals plot
SMA_freitas_1966 %>% 
  ggplot(aes(x = fitted_axis, y = residuals))+
  ggtitle("Residuals SMA. Dataset 3")+
  geom_point(shape=21, fill = "orange", size = 3, alpha = 0.75)+
  geom_hline(yintercept = 0, col = "grey10", linewidth = 1.25, linetype = "dashed")+
  theme_bw()+
  # Axis titles
  labs(x = "Fitted Axis -SMA- (see Warton et al. 2006)", y = "Residuals (STV units)")


