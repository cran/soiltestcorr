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

theme_set(theme_minimal())

## ----warning=F, message=F-----------------------------------------------------
# Clay loam simulated dataset
clay_loam <- dplyr::tibble(
  "soil" = "clay_loam",
  "RY"  = c(65,80,85,88,90,94,93,96,97,95,98,100,99,99,100),
  "STV" = seq(1, 15, by = 1) )

# Sandy loam simulated dataset
sandy_loam <- dplyr::tibble(
  "soil" = "sandy_loam",
  "RY" = c(55,70,80,84,89,92,90,94,99,92,96,100,100,99,99), 
  "STV" = seq(2, 16, by = 1))

# Merging soils datasets
dataframe_soils <- bind_rows(clay_loam, sandy_loam)

## ----warning=F, message=F-----------------------------------------------------
set.seed(123)
boot_cn_65 <- boot_cn_1965(data = dataframe_soils,
                           stv = STV, ry = RY, 
                           target = 90,
                           n = 100, # only 100 replicates to save compute time on demo
                           # Group by soil
                           soil = soil)

# CSTV Confidence Interval
boot_cn_65_sum <- boot_cn_65 %>%
  group_by(soil) %>% 
  # Obtain quantiles of interest
  summarise(q025 = quantile(CSTV, prob = 0.025, na.rm = TRUE),
            median = quantile(CSTV, prob = 0.500, na.rm = TRUE),
            q975 = quantile(CSTV, prob = 0.975, na.rm = TRUE)) %>% 
  ungroup()

# Plot CSTV
boot_cn_65 %>% 
  ggplot2::ggplot(aes(x = CSTV))+
  geom_density(aes(fill = soil), color = "grey40", alpha = 0.5)+
  # Draw lines
  geom_vline(data = boot_cn_65_sum, aes(xintercept = median, color = soil),
             linetype = "dashed", linewidth = 1)+
  geom_vline(data = boot_cn_65_sum, aes(xintercept = q025, color = soil),
              linetype = "dotted", linewidth = 0.5)+
  geom_vline(data = boot_cn_65_sum, aes(xintercept = q975, color = soil),
              linetype = "dotted", linewidth = 0.5)+
  scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1))+
  labs(title = "boot_cn_1965()", x = "CSTV for RY=90%")


## ----warning=F, message=F-----------------------------------------------------
set.seed(123)
boot_cn_71 <- boot_cn_1971(data = dataframe_soils,
                           stv = STV, ry = RY, 
                           n = 100, # only 100 replicates to save compute time on demo
                           # Group by soil
                           soil = soil)

# CSTV Confidence Interval
boot_cn_71_sum <- boot_cn_71 %>% 
              group_by(soil) %>% 
            # Obtain quantiles of interest
              summarise(q025 = quantile(CSTV, prob = 0.025, na.rm = TRUE),
                        median = quantile(CSTV, prob = 0.500, na.rm = TRUE),
                        q975 = quantile(CSTV, prob = 0.975, na.rm = TRUE))

# Plot CSTV
boot_cn_71 %>% 
  ungroup() %>% 
  ggplot2::ggplot(aes(x = CSTV, y = CRYV))+
  geom_density_2d(aes(color = soil), shape = 21, alpha = 0.5)+
  # Draw lines
  geom_vline(data = boot_cn_71_sum, aes(xintercept = median, color = soil),
             linetype = "dashed", linewidth = 1)+
  scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1))+
  labs(title = "boot_cn_1971()", x = "CSTV for CRYV", y = "RV (%) at CSTV")


## ----warning=F, message=F-----------------------------------------------------
set.seed(123)
boot_alcc <- boot_mod_alcc(data = dataframe_soils,
                           stv = STV, ry = RY,
                           target = 90, n = 1000, 
                           # Group by id
                           soil = soil)

# CSTV Confidence Interval
boot_alcc_sum <- boot_alcc %>% 
              group_by(soil) %>% 
            # Obtain quantiles of interest
              summarise(q025 = quantile(CSTV, prob = 0.025, na.rm = TRUE),
                        median = quantile(CSTV, prob = 0.500, na.rm = TRUE),
                        q975 = quantile(CSTV, prob = 0.975, na.rm = TRUE))

# Plot CSTV
boot_alcc %>% 
  ungroup() %>% 
  ggplot2::ggplot(aes(x = CSTV))+
  geom_density(aes(fill = soil), color = "grey40", alpha = 0.5)+
  # Draw lines
  geom_vline(data = boot_alcc_sum, aes(xintercept = median, color = soil),
             linetype = "dashed", linewidth = 1)+
  geom_vline(data = boot_alcc_sum, aes(xintercept = q025, color = soil),
              linetype = "dotted", linewidth = 0.5)+
  geom_vline(data = boot_alcc_sum, aes(xintercept = q975, color = soil),
              linetype = "dotted", linewidth = 0.5)+
  scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1))+
  labs(title = "boot_mod_alcc()", x = "CSTV for RY=90%")+
  theme_bw()


## ----warning=F, message=F-----------------------------------------------------
set.seed(123)
boot_lp <- boot_linear_plateau(data = dataframe_soils,
                               stv = STV, ry = RY,
                               target = 90, n = 1e3,
                               # Group by soil
                               soil = soil)

# CSTV Confidence Interval
boot_lp_sum <- boot_lp %>% 
              group_by(soil) %>% 
            # Obtain quantiles of interest
            # Note: for linear_plateau, the CSTV for a specific RY target = STVt
              summarise(q025 = quantile(STVt, prob = 0.025, na.rm = TRUE),
                        median = quantile(STVt, prob = 0.500, na.rm = TRUE),
                        q975 = quantile(STVt, prob = 0.975, na.rm = TRUE))

# Plot STVt
boot_lp %>% 
  ungroup() %>% 
  ggplot2::ggplot(aes(x = STVt))+
  geom_density(aes(fill = soil), color = "grey40", alpha = 0.5)+
  # Draw lines
  geom_vline(data = boot_lp_sum, aes(xintercept = median, color = soil),
             linetype = "dashed", linewidth = 1)+
  geom_vline(data = boot_lp_sum, aes(xintercept = q025, color = soil),
              linetype = "dotted", linewidth = 0.5)+
  geom_vline(data = boot_lp_sum, aes(xintercept = q975, color = soil),
              linetype = "dotted", linewidth = 0.5)+
  scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1))+
  labs(title = "boot_linear_plateau()", x = "STVt for RY=90%")+
  theme_bw()


## ----warning=F, message=F-----------------------------------------------------
set.seed(123)
boot_qp <- boot_quadratic_plateau(data = dataframe_soils,
                               stv = STV, ry = RY,
                               target = 90, n = 1000,
                               # Group by soil
                               soil = soil)

# CSTV Confidence Interval
boot_qp_sum <- boot_qp %>% 
              group_by(soil) %>% 
            # Obtain quantiles of interest
            # Note: for quad_plateau, the CSTV for a specific RY target = STVt
              summarise(q025 = quantile(STVt, prob = 0.025, na.rm = TRUE),
                        median = quantile(STVt, prob = 0.500, na.rm = TRUE),
                        q975 = quantile(STVt, prob = 0.975, na.rm = TRUE))

# Plot STVt
boot_qp %>% 
  ungroup() %>% 
  ggplot2::ggplot(aes(x = STVt))+
  geom_density(aes(fill = soil), color = "grey40", alpha = 0.5)+
  # Draw lines
  geom_vline(data = boot_qp_sum, aes(xintercept = median, color = soil),
             linetype = "dashed", linewidth = 1)+
  geom_vline(data = boot_qp_sum, aes(xintercept = q025, color = soil),
              linetype = "dotted", linewidth = 0.5)+
  geom_vline(data = boot_qp_sum, aes(xintercept = q975, color = soil),
              linetype = "dotted", linewidth = 0.5)+
  scale_x_continuous(limits = c(1,10), breaks = seq(2, 10, by = 1))+
  labs(title = "boot_quadratic_plateau()", x = "CSTV for RY=90%")+
  theme_bw()


## ----warning=F, message=F-----------------------------------------------------
set.seed(123)
boot_mits <- boot_mitscherlich(data = dataframe_soils,
                               stv = STV, ry = RY, type = 1,
                               target = 90, n = 1e3,
                               # Group by soil
                               soil = soil)

# CSTV Confidence Interval
boot_mits_sum <- boot_mits %>% 
              group_by(soil) %>% 
            # Obtain quantiles of interest
              summarise(q025 = quantile(CSTV, prob = 0.025, na.rm = TRUE),
                        median = quantile(CSTV, prob = 0.500, na.rm = TRUE),
                        q975 = quantile(CSTV, prob = 0.975, na.rm = TRUE))

# Plot CSTV
boot_mits %>% 
  ungroup() %>% 
  ggplot2::ggplot(aes(x = CSTV))+
  geom_density(aes(fill = soil), color = "grey40", alpha = 0.5)+
  # Draw lines
  geom_vline(data = boot_mits_sum, aes(xintercept = median, color = soil),
             linetype = "dashed", linewidth = 1)+
  geom_vline(data = boot_mits_sum, aes(xintercept = q025, color = soil),
              linetype = "dotted", linewidth = 0.5)+
  geom_vline(data = boot_mits_sum, aes(xintercept = q975, color = soil),
              linetype = "dotted", linewidth = 0.5)+
  scale_x_continuous(limits = c(1,10), breaks = seq(1, 10, by = 1))+
  labs(title = "boot_mitscherlich()", x = "CSTV for RY=90%")+
  theme_bw()

