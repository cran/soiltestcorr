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
library(utils) # Data wrangling
library(data.table) # Mapping
library(purrr) # Mapping


## -----------------------------------------------------------------------------

# Example 1 dataset
# Fake dataset manually created
data_1 <- data.frame("RY"  = c(65,80,85,88,90,94,93,96,97,95,98,100,99,99,100),
                     "STV" = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15))
  
# Example 2. Native fake dataset from soiltestcorr package

data_2 <- soiltestcorr::data_test


# Example 3. Native dataset from soiltestcorr package, Freitas et al.  (1966), used by Cate & Nelson (1971)
data_3 <- soiltestcorr::freitas1966



## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_1_type_1 <- 
  soiltestcorr::mitscherlich(data = data_1, 
                             ry = RY, 
                             stv = STV, 
                             type = 1, 
                             target = 90)

utils::head(fit_1_type_1)


## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_1_type_2 <- 
  soiltestcorr::mitscherlich(data = data_1, 
                             ry = RY, 
                             stv = STV, 
                             type = 2, 
                             target = 90)

utils::head(fit_1_type_2)


## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_1_type_3 <- 
  soiltestcorr::mitscherlich(data = data_1, 
                             ry = RY, 
                             stv = STV, 
                             type = 3, 
                             target = 90)

utils::head(fit_1_type_3)


## ----warning=TRUE, message=TRUE-----------------------------------------------

# Using dataframe argument, tidy = FALSE -> return a LIST
fit_1_tidy_false <- 
  soiltestcorr::mitscherlich(data = data_1, 
                               ry = RY, 
                               stv = STV, type = 1, target = 90, 
                               tidy = FALSE)

utils::head(fit_1_tidy_false)


## ----warning=TRUE, message=TRUE-----------------------------------------------

# Using dataframe argument, tidy = FALSE -> return a LIST
fit_1_tidy_true <- 
  soiltestcorr::mitscherlich(data = data_1, 
                               ry = RY, 
                               stv = STV, type = 1, target = 90,
                               tidy = TRUE)

fit_1_tidy_true


## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_1_vectors_list <-
  soiltestcorr::mitscherlich(ry = data_1$RY,
                             stv = data_1$STV,
                             type = 1,
                             tidy = FALSE)

fit_1_vectors_tidy <- 
  soiltestcorr::mitscherlich(ry = data_1$RY,
                             stv = data_1$STV,
                             type = 1,
                             tidy = TRUE)


## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_2 <-
  soiltestcorr::mitscherlich(data = data_2, 
                             ry = RY,
                             stv = STV,
                             type = 1,
                             target = 90)

utils::head(fit_2)

## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_3 <-
  soiltestcorr::mitscherlich(data = data_3, 
                             ry = RY,
                             stv = STK, 
                             type = 1, 
                             target = 90)
utils::head(fit_3)


## ----warning=T, message=F-----------------------------------------------------
# 
data.all <- dplyr::bind_rows(data_1, data_2,
                      data_3 %>% dplyr::rename(STV = STK),
                     .id = "id") %>% 
  tidyr::nest(data = c("STV", "RY"))

## ----warning=T, message=F-----------------------------------------------------

# Run multiple examples at once with map()
fit_multiple_map <-
  data.all %>%
  dplyr::mutate(models = purrr::map(data, 
                                     ~ soiltestcorr::mitscherlich(ry = .$RY,
                                                                  stv = .$STV,
                                                                  type = 1,
                                                                  target = 90,
                                                                  tidy = TRUE)))

utils::head(fit_multiple_map)

unnest(fit_multiple_map, models)


## ----warning=T, message=F-----------------------------------------------------

fit_multiple_group_map <- 
  dplyr::bind_rows(data_1, data_2, .id = "id") %>% 
  dplyr::group_by(id) %>% 
  dplyr::group_map(~ soiltestcorr::mitscherlich(data = ., 
                                           ry = RY,
                                           stv = STV, type = 1, target = 90,
                                           tidy = TRUE))

utils::head(fit_multiple_group_map)


## ----warning=F, message=F-----------------------------------------------------

mitscherlich_plot <- 
  soiltestcorr::mitscherlich(data = data_3, 
                               ry = RY, 
                               stv = STK, type = 1, target = 90, 
                               plot = TRUE)

mitscherlich_plot

## ----warning=F, message=F-----------------------------------------------------
mitscherlich_plot_2 <- 
  mitscherlich_plot +
  labs(
    # Main title
    title = "My own plot title",
    # Axis titles
    x = "Soil Test K (ppm)",
    y = "Cotton RY(%)")

mitscherlich_plot_2

## ----warning=F, message=F-----------------------------------------------------
mitscherlich_plot_3 <-
mitscherlich_plot_2 +
  # Axis scales
  scale_x_continuous(breaks = seq(0,220, by = 20))+
  # Axis limits
  scale_y_continuous(breaks = seq(0,100, by = 10))

mitscherlich_plot_3
  

## ----warning=F, message=F-----------------------------------------------------

# Residuals plot

soiltestcorr::mitscherlich(data = data_3, 
                               ry = RY, 
                               stv = STK, type = 1, target = 90, 
                               resid = TRUE)


