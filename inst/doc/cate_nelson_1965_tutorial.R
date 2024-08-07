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
library(utils) # Data wrangling
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

# Using dataframe argument, tidy = FALSE -> return a LIST
fit_1_tidy_false <- 
  soiltestcorr::cate_nelson_1965(data = data_1,
                                 ry = RY,
                                 stv = STV,
                                 target = 90,
                                 tidy = FALSE,
                                 plot = FALSE)

utils::head(fit_1_tidy_false)


## ----warning=TRUE, message=TRUE-----------------------------------------------

# Using dataframe argument, tidy = FALSE -> return a LIST
fit_1_tidy_false <- 
  soiltestcorr::cate_nelson_1965(data = data_1,
                                 ry = RY,
                                 stv = STV,
                                 target = 90,
                                 tidy = TRUE)

utils::head(fit_1_tidy_false)


## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_1_vectors_list <-
  soiltestcorr::cate_nelson_1965(ry = data_1$RY,
                                 stv = data_1$STV, 
                                 target=90,
                                 tidy = FALSE)

fit_1_vectors_tidy <-
  soiltestcorr::cate_nelson_1965(ry = data_1$RY,
                                 stv = data_1$STV, 
                                 target=90,
                                 tidy = TRUE)


## ----warning=TRUE, message=TRUE-----------------------------------------------
fit_2 <-
  soiltestcorr::cate_nelson_1965(data = data_2,
                                 ry = RY,
                                 stv = STV,
                                 target = 90,
                                 tidy = TRUE)

utils::head(fit_2)

## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_3 <-
  soiltestcorr::cate_nelson_1965(data = data_3,
                                 ry = RY,
                                 stv = STK,
                                 target = 90,
                                 tidy = TRUE)

utils::head(fit_3)


## ----warning=T, message=F-----------------------------------------------------
# 
data.all <- dplyr::bind_rows(data_1, data_2,
                      data_3 %>% dplyr::rename(STV = STK),
                     .id = "id") %>% 
  tidyr::nest(data = c("STV", "RY"))

## ----warning=T, message=F-----------------------------------------------------

# Run multiple examples at once with map()
fit_multiple_map = data.all %>%
  dplyr::mutate(mod_alcc = purrr::map(data, ~ soiltestcorr::cate_nelson_1965(ry = .$RY,
                                                              stv = .$STV,
                                                              target=90,
                                                              tidy = TRUE)))

utils::head(fit_multiple_map)


## ----warning=T, message=F-----------------------------------------------------

fit_multiple_group_map <- 
  data.all %>% tidyr::unnest(data) %>% 
  #dplyr::bind_rows(data_1, data_2, .id = "id") %>% 
  dplyr::group_by(id) %>% 
  dplyr::group_map(~ soiltestcorr::cate_nelson_1965(data = ., 
                                     ry = RY,
                                     stv = STV, 
                                     target = 90, 
                                     tidy = TRUE))

utils::head(fit_multiple_group_map)


## -----------------------------------------------------------------------------
boot_cn65 <- boot_cn_1965(data = data_3,
                          ry = RY, stv = STK, target = 90,
                          n = 99)

boot_cn65 %>% dplyr::slice_head(., n=5)

# CSTV Confidence Interval
quantile(boot_cn65$CSTV, probs = c(0.025, 0.5, 0.975))

# Plot
boot_cn65 %>% 
  ggplot2::ggplot(aes(x = CSTV))+
  geom_histogram(color = "grey25", fill = "#9de0bf", bins = 10)


## ----warning=F, message=F-----------------------------------------------------

soiltestcorr::cate_nelson_1965(data = data_1, 
                       ry = RY, 
                       stv = STV, 
                       target=90, 
                       plot = TRUE)

soiltestcorr::cate_nelson_1965(data = data_2, 
                       ry = RY, 
                       stv = STV, 
                       target=90, 
                       plot = TRUE)

soiltestcorr::cate_nelson_1965(data = data_3, 
                       ry = RY, 
                       stv = STK, 
                       target=90, 
                       plot = TRUE)

