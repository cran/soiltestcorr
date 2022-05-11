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

# Using dataframe argument, tidy = FALSE -> return a LIST
fit_1_tidy_false <-
  soiltestcorr::mod_alcc(data = data_1, 
                         ry = RY, 
                         stv = STV, 
                         target=90, 
                         confidence = 0.95, 
                         tidy = FALSE)

utils::head(fit_1_tidy_false)


## ----warning=TRUE, message=TRUE-----------------------------------------------

# Using dataframe argument, tidy = FALSE -> return a LIST
fit_1_tidy_true <-
  soiltestcorr::mod_alcc(data = data_1, 
                         ry = RY, 
                         stv = STV, 
                         target=90, 
                         confidence = 0.95, 
                         tidy = TRUE)

fit_1_tidy_true


## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_1_vectors_list <-
  soiltestcorr::mod_alcc(ry = data_1$RY,
                         stv = data_1$STV, 
                         target=90,
                         confidence = 0.95,
                         tidy = FALSE)

fit_1_vectors_tidy <-
  soiltestcorr::mod_alcc(ry = data_1$RY,
                         stv = data_1$STV, 
                         target=90,
                         confidence = 0.95,
                         tidy = TRUE)


## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_2 <-
  soiltestcorr::mod_alcc(data = data_2, 
                         ry = RY,
                         stv = STV,
                         target=90, 
                         confidence = 0.95)

utils::head(fit_2)

## ----warning=TRUE, message=TRUE-----------------------------------------------

fit_3 <-
  soiltestcorr::mod_alcc(data = data_3, 
                         ry = RY,
                         stv = STK,
                         target=90,
                         confidence = 0.95)
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
  dplyr::mutate(mod_alcc = purrr::map(data, 
                               ~ soiltestcorr::mod_alcc(ry = .$RY,
                                                        stv = .$STV,
                                                        target=90,
                                                        confidence = 0.95,
                                                        tidy = TRUE)))

utils::head(fit_multiple_map)


## ----warning=T, message=F-----------------------------------------------------

fit_multiple_group_map <- 
  dplyr::bind_rows(data_1, data_2, .id = "id") %>% 
  dplyr::group_by(id) %>% 
  dplyr::group_map(~ soiltestcorr::mod_alcc(data = ., 
                                     ry = RY,
                                     stv = STV, 
                                     target = 90, 
                                     confidence = 0.95, tidy = TRUE))

utils::head(fit_multiple_group_map)


## ----warning=F, message=F-----------------------------------------------------

modalcc_plot <- 
  soiltestcorr::mod_alcc(data = data_3,
                         ry = RY, 
                         stv = STK, 
                         target=95, 
                         confidence = 0.95, 
                         plot = TRUE)

modalcc_plot

## ----warning=F, message=F-----------------------------------------------------
modalcc_plot_2 <- 
  modalcc_plot +
  # Main title
  ggtitle("My own plot title")+
  # Axis titles
  labs(x = "Soil Test K (ppm)",
       y = "Cotton RY(%)")

modalcc_plot_2

## ----warning=F, message=F-----------------------------------------------------

modalcc_plot_3 <-
modalcc_plot_2 +
  # Axis scales
  scale_x_continuous(limits = c(20,220),
                     breaks = seq(0,220, by = 20))+
  # Axis limits
  scale_y_continuous(limits = c(30,100),
                     breaks = seq(30,100, by = 10))

modalcc_plot_3
  

## ----warning=F, message=F-----------------------------------------------------

# Extract SMA regression fit and residuals from fit_3 (data_3, (Freitas et al., 1966))
SMA_freitas_1966 <- fit_3$SMA %>% as.data.frame()
 
SMA_freitas_1966 %>% 
  ggplot(aes(x = arc_RY, y = ln_STV))+
  ggtitle("SMA Regression. Dataset 3")+
  geom_point(shape=21, fill = "orange", size = 4, alpha = 0.75)+
  #SMA Line
  geom_path(aes(x=arc_RY, y = SMA_line, linetype = "SMA_fit"), size = 2, col = "grey25")+
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
  geom_point(shape=21, fill = "orange", size = 4, alpha = 0.75)+
  geom_hline(yintercept = 0, col = "grey10", size = 1.25, linetype = "dashed")+
  theme_bw()+
  # Axis titles
  labs(x = "Fitted Axis -SMA- (see Warton et al. 2006)", y = "Residuals (STV units)")


