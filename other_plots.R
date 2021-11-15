library(INLA)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(sf)

# ==========================================================================
# Additional plots of the space-time residuals
# ==========================================================================

# The inla result object
load("res_main.RData")
inla_res <- res_main

# For spatial plotting
load("space_obj.Rdata")

# ==========================================================================
# 1. Exceedance probabilities: probability that the residual > threshold
# - requires running R-INLA with: `control.predictor=list(compute = TRUE)`
# - see here for more info: https://www.paulamoraga.com/book-geospatial/sec-inla.html 
# ==========================================================================

# Retrieve marginals from R-INLA output
marginals <- inla_res$marginals.random$date_LTLA_ID

# Define thresholds for exceedance probabilities (i.e., p(param > threshold))
thresholds <- seq(0,1,0.5)
exc <- list()
for (i in 1:length(thresholds)){
  exc[[i]] <- sapply(marginals, FUN = function(mar){1-inla.pmarginal(q=thresholds[i], marginal=mar)})
}

# Need this for spatial plotting
plot_df <- LTLA_shp_Reg
# We will get a plot for each threshold
plot_list = list()

# Specify which week want to plot
# --> this is required to correctly index output of the space-time interactions
week <- 1
for(i in 1:length(thresholds)){
  
  start_idx <- 1 + (week-1)*nrow(LTLA_shp_Reg)
  end_idx <- week * nrow(LTLA_shp_Reg)
  
  # Get the result probabilities for threshold i in the given week and convert to factor 
  res <-  cut(exc[[i]][start_idx:end_idx],
        breaks = c(0, 0.1, 0.2, 0.8, 0.9, 1),
        labels = c("[0,0.1)", "[0.1,0.2)", "[0.2,0.8)", "[0.8,0.9)", "[0.9,1.0]"),
        right=FALSE,
        include.highest=TRUE
  )
  plot_df$val <-  as.factor(res)

  # Create plot for the week
  pp <- ggplot(plot_df) + geom_sf(aes(fill = val), alpha = 0.5, colour = NA) +
  labs(fill = "") + ggtitle('Exceedance probabilities',
                            subtitle = paste('P(\U1D709 >', thresholds[[i]],')')) +
  scale_fill_brewer(palette='RdYlGn', drop=FALSE, direction = -1) + theme_minimal()
  plot_list[[i]] <-pp
}

# Combine all the threshold plots
plot_all <- plot_list[[1]] + ggtitle("") 
for (i in 2:length(thresholds)){
  plot_all <- plot_all + plot_list[[i]] + ggtitle("")
}
ppp <- plot_all + plot_layout(guides = "collect") & scale_fill_brewer(palette='RdYlGn', drop=FALSE, direction =  -1)
ppp

# ==========================================================================
# 2. Anomalies 
# - For each LTLA and quarter (15 week period), count the number of weeks that 
#   the median value of the space-time residual was above some threshold value
# ==========================================================================

# Set threshold (Odds ratio scale)
threshold <- 2
# Need this for spatial plotting
plot_df <- LTLA_shp_Reg 

# Loop through weeks
for(n in 1:45){
  start <- 1+(n-1)*nrow(LTLA_shp_Reg)
  end <- n*nrow(LTLA_shp_Reg)
  # NOTE: we are on the OR rather than logit scale
  res <- exp(inla_res$summary.random$date_LTLA_ID$`0.5quant`[start:end])
  # Set anything above our threshold as NA (then can just count those)
  res[res > threshold] <- NA
  if (n < 10){
    plot_df[,paste0('week0', n)] <- res
  } else{
    plot_df[,paste0('week', n)] <- res
  }
}  
plot_df <- plot_df %>% gather('week', 'val', week01:week45)  
plot_df$date_ID <- as.integer(as.factor(plot_df$week))

# Here we define a quarter as a 15 week period
plot_df <- plot_df %>% 
  mutate(quarter = if_else(date_ID <= 15, 'Quarter 1', 'Quarter 2')) %>%
  mutate(quarter = if_else(date_ID > 30, 'Qarter 3', quarter)) 

# Count the NAs in each quarter
sub <- plot_df %>% group_by(LTLA_ID, quarter) %>% summarise(count = sum(is.na(val)), .groups='drop')
sub$quarter <- factor(sub$quarter, levels=c('Quarter 1', 'Quarter 2', 'Qarter 3'))
sub$count <- as.factor(sub$count)

# Plot
pp <- ggplot(sub) + geom_sf(aes(fill = count), colour = NA) +
  labs(fill="Anomalous\nweeks", 
       title='', 
       y="Test Positivity",
       subtitle="") + 
  scale_fill_viridis_d() +
  theme_minimal() + facet_wrap(~quarter, ncol=3) 
pp
