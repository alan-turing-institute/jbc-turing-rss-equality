library(INLA)
# data manipulation
library(tidyr)
library(dplyr)
# plotting
library(ggplot2)
library(patchwork)

# Number of LTLAs and weeks in analysis 
n_LTLA = 311
n_weeks = 45

# Load result of the analysis
load("res_main.RData")

# Load data
fin_data <- readRDS(file="toy_data.RDS")
# remove duplicate rows
fin_data <- fin_data %>% 
  group_by(LTLA_ID, date_ID, age_class) %>% 
  filter(row_number()==1) %>% ungroup()

# Load object LTLA_shp_Reg with geography data to link LTLA IDs to locations for plotting
load("space_obj.RData")

# ==========================================================================
# 1. Temporal trend (logit scale)
# - Supplementary Figure 3 in the manuscript
# ========================================================================== 

make_time_plot <- function(inla_res, outcome_name, color_band) { 
  
  # temporal random effects
  time_res <- inla_res$summary.random$date_ID
  time_res$week <- seq.Date(start_date, end_date, by = 7)
  
  # space-time interactions
  inter_df <- data.frame(LTLA_ID = 1:n_LTLA)
  for(n in 1:n_weeks){
    start <- 1+(n-1)*n_LTLA
    end <- n*n_LTLA
    res <- inla_res$summary.random$date_LTLA_ID$`0.5quant`[start:end]
    #res[res > 2] <- NA
    if (n < 10){
      inter_df[,paste0('week0', n)] <- res
    } else{
      inter_df[,paste0('week', n)] <- res
    }
  }
  # average over LTLAs to get mean by week
  # !! NOTE: BELOW DEPENDS ON NUMBER OF WEEKS IN DATA !!
  time_inter_df <- inter_df %>% gather('week', 'val', week01:week45)
  time_res_week <- time_inter_df %>% group_by(week) %>% summarise(mean= mean(val, na.rm = T))
    
  # sum the two temporal components
  time_res$lin_pred <- (time_res$mean + time_res_week$mean)
  time_res$lin_pred_0.025quant <- (time_res$`0.025quant` + time_res_week$mean)
  time_res$lin_pred_0.975quant <- (time_res$`0.975quant` + time_res_week$mean)
  
  # plot
  time_plot <- ggplot(time_res, aes(x = week, y = lin_pred)) + geom_point(size = 0.75) + geom_line() +
    geom_ribbon(aes(ymin=lin_pred_0.025quant, ymax = lin_pred_0.975quant),
                linetype = 2, alpha=0.5, fill = color_band, colour = NA) + 
    theme_minimal() + labs(y = "Median and 95% CI (logit scale)", x = "", color ="") + 
    ylim(-2.5,2.1) +
    ggtitle(outcome_name) + 
    
  return(time_plot)
}

time_plot <- make_time_plot(inla_res = res_main, outcome_name = "Test Positivity", color_band = "#feb24c") 
ggsave("Supplementary_Figure3.png", time_plot, width = 10, height = 4)

# ==========================================================================
# 2. Spatial plot (logit scale)
# - Figure 1 in the manuscript
# ========================================================================== 

space_dis_plots <- function(inla_res, data, outcome_name, lims) {

  # keep track of overall spatial distribution
  tot_df <- data.frame(LTLA_ID = 1:n_LTLA)
  # covariate effect ("explained")
  exp_df <- data.frame(LTLA_ID = 1:n_LTLA)
  # spatial residual ("unexplained")
  une_df <- data.frame(LTLA_ID = 1:n_LTLA)
  
  # temporal random effect
  time_res <- inla_res$summary.random$date_ID

  # get spatial distributions by week
  for(n in 1:n_weeks){
    month_idx <- data %>% filter(date_ID ==n ) %>% summarise(as.integer(date_month)) %>% unique()
    month_idx <- month_idx$`as.integer(date_month)`
    
    # get data for this week
    plot_data <- data %>% filter(date_ID == n & age_class == "[35,55)") %>% 
      pivot_wider(names_from = rural_urban, values_from = rural_urban) %>%
      mutate(`rural_urbanPredominantly Urban` = !is.na(`Predominantly Urban`), 
             `rural_urbanUrban with Significant Rural` = !is.na(`Urban with Significant Rural`)) 
    
    # add row for Buckinghamshire
    tmp <- rep(NA, ncol(plot_data))
    #tmp <- plot_data[1,]
    plot_data <- rbind(plot_data, tmp)
    plot_data[1,'lad20cd'] <- "E06000060"
    plot_data[1, 'LTLA_ID'] <- 55

    prob_function <- function(x, inla_res_p) {
      # fixed effects - multiply coefficient means with covariate values
      log_scale_FE <-  inla_res_p$summary.fixed[c("(Intercept)",
                                                  "bame_black_stand",
                                                  "bame_southasian_stand",
                                                  "bame_other_stand", 
                                                  "IMD_stand",
                                                  "rural_urbanPredominantly Urban", 
                                                  "rural_urbanUrban with Significant Rural", 
                                                  "vax_prop_stand"), 1] %*% 
        c(0,(as.matrix(plot_data[x,c("bame_black_stand",
                                      "bame_southasian_stand",
                                      "bame_other_stand", 
                                      "IMD_stand",
                                      "rural_urbanPredominantly Urban", 
                                      "rural_urbanUrban with Significant Rural", 
                                      "vax_prop_stand")]))[1,]) 
      # random effects 
      log_scale_RE <-  c(inla_res_p$summary.random$date_month_Black[month_idx,"0.5quant"], 
                         inla_res_p$summary.random$date_month_SA[month_idx,"0.5quant"], 
                         inla_res_p$summary.random$date_month_Other[month_idx,"0.5quant"],
                         inla_res_p$summary.random$date_month_IMD[month_idx,"0.5quant"]) %*% 
        c((as.matrix(plot_data[x,c("bame_black_stand",
                                    "bame_southasian_stand",
                                    "bame_other_stand", 
                                    "IMD_stand")]))[1,]) 
      
      return(log_scale_FE + log_scale_RE)
    }
    
    explained <- sapply(1:nrow(plot_data), prob_function, inla_res_p = inla_res )
    
    start <- 1+(n-1)*n_LTLA
    end <- n*n_LTLA
    # space-time residuals
    res <- inla_res$summary.random$date_LTLA_ID$`0.5quant`[start:end]
    # spatial latent field
    space_lf <- inla_res$summary.random$LTLA_ID$`0.5quant`[1:n_LTLA]
    
    #res[res > 2] <- NA
    if (n < 10){
      tot_df[,paste0('week0', n)] <- res + space_lf + explained + time_res[n,"0.5quant"]
      exp_df[,paste0('week0', n)] <- explained
      une_df[,paste0('week0', n)] <- res + space_lf + time_res[n,"0.5quant"]
      
    } else{
      tot_df[,paste0('week', n)] <- res + space_lf + explained + time_res[n,"0.5quant"]
      exp_df[,paste0('week', n)] <- explained
      une_df[,paste0('week', n)] <- res + space_lf + time_res[n,"0.5quant"]
    }
    print(n)
  }
  
  # Average spatial distributions over time
  # !! NOTE: BELOW DEPENDS ON NUMBER OF WEEKS IN DATA !!
  LTLA_shp_Reg$total_pr <- apply(as.matrix(tot_df %>% dplyr::select( week01:week45 )), 1, mean, na.rm = T)
  LTLA_shp_Reg$exp_fraction <- apply(as.matrix(exp_df %>% dplyr::select( week01:week45 )), 1, mean, na.rm = T)
  LTLA_shp_Reg$unexp_fraction <- apply(as.matrix(une_df %>% dplyr::select( week01:week45 )), 1, mean, na.rm = T)
  
  #LTLA_shp_Reg %>% summarise(sd_tot = var(total_pr), sd_exp = var(exp_fraction), sd_unexp = var(unexp_fraction)) %>% print()
  
  spatial_plot1 <- ggplot(LTLA_shp_Reg) + geom_sf(aes(fill = total_pr), colour = NA) +
    labs(fill="") +
    # scale_fill_viridis_c(labels = function(x) paste0(x*100, "%")) +#  scale_fill_continuous() +
    theme_minimal() + ggtitle("Overall Spatial Distribution")
  
  spatial_plot2 <- ggplot(LTLA_shp_Reg) + geom_sf(aes(fill = exp_fraction), colour = NA) +
    labs(fill= "") +
    # scale_fill_viridis_c(labels = function(x) paste0(x*100, "%")) +#  scale_fill_continuous() +
    theme_minimal() + ggtitle("Covariate Effect")
  
  spatial_plot3 <- ggplot(LTLA_shp_Reg) + geom_sf(aes(fill = unexp_fraction), colour = NA) +
    labs(fill = "") +
    # scale_fill_viridis_c(labels = function(x) paste0(x*100, "%")) +#  scale_fill_continuous() +
    theme_minimal() + ggtitle("Spatial Residual")
  
  combined <- spatial_plot1 + spatial_plot2 + spatial_plot3 + 
    plot_layout(guides = "collect") & scale_fill_viridis_c(limits = lims) 
  
  combined <- combined +  plot_annotation(title = outcome_name)
  
  return(combined)
}

space_plot <- space_dis_plots(inla_res = res_main,  data = fin_data, outcome_name = "Test Positivity", lims = c(-2, 2))
ggsave(filename = 'Figure1.png', space_plot, width = 10, height = 4)


