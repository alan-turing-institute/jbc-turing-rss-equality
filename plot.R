library(INLA)
library(tidyr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(pals)
library(sf)

# ==========================================================================
# Set up
# ==========================================================================

# Number of LTLAs, weeks and months in analysis 
n_LTLA = 311
n_weeks = 45
n_months = 11

# Load results of the analyses
load("res_main.RData")
load("res_main2.RData")

# Load data
fin_data <- readRDS(file="toy_data.RDS")
# remove duplicate rows
fin_data <- fin_data %>% 
  group_by(LTLA_ID, date_ID, age_class) %>% 
  filter(row_number()==1) %>% ungroup()

# Load object LTLA_shp_Reg which links LTLA IDs to geography (for Figure 1)
# Note that we combine Buroughs of Hackney and City of London 
# Here we also exclude Isle of Wight and Isles of Scilly
load("space_obj.RData")

# ==========================================================================
# 1. Spatial plot (logit scale)
# - Figure 1 in the manuscript 
# ========================================================================== 

space_dis_plots <- function(inla_res, data, outcome_name, lims) {

  # Keep track of overall spatial distribution
  tot_df <- data.frame(LTLA_ID = 1:n_LTLA)
  # Covariate effect ("explained")
  exp_df <- data.frame(LTLA_ID = 1:n_LTLA)
  # Spatial residuals ("unexplained")
  une_df <- data.frame(LTLA_ID = 1:n_LTLA)
  
  # 1. Get spatial distributions by week
  
  ## Temporal random effect/residual
  time_res <- inla_res$summary.random$date_ID
  
  for(n in 1:n_weeks){
    ## Get the month that the week is in - need for random effects
    month_idx <- data %>% filter(date_ID ==n ) %>% summarise(as.integer(date_month)) %>% unique()
    month_idx <- month_idx$`as.integer(date_month)`

    ## Get data for this week
    plot_data <- data %>% filter(date_ID == n & age_class == "[35,55)") %>% 
      pivot_wider(names_from = rural_urban, values_from = rural_urban) %>%
      mutate(`rural_urbanPredominantly Urban` = !is.na(`Predominantly Urban`), 
             `rural_urbanUrban with Significant Rural` = !is.na(`Urban with Significant Rural`)) 
    
    ## Add row for Buckinghamshire - it's missing in the toy dataset
    tmp <- rep(NA, ncol(plot_data))
    plot_data <- rbind(plot_data, tmp)
    plot_data[1,'lad20cd'] <- "E06000060"
    plot_data[1, 'LTLA_ID'] <- 55

    ## Function to extract covariate effects
    prob_function <- function(x, inla_res_p) {
      ## fixed effects - multiply coefficients with covariate values
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
      ## random effects 
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
    ## Space-time residuals (one per week)
    res <- inla_res$summary.random$date_LTLA_ID$`0.5quant`[start:end]
    ## Overall spatial latent field (same for each week)
    space_lf <- inla_res$summary.random$LTLA_ID$`0.5quant`[1:n_LTLA]
    
    ## Sum it all together
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
  LTLA_shp_Reg$total_pr <- apply(as.matrix(tot_df %>% dplyr::select( week01:week45 )), 1, mean, na.rm = T)
  LTLA_shp_Reg$exp_fraction <- apply(as.matrix(exp_df %>% dplyr::select( week01:week45 )), 1, mean, na.rm = T)
  LTLA_shp_Reg$unexp_fraction <- apply(as.matrix(une_df %>% dplyr::select( week01:week45 )), 1, mean, na.rm = T)
  
  # Create the three spatial plots and combine into a single figure
  spatial_plot1 <- ggplot(LTLA_shp_Reg) + geom_sf(aes(fill = total_pr), colour = NA) +
    labs(fill="") +
    theme_minimal() + ggtitle("Overall Spatial Distribution")
  
  spatial_plot2 <- ggplot(LTLA_shp_Reg) + geom_sf(aes(fill = exp_fraction), colour = NA) +
    labs(fill= "") +
    theme_minimal() + ggtitle("Covariate Effect")
  
  spatial_plot3 <- ggplot(LTLA_shp_Reg) + geom_sf(aes(fill = unexp_fraction), colour = NA) +
    labs(fill = "") +
    theme_minimal() + ggtitle("Spatial Residual")
  
  combined <- spatial_plot1 + spatial_plot2 + spatial_plot3 + 
    plot_layout(guides = "collect") & scale_fill_viridis_c(limits = lims) 
  combined <- combined +  plot_annotation(title = outcome_name)
  
  return(combined)
}

space_plot <- space_dis_plots(inla_res = res_main,  data = fin_data, outcome_name = "Test Positivity", lims = c(-1.5, 1.5))

# NOTE: spatial plots take a while to generate
space_plot
#ggsave(filename = 'Figure1.png', space_plot, width = 10, height = 4)

# ==========================================================================
# 2. Outcome (average weekly test positivity) by IMD/BAME profile
# - Figure 2 in the manuscript
# ========================================================================== 

# The INLA analysis result object
inla_res <- res_main2

# Get values for low, medium and high BAME and IMD profiles
cov_df <- fin_data %>% filter(date_ID == 1 & age_class == '[35,55)')
cov_df_small <- cov_df %>% summarise(bame_stand = quantile(BAME_stand, probs = c(0.25, .5, .75)),
                                     IMD_stand = quantile(IMD_stand, probs = c(0.25, .5,  .75)))

# function to transform coefficients into proportions
prob_function <- function(x, y) {
  log_scale <-  inla_res$summary.fixed[c("(Intercept)","IMD_stand","BAME_stand"), "0.5quant"] %*% c(1, cov_df_small$IMD_stand[x], cov_df_small$bame_stand[y])
  return(boot::inv.logit(log_scale))
}

# get test positivity by BAME & IMD profile (1=low/2=medium/3=high)
val_df <- with(cov_df_small, data.frame(bame_stand = bame_stand[1], bame_idx = 1,
                                        IMD_stand = IMD_stand[1], IMD_idx = 1,
                                        prob = prob_function(1, 1)))
for(i in 1:3){
  for(j in 1:3)
    val_df <- rbind(val_df, with(cov_df_small ,data.frame(bame_stand = bame_stand[i], bame_idx = i,
                                                          IMD_stand = IMD_stand[j], IMD_idx = j,
                                                          prob = prob_function(i, j))))
}
val_df <- val_df %>% distinct()
val_df$bame_idx <- factor(val_df$bame_idx, levels = c(1, 2,3), labels = c("Low", "Medium", "High"))
val_df$IMD_idx <- factor(val_df$IMD_idx, levels = c(1, 2,3), labels = c("Low", "Medium", "High"))

# format text for tiles:
# - the outcome as percentage
# - in parenthesis the relative change between profile and the reference (low BAME & low IMD)
val_df$perc <- paste0(format(round(val_df$prob, 4) *100, nsmall = 2), 
                      "%\n(", 
                      format(round(val_df$prob/prob_function(1, 1), 2), nsmall = 2), 
                      ")")

# plot
tile_tp <- ggplot(val_df, aes(x = bame_idx, y = IMD_idx, fill = prob)) +
  geom_tile(aes(fill = prob)) +
  geom_text(aes(label = perc)) +
  scale_fill_viridis_c(option = "E",  na.value="transparent", alpha =.9, begin = 0.5, end = 0.7,direction = -1) +
  theme_minimal() + theme(legend.position = "none") + guides(y=  guide_axis(angle = 90)) +
  labs(x = "BAME", y = "IMD") +
  ggtitle("Test Positivity")

tile_tp

# ==========================================================================
# 3. Time varying coefficients (OR scale)
# - Figure 3 in the manuscript
# ==========================================================================

# The inla output object
inla_res <- res_main2

# Need below to create credible intervals

## 1. sample from the posterior
aa <- inla.posterior.sample(100, inla_res)

## 2. sum the coefficients of interest i.e., fixed and time varying:
### - fixed (Black_stand and IMD_stand) 
### - time varying (date_month and date_month_IMD)
aa_BAME <- inla.posterior.sample.eval(function(xx) date_month + BAME_stand, aa)
aa_IMD <- inla.posterior.sample.eval(function(xx) date_month_IMD + IMD_stand, aa)

## 3. save
save(aa_BAME, aa_IMD, file = "AA_uncertainty.RData")

# Get the the temporal random effect by month 
time_res <- inla_res$summary.random$date_ID
time_res$weeks <- seq(1, n_weeks)

## Get the month that each week is in 
months <- list()
for(n in 1:n_weeks){
  month_idx <- fin_data %>% filter(date_ID ==n ) %>% summarise(as.integer(date_month)) %>% unique()
  month_idx <- month_idx$`as.integer(date_month)`
  months[n] <- month_idx
}
time_res$months_idx <- months

## Average over weeks
time_res2 <- time_res %>%group_by(months_idx) %>% summarise(mean = mean(mean))

# Get space-time interactions by month
inter_df <- data.frame(LTLA_ID = 1:n_LTLA)
for(nn in 1:n_weeks){
  start <- 1+(nn-1)*n_LTLA
  end <- nn*n_LTLA
  res <- inla_res$summary.random$date_LTLA_ID$`0.5quant`[start:end]
  #res[res > 2] <- NA
  if (nn < 10){
    inter_df[,paste0('week0', nn)] <- res
  } else{
    inter_df[,paste0('week', nn)] <- res
  }
}

## Average over LTLAs and weeks to get mean by month 
time_inter_df <- inter_df %>% gather('week', 'val', week01:week45)
time_inter_df$months_idx <- time_res$months_idx
time_res3 <- time_inter_df %>% group_by(months_idx) %>% summarise(mean= mean(val, na.rm = T))

# Sum fixed and time-varying
## For jth posterior sample and xx-th BAME profile level and yy-th IMD profile level (1/2/3 for low/medium/high)
comp_linpred <- function(j, xx, yy){
  
  tv_coeff <- inla_res$summary.random$date_month[, c("0.5quant", "0.025quant", "0.975quant")]
  
  for (n_n in 1:(n_months-1)){
    
    tv_coeff[n_n,1] <-  e1071::sigmoid(   inla_res$summary.fixed['(Intercept)','0.5quant' ] +
                                            aa_BAME[n_n,j] * cov_df_small[xx, "bame_stand"] +
                                            aa_IMD[n_n,j] * cov_df_small[yy, "IMD_stand"] +
                                            time_res2[n_n, 2]$mean + time_res3$mean[n_n] )[1,1]
    
    tv_coeff$var <- seq(1,n_months-1)
    tv_coeff$level_bame <- xx
    tv_coeff$level_imd <- yy
    tv_coeff$sim <- j
  }
  
  return(tv_coeff)
}

#comp_linpred(1,1,2)

# low BAME, low IMD
tv_coeff1.1 <-  lapply(1:100, comp_linpred, xx = 1, yy = 1)
# lowe BAME, high IMD
tv_coeff1.3 <-  lapply(1:100, comp_linpred, xx = 1, yy = 3)
# high BAME, low IMD
tv_coeff3.1 <-  lapply(1:100, comp_linpred, xx = 3, yy = 1)
# high BAME, high IMD
tv_coeff3.3 <-  lapply(1:100, comp_linpred, xx = 3, yy = 3)

tv_coeff1.1 <- do.call("rbind", tv_coeff1.1)
# Get the relative change between profile and reference (low BAME, low IMD)
tv_coeff1.1$RR <- tv_coeff1.1$`0.5quant`/tv_coeff1.1$`0.5quant`

tv_coeff1.3 <- do.call("rbind", tv_coeff1.3)
# Get the relative change between profile and reference (low BAME, low IMD)
tv_coeff1.3$RR <- tv_coeff1.3$`0.5quant`/tv_coeff1.1$`0.5quant`

tv_coeff3.1 <- do.call("rbind", tv_coeff3.1)
# Get the relative change between profile and reference (low BAME, low IMD)
tv_coeff3.1$RR <- tv_coeff3.1$`0.5quant`/tv_coeff1.1$`0.5quant`

tv_coeff3.3 <- do.call("rbind", tv_coeff3.3)
# Get the relative change between profile and reference (low BAME, low IMD)
tv_coeff3.3$RR <- tv_coeff3.3$`0.5quant`/tv_coeff1.1$`0.5quant`

pal <- c("red", brewer.seqseq1()[c(3, 7, 9)])

# Combine all outputs and get summaries
df_fin <- rbind(tv_coeff1.1, tv_coeff1.3, tv_coeff3.1, tv_coeff3.3)
df_fin <- df_fin %>% group_by(var, level_bame, level_imd) %>% summarise(q2 = median(RR),
                                                                        q1 = quantile(RR, probs = 0.025),
                                                                        q3 = quantile(RR, probs = 0.975) )
df_fin$level <- interaction(as.factor(df_fin$level_bame), as.factor(df_fin$level_imd))

levels(df_fin$level) <- c("Low BAME - Low IMD", "High BAME - Low IMD",
                          "Low BAME - High IMD", "High BAME - High IMD")
#table(df_fin$level)

test_posRR <- ggplot(df_fin, aes(x = var, y = q2,  color = level, fill = level)) + geom_point( )+ geom_line(size = .61, alpha = 0.71) +
  geom_ribbon(aes(ymin=q1, ymax = q3, fill = level),
              colour = NA, alpha=0.3) + scale_color_manual(values = pal) + scale_fill_manual(values = pal) +
  theme_minimal() + labs(y = "Median and 95% CI (OR scale)", x = "", color ="", fill= "") +
  theme(legend.position = "bottom") 

test_posRR

# ==========================================================================
# 4. Temporal trend (logit scale)
# - Supplementary Figure 3 
# ========================================================================== 

make_time_plot <- function(inla_res, outcome_name, color_band) { 
  
  # temporal random effects
  time_res <- inla_res$summary.random$date_ID
  time_res$week <- seq(1, n_weeks)
  
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
    ggtitle(outcome_name)  
  
  return(time_plot)
}

time_plot <- make_time_plot(inla_res = res_main, outcome_name = "Test Positivity", color_band = "#feb24c") 

time_plot
#ggsave("Supplementary_Figure3.png", time_plot, width = 10, height = 4)

# ==========================================================================
# 4.Fixed effects
# - Supplementary Figure 5
# ========================================================================== 
