################################################################################
## Title:         Corporate Partners 2022 Poster Analysis
## Author:        Adeline Guthrie
## Project:       PPC
## Date Created:  10/04/2022
## Description:   This script loads Hockey Pundit data and reproduces all analyses 
##                from my Corporate Partners 2022 Poster.  This included creation 
##                of plots.
##
## Note: 
##  This file relies on:
##  - The hockey pundit data found in "compiled_NHL_pundit_data20_21_FULL.csv"
##  - "LLO_function.R" which contains all functions for performing our methodology,
##    this cannot be added to github until paper is complete and submitted
################################################################################

# Packages needed
library(ggplot2)
library(gridExtra)
library(kableExtra)
library(dplyr)

# Setting base font sizes for plots
base <- 28
axis_title <- 30
axis_labs <- 25

# Base Z plot
Zplot <- function(x, y, int = NA, slp = NA, color = "black", ttle =""){
  if(is.na(int) | is.na(slp)){
    plot1 <- ggplot(mapping = aes(x, y)) +
      geom_point(col = color, alpha = 0.25, size = 2,
                 position = position_jitter(seed = 1, height = 0.05)) +
      geom_abline(col = "gray38", lty = 2) +
      xlim(c(0,1)) +
      theme_bw() +
      ggtitle(ttle)
  }else{
    plot1 <- ggplot(mapping = aes(x, y)) +
      geom_point(col = color, alpha = 0.25, size = 2,
                 position = position_jitter(seed = 1, height = 0.05)) +
      geom_abline(intercept = int, slope = slp, col = color, size = 1) +
      geom_abline(col = "gray38", lty = 2) +
      xlim(c(0,1)) +
      theme_bw() +
      ggtitle(ttle)
  }
  return(plot1)
}

# Load all functions pertaining to research methodology
# (Cannot upload LLO_functions.R to github until paper is submitted) 
source("../../LLO_functions.R") 

# Load compiled hockey data
pundit_data <- read.csv("../../hockey/data/compiled_NHL_pundit_data20_21_FULL.csv", 
                        row.names=1, stringsAsFactors=TRUE)

# Game outcomes - "Win" indicated a home team win
outcomes <- recode_factor(pundit_data$Winner, home = "Win", away = "Loss")
y_home <- pundit_data$Winner01
n <- length(y_home)

# Hockey-Statistics.com home team win probs
x_homeHS <- pundit_data$HomeProbHS

# fivethirtyeight.com home team win probs
x_home538 <- pundit_data$HomeProb538

# "Random" Pundit 
set.seed(8333)
x_rand <- runif(n = length(y_home), min = min(x_homeHS, x_home538), max = max(x_homeHS, x_home538))

#####################################################################
#                          Testing Results                          #
#####################################################################
# Testing consisted of:
# - Likelihood ratio test for calibration
# - Bayesian testing via BIC approx
# - Recalibration via MLEs and LLO function
# - Checking that new MLEs for delta = gamma = 1 (i.e. adjusted probs are calibrated)


#########################
# Hockey-Statistics.com #
#########################

# Likelihood ratio test
LRT_HS <- LLO_LRT(x = x_homeHS, y = y_home) # delta = 1.0165661, gamma = 0.9575038

# Delta, gamma MLEs
delta_HS <- LRT_HS$est_params[1]
gamma_HS <- LRT_HS$est_params[2]

# Bayesian Testing via BIC approximation of Bayes Factor
bayes_HS <- bayes_testing(x = x_homeHS, y = y_home)

# Recalibrate Probs
x_homeHS_new <- LLO(x_homeHS, delta_HS, gamma_HS)

# Running testing again to see if MLEs for delta = gamma = 1
LRT_HS_new <- LLO_LRT(x_homeHS_new, y_home)
bayes_HS_new <- bayes_testing(x_homeHS_new, y_home)

#######################
# fivethirtyeight.com #
#######################

# Likelihood ratio test
LRT_538 <- LLO_LRT(x = x_home538, y = y_home) # delta = 1.0165661, gamma = 0.9575038

# Delta, gamma MLEs
delta_538 <- LRT_538$est_params[1]
gamma_538 <- LRT_538$est_params[2]

# Bayesian Testing via BIC approximation of Bayes Factor
bayes_538 <- bayes_testing(x = x_home538, y = y_home)

# Recalibrate Probs
x_home538_new <- LLO(x_home538, delta_538, gamma_538)

# Running testing again to see if MLEs for delta = gamma = 1
LRT_538_new <- LLO_LRT(x_home538_new, y_home)
bayes_538_new <- bayes_testing(x_home538_new, y_home)

################
# Random Noise #
################

# Likelihood ratio test
LRT_rand <- LLO_LRT(x = x_rand, y = y_home) # delta = 1.0165661, gamma = 0.9575038

# Delta, gamma MLEs
delta_rand <- LRT_rand$est_params[1]
gamma_rand <- LRT_rand$est_params[2]

# Bayesian Testing via BIC approximation of Bayes Factor
bayes_rand <- bayes_testing(x = x_rand, y = y_home)

# Recalibrate Probs
x_rand_new <- LLO(x_rand, delta_rand, gamma_rand)

# Running testing again to see if MLEs for delta = gamma = 1
LRT_rand_new <- LLO_LRT(x_rand_new, y_home)
bayes_rand_new <- bayes_testing(x_rand_new, y_home)


res_table <- data.frame(Pundit = c("Hockey-Statistics.com", "Fivethirteight.com", "Random Noise"),
                        `LRT p-value` = round(c(LRT_HS$pval, LRT_538$pval, LRT_rand$pval), 4),
                        `Bayesian Posterior Model Probability` = round(c(bayes_HS$posterior_model_prob, 
                                                                         bayes_538$posterior_model_prob, 
                                                                         bayes_rand$posterior_model_prob), 4),
                        Delta = round(c(delta_HS, delta_538, delta_rand), 4),
                        Gamma = round(c(gamma_HS, gamma_538, gamma_rand), 4),
                        `Delta After Recalib` = round(c(LRT_HS_new$est_params[1], 
                                                        LRT_538_new$est_params[1], 
                                                        LRT_rand_new$est_params[1]), 4),
                        `Gamma After Recalib` = round(c(LRT_HS_new$est_params[2], 
                                                        LRT_538_new$est_params[2], 
                                                        LRT_rand_new$est_params[2]), 4))

res_table

#####################################################################
#                               Plots                               #
#####################################################################
# Note: These plots have been tailored for use on the poster, thus the font sizes 
#       are much larger than needed to view them within R-studio.  Also, labels 
#       for these plots were added manually when creating the poster for easy
#       adjustment. 


#############################
# Recalibration Curves Plot #
#############################
# - These plots show the recalibration curve (i.e. via the linear log odds function)
#   with the pundit MLEs for delta and gamma 
# - Dashed diagonal line represents perfect calibration, i.e. delta = 1, gamma = 1
# - Dots are the actual pundit probs

# Hockey-Statistics.com
rc_HS <- ggplot(mapping = aes(x = x_homeHS, y = x_homeHS_new)) +
  stat_function(aes(x = NULL, y = NULL),
                fun = LLO,
                args = list(delta = delta_HS,
                            gamma = gamma_HS),
                geom = "line",
                xlim = c(0,1),
                color = "blue",
                size = 1)+
  geom_point(size = 2, alpha = 0.15) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  xlim(c(0,1)) +
  # ylim(c(0,1)) +
  scale_y_continuous(breaks = seq(0,1,by=0.2)) +
  # xlab("Original Probabilities") +
  # ylab("Recalibrated Probabilities") +
  # ggtitle("Hockey-Statistics.com") + 
  theme_bw(base_size = base) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Fivethirtyeight.com
rc_538 <- ggplot(mapping = aes(x = x_home538, y = x_home538_new)) +
  stat_function(aes(x = NULL, y = NULL),
                fun = LLO,
                args = list(delta = delta_538,
                            gamma = gamma_538),
                geom = "line",
                xlim = c(0,1),
                color = "blue",
                size = 1)+
  geom_point(size = 2, alpha = 0.15) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  xlim(c(0,1)) +
  # ylim(c(0,1)) +
  scale_y_continuous(breaks = seq(0,1,by=0.2)) +
  # xlab("Original Probabilities") +
  # ylab("Recalibrated Probabilities") +
  # ggtitle("fivethirtyeight.com") + 
  theme_bw(base_size = base) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank())

# Random Noise
rc_rand <- ggplot(mapping = aes(x = x_rand, y = x_rand_new)) +
  stat_function(aes(x = NULL, y = NULL),
                fun = LLO,
                args = list(delta = delta_rand,
                            gamma = gamma_rand),
                geom = "line",
                xlim = c(0,1),
                color = "blue",
                size = 1)+
  geom_point(size = 2, alpha = 0.15) +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  xlim(c(0,1)) +
  # ylim(c(0,1)) +
  scale_y_continuous(breaks = seq(0,1,by=0.2)) +
  # xlab("Original Probabilities") +
  # ylab("Recalibrated Probabilities") +
  # ggtitle("Random Noise") + 
  theme_bw(base_size = base) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 


# Arrange in 3 side-by-side panels
rc <- arrangeGrob(rc_HS, rc_538, rc_rand, ncol = 3, nrow=1)

# Save plot to current working directory
ggsave("hockey_curves.png", rc, units = "in", height = 4.63, width = 14.75)

##############
# Line Plots #
##############
# - These plots show how the pundit probs are adjusted through recalibration
# - Points on the left-hand side of each panel represent the original probs
# - Points on the right-hand side of each panel represent the re-calibrated probs
# - Line connect "before" and "after" observations 
# - Color represent the outcome of the game: blue = home team win, red = home team loss
# - We expect "good" pundits to have more "blue" points between 0.5 and 1 (i.e. higher predicted probs)
#   and more "red" points between 0 and 0.5 (i.e. lower predicted probs)



# Hockey-statistics.com

# Round of posterior model probs for axis labels
post_HS_r <- round(bayes_HS$posterior_model_prob, digits = 4)
post_HS_new_r <- round(bayes_HS_new$posterior_model_prob, digits = 4)

# Create new data frame with summarized/rounded data for plotting simplicity
df_HS <- data.frame(probs = c(x_homeHS, x_homeHS_new), 
                    outcome = factor(c(outcomes, outcomes), levels = c("Win","Loss")), 
                    post = factor(c(rep(as.character(post_HS_r), n), 
                                    rep(as.character(post_HS_new_r), n)), 
                                  levels = c(as.character(post_HS_r), 
                                             as.character(post_HS_new_r))),
                    pairing = factor(c(seq(1, n), seq(1, n))))

lines_HS <- ggplot(data = df_HS, mapping = aes(x = post, y = probs)) +
  geom_point(aes(color = outcome), alpha = 0.35, size = 2,
             show.legend = FALSE) +
  geom_line(aes(group=pairing, color = outcome), size = 1, alpha = 0.25,
            show.legend = FALSE) +
  # labs(x = "Posterior Model Prob.",
  #      y = "Probability Home Team Wins") +
  theme_bw(base_size = base) +
  scale_y_continuous(breaks = seq(0,1,by=0.2),
                     limits = c(0,1),
                     expand = c(0, 0))+
  scale_color_manual(values = c("blue", "red")) +
  scale_x_discrete(expand = c(0, 0.075)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 



# fivethirtyeight.com

# Round of posterior model probs for axis labels
post_538_r <- round(bayes_538$posterior_model_prob, digits = 4)
post_538_new_r <- round(bayes_538_new$posterior_model_prob, digits = 4)

# Create new data frame with summarized/rounded data for plotting simplicity
df_538 <- data.frame(probs = c(x_home538, x_home538_new), 
                     outcome = factor(c(outcomes, outcomes), levels = c("Win","Loss")), 
                     post = factor(c(rep(as.character(post_538_r), n), rep(as.character(post_538_new_r), n)), 
                                   levels = c(as.character(post_538_r), as.character(post_538_new_r))),
                     pairing = factor(c(seq(1, n), seq(1, n))))

lines_538 <- ggplot(data = df_538, mapping = aes(x = post, y = probs)) +
  geom_point(aes(color = outcome), alpha = 0.35, size = 2,
             show.legend = FALSE) +
  geom_line(aes(group=pairing, color = outcome), size = 1, alpha = 0.25,
            show.legend = FALSE) +
  # labs(x = "Posterior Model Prob.",
  #      y = "Probability Home Team Wins") +
  theme_bw(base_size = base) +
  scale_y_continuous(breaks = seq(0,1,by=0.2),
                     limits = c(0,1),
                     expand = c(0, 0))+
  scale_color_manual(values = c("blue", "red")) +
  scale_x_discrete(expand = c(0, 0.075)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 


# Random Noise

# Round of posterior model probs for axis labels
post_rand_r <- "0.0000"
post_rand_new_r <- round(bayes_rand_new$posterior_model_prob, digits = 4)

# Create new data frame with summarized/rounded data for plotting simplicity
df_rand <- data.frame(probs = c(x_rand, x_rand_new), 
                      outcome = factor(c(outcomes, outcomes), levels = c("Win","Loss")), 
                      post = factor(c(rep(as.character(post_rand_r), n), rep(as.character(post_rand_new_r), n)), 
                                    levels = c(as.character(post_rand_r), as.character(post_rand_new_r))),
                      pairing = factor(c(seq(1, n), seq(1, n))))

lines_rand <- ggplot(data = df_rand, mapping = aes(x = post, y = probs)) +
  geom_point(aes(color = outcome), alpha = 0.35, size = 2,
             show.legend = FALSE) +
  geom_line(aes(group=pairing, color = outcome), size = 1, alpha = 0.25,
            show.legend = FALSE) +
  # labs(x = "Posterior Model Prob.",
  #      y = "Probability Home Team Wins") +
  theme_bw(base_size = base) +
  scale_y_continuous(breaks = seq(0,1,by=0.2),
                     limits = c(0,1),
                     expand = c(0, 0))+
  scale_color_manual(values = c("blue", "red")) +
  scale_x_discrete(expand = c(0, 0.075)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) 

# Arrange plots in three side-by-side panels
p <- arrangeGrob(lines_HS, lines_538, lines_rand, ncol = 3, nrow=1)

# Save plot to current working directory
ggsave("hockey_lines.png", p, units = "in", height = 6, width = 14.75)


####################
# Calibration plot #
####################
# - This plot shows an example of perfect calibration by comparing the predicted 
#   probability of an event and the relative frequency in which that event occurs 
#   given that the corresponded prediction was made (i.e. events that are assigned 
#   40% probability occur 40% of the time)

cal <- ggplot() +
  geom_point(mapping = aes(x = c(0.0, 0.2, 0.4, 0.6, 0.8, 1), 
                           y = c(0.0, 0.2, 0.4, 0.6, 0.8, 1)),
             size = 3) +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = .65) +
  scale_x_continuous(breaks = seq(0,1,by=0.2),
                     limits = c(0,1), 
                     expand = c(0,0.02)) +
  scale_y_continuous(breaks = seq(0,1,by=0.2),
                     limits = c(0,1), 
                     expand = c(0,0.02)) +
  theme_bw() +
  labs(x = "Forecast",
       y = "Percent Observed") +
  theme(axis.title.x = element_text(size = 30),
        axis.title.y = element_text(size = 30),
        axis.text.x = element_text(size = 26),
        axis.text.y = element_text(size = 26))

# Saves the plot to the current working directory
ggsave("calib_plot.png", cal, units = "in", width = 5, height = 5)

################################
# Recalibration function plots #
################################
# - These plots show examples of some shapes the LLO recalibration function can
#   take on depending on the different values of delta and gamma
# - The left panel fixes delta at 0.5 and shows the curves for gamma = 0.1, 0.25,
#   0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, and 2.5.
# - The right panel fixes gamma at 0.5 and shows the curves for delta = 0.1, 0.25,
#   0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, and 2.5.
# - Dashed red line represents perfect calibration, delta = gamma = 1

# Fix delta at 0.5
del <- 0.5
del_fix <- ggplot()+
  stat_function(fun = LLO,
                args = list(delta = del, gamma = 0.1),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = del, gamma = 0.25),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = del, gamma = 0.5),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = del, gamma = 0.75),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = del, gamma = 1),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = del, gamma = 1.25),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = del, gamma = 1.5),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = del, gamma = 1.75),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = del, gamma = 2),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = del, gamma = 2.5),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = .65, color = "red") +
  scale_x_continuous(breaks = seq(0,1,by=0.2),
                     limits = c(0,1), 
                     expand = c(0,0.02)) +
  scale_y_continuous(breaks = seq(0,1,by=0.2),
                     limits = c(0,1), 
                     expand = c(0,0.02)) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24))

# Fix Gamma at 0.5
gam <- 0.5
gam_fix <- ggplot()+
  stat_function(fun = LLO,
                args = list(delta = 0.1, gamma = gam),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = 0.25, gamma = gam),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = 0.5, gamma = gam),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = 0.75, gamma = gam),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = 1, gamma = gam),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = 1.25, gamma = gam),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = 1.5, gamma = gam),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = 1.75, gamma = gam),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = 2, gamma = gam),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  stat_function(fun = LLO,
                args = list(delta = 2.5, gamma = gam),
                geom = "line",
                size = 1,
                color = "black",
                xlim = c(0, 1)) +
  geom_abline(slope = 1, intercept = 0, lty = 2, size = .65, color = "red") +
  theme_bw()+
  scale_x_continuous(breaks = seq(0,1,by=0.2),
                     limits = c(0,1), 
                     expand = c(0,0.02)) +
  scale_y_continuous(breaks = seq(0,1,by=0.2),
                     limits = c(0,1), 
                     expand = c(0,0.02)) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 24),
        axis.text.y = element_text(size = 24))

# Arrange plots side-by-side
p <- arrangeGrob(del_fix, gam_fix, ncol = 2)

# Save plot to current working directory
ggsave("recalib_curves.png", p, units = "in", width = 9, height = 4.5)


