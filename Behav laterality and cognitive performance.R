
# load libraries
library(brms)
library(dplyr)
library(psych)
library(ggpubr)
library(ggplot2)
library(emmeans)
library(performance)
# library(stringdist)
# library(RColorBrewer)
# library(readxl)
 library(tidyr)
# library(posterior)
# library(tibble)




# 1.1) Read data ----------------------------------------------------------

laterality_data <- read.csv("Data/combined.data.csv")

# this contains how many times each individual was trapped during summer 2024 and how often they were registered on the puzzle boxes
num.visits <- read.csv("Data//num.visits.csv", row.names=1)

trap.behav <- read.csv("Data//trap.behav.csv", row.names = 1)

# 1.2) Calculate behavioral type ------------------------------------------

# we want to calculate a score for behaivoral reactivity to humans in the trap
# extract the proportion of trapping occasiona where individuals showed these behaviors

trap.behav.sum <- aggregate(. ~ subject, data=trap.behav, sum)

# add trappabilty
trap.behav.sum <- merge(trap.behav.sum, num.visits, by="subject")

# divide by number of trapping events
trap.behav.sum[,c(2:4)] <- trap.behav.sum[,c(2:4)]/trap.behav.sum$num_trapped

# run factor analysis
fa <- fa(trap.behav.sum[,c(2:4)], nfactors = 1)

fa$loadings

# Loadings:
#   MR1   
# beh_chat  0.999
# beh_call       
# beh_strg -0.181
# 
# MR1
# SS loadings    1.035
# Proportion Var 0.345

# individual scores
fa$scores
scores <- cbind.data.frame(fa$scores, trap.behav.sum$subject)
colnames(scores) <- c("loading", "subject")

# put into num.trap data frame
num.visits <- merge(num.visits, scores, by="subject")

colnames(num.visits) <- c("Subject","num_visits", "num_trapped", "behav")


# 1.3) Extract some numbers -----------------------------------------------

# how often did the use which paw
table(laterality_data$body_part)

# both paws  left paw right paw 
# 766       605       584 

# how many unique squirrels
length(unique(laterality_data$Subject))
# 20

# how many times did they solve?
table(laterality_data$Subject)


# squirrel 1 squirrel 10 squirrel 11 squirrel 12 squirrel 13 squirrel 14 squirrel 15 squirrel 16 squirrel 17 squirrel 18 squirrel 19 
# 86          28          66         134          15         547         119           9          10          68          97 
# squirrel 2 squirrel 20  squirrel 3  squirrel 4  squirrel 5  squirrel 6  squirrel 7  squirrel 8  squirrel 9 
# 43          10          33          13          82         493          73          16          13 

mean(table(laterality_data$Subject))
97.75

range(table(laterality_data$Subject))

# 2) Handedness - bernoulli mixed effect model comparing left to right paw use only ------------------------------------


# 2.1) Run model ----------------------------------------------------------


model_handedness <- brm(  
  body_part ~ lever_side +
    (1 + lever_side | Subject), 
  data = laterality_data[laterality_data$body_part!="both paws",], # we exclude both paws from this data set
  family = bernoulli(),
  cores = 4,
  iter = 4000,
  chains = 4,
  seed = 3,
  control = list(adapt_delta = 0.99)
)

#save(model_handedness, file="model output/model_handedness.RDA")
load("model output/model_handedness.RDA")

# let's look at the model output
summary(model_handedness)


# Family: bernoulli 
# Links: mu = logit 
# Formula: body_part ~ lever_side + (1 + lever_side | Subject) 
# Data: laterality_data[laterality_data$body_part != "both (Number of observations: 1189) 
#   Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
#          total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
# ~Subject (Number of levels: 20) 
#                                     Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)                           0.36      0.20     0.03     0.83 1.00     1478     1603
# sd(lever_siderightlever)                1.10      0.34     0.56     1.92 1.00     2300     4157
# cor(Intercept,lever_siderightlever)    -0.33      0.39    -0.89     0.60 1.00     1185     1640
# 
# Regression Coefficients:
#                      Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept               -1.14      0.17    -1.53    -0.85 1.00     2914     4364
# lever_siderightlever     2.65      0.37     1.96     3.43 1.00     3174     3964
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).



# how to interpret:
# fixed effects:
# intercept of -1.14 is the log odds of using the right paw when the lever side is left. This corresponds to a probability of about 0.25 for using the right paw on the left lever.
# lever_siderightlever is how the log odds change when squirrels are using the right instead of the left lever. so the log odds of using the right paw increase by 2.65 relative to the left lever. 

# random effects:
# sd(Intercept) of 0.36. Some animals prefer one paw over the other
# sd(lever_siderightlever): 	There’s substantial variation among animals in how much they are influenced by lever side. Some animals may show stronger or weaker lever-related effects.
# cor(Intercept,lever_siderightlever): no evidence for correlation between baseline paw preference and the lever side effect


# extract odds by exponentiating:
round(exp(fixef(model_handedness)),2)

# Estimate Est.Error Q2.5 Q97.5
# Intercept                0.32      1.19 0.22  0.43
# lever_siderightlever    14.10      1.45 7.11 30.91


# get probabilities
plogis(fixef(model_handedness))

# Estimate Est.Error      Q2.5     Q97.5
# Intercept            0.2423548 0.5427066 0.1780327 0.2988367
# lever_siderightlever 0.9337884 0.5915081 0.8766662 0.9686638

# alternatively, we could do left paw on left lever by:

1-0.2423548 
# [1] 0.7576452 probability of using left paw on left lever
# CIs
1-0.1780327
# 0.8219673

1-0.2988367
# 0.7011633

# lever_siderightlever is the probability of the right paw on the right lever


# 2.2) Model check and plot -----------------------------------------------

# visual inspection of model fit
pp_check(model_handedness)
# looks like a good fit

# check for stationarity and mixing
plot(model_handedness)

# conditional effects plot
cond.plot <- plot(conditional_effects(model_handedness))


fig_lever_side <- cond.plot$lever_side+
  labs(x= "Lever side", y= "Probability of using right paw")+
  ylim(c(0,1))+
  theme_bw()
  

# we save this figure for further down


# 2.3) Predict from model to estimate handedness -------------------------------------------------

# Get unique subjects and create newdata
newdata_handedness <- expand.grid(
  Subject = unique(model_handedness$data$Subject),
  lever_side = c("left lever", "right lever")
)

# Predict expected probability of right paw use
posterior_handedness <- posterior_epred(model_handedness, newdata = newdata_handedness, re_formula = NULL)

# this gives us predicted probabilities of using left and right paw for each lever side for each individual

n_draws <- nrow(posterior_handedness)
n_rows <- ncol(posterior_handedness)

# Create a lookup table for which row corresponds to which Subject and lever_side
row_lookup <- newdata_handedness
row_lookup$row <- 1:nrow(row_lookup)

# Convert posterior to long format
long_data <- data.frame(
  draw = rep(1:n_draws, times = n_rows),
  cell = rep(1:n_rows, each = n_draws),
  p_right = as.vector(posterior_handedness)
)

# Merge with newdata info
long_data$Subject <- row_lookup$Subject[long_data$cell]
long_data$lever_side <- row_lookup$lever_side[long_data$cell]

# Initialize list to store handedness results
subjects_list <- unique(long_data$Subject)
hi_draws <- data.frame()

for (subj in subjects_list) {
  subj_data <- long_data[long_data$Subject == subj, ]
  
  # Get left and right separately
  right_data <- subj_data[subj_data$lever_side == "right lever", ]
  left_data  <- subj_data[subj_data$lever_side == "left lever", ]
  
  # Make sure we have both
  if (nrow(right_data) == nrow(left_data)) {
    R <- right_data$p_right
    L <- 1 - left_data$p_right
    HI <- ifelse((R + L) > 0, (R - L) / (R + L), NA)
    
    hi_subj <- data.frame(
      draw = right_data$draw,
      Subject = subj,
      HI = HI
    )
    hi_draws <- rbind(hi_draws, hi_subj)
  }
}

# summarize per subject
subjects_summary <- unique(hi_draws$Subject)
summary_list <- data.frame()

for (subj in subjects_summary) {
  hi_vals <- hi_draws$HI[hi_draws$Subject == subj]
  hi_vals <- hi_vals[!is.na(hi_vals)]
  
  if (length(hi_vals) > 0) {
    mean_HI <- mean(hi_vals)
    lower <- quantile(hi_vals, 0.025)
    upper <- quantile(hi_vals, 0.975)
    
    summary_list <- rbind(summary_list, data.frame(
      Subject = subj,
      mean_HI = mean_HI,
      lower = lower,
      upper = upper
    ))
  }
}

# summary list now contains the handedness index with upper and lower CI for all squirrels that have used both left and right paws

# how many squirrels?

length(summary_list$Subject)
# 20


# what is the population-level HI
# hi_draws has columns: draw, Subject, HI

# Step 1: Remove NAs
hi_clean <- hi_draws[!is.na(hi_draws$HI), ]

# Step 2: Compute population HI per draw
# For each draw, take the mean across subjects
pop_hi_per_draw <- tapply(hi_clean$HI, hi_clean$draw, mean)

# Step 3: Summarize
pop_mean_HI <- mean(pop_hi_per_draw)
# [1] 0.01208529
pop_median_HI <- median(pop_hi_per_draw)
# [1]  0.0134386
pop_CI <- quantile(pop_hi_per_draw, c(0.025, 0.975))
# 2.5%       97.5% 
#   -0.04811638  0.06241634 

# plot:

# Ensure the data is sorted (optional)
summary_list <- summary_list[order(summary_list$mean_HI),]


# Add factor levels to preserve subject order in the plot
summary_list$Subject <- factor(summary_list$Subject, levels = summary_list$Subject)

# for plotting, we assign an ordered number 
summary_list$Subject2 <- paste("squirrel", seq(1,20, by=1), sep=" ")
summary_list$Subject2 <- factor(summary_list$Subject2, levels = summary_list$Subject2)

# plot
HI_plot <- ggplot(summary_list, aes(x=mean_HI, y=Subject2))+
  geom_rect(aes(xmin = -0.2, xmax = 0.2, ymin = -Inf, ymax = Inf),
  fill = "grey80", alpha = 0.2, inherit.aes = FALSE)+
  geom_point()+
  geom_errorbarh(aes(xmin = lower, xmax = upper), height = 0.3)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  geom_vline(xintercept = c(-0.2, 0.2), linetype = "dashed", color = "grey10")+
  labs(x = "Handedness Index", y = "Subject") +
  theme_bw()+
  scale_x_continuous(limits = c(-1, 1), expand = c(0,0))
  
# combine the two plots and save

ggarrange( HI_plot, fig_lever_side, ncol=2, nrow=1, labels = c("a", "b"))

ggsave("Figures output/Figure2.tiff", units="in", width=9, height=5, dpi=300, compression = 'lzw')



# 3) Both versus single paw use -------------------------------------------

# 

# 3.1) Data prep ----------------------------------------------------------

# we add behavioral type, cognitive performance and trappability to the data frame
# first extract the number of solves per individual
num.solves <- as.data.frame(table(laterality_data$Subject))
colnames(num.solves) <- c("Subject", "num.solves")

# add it to the num.visits data frame
num.vists <- merge(num.visits, num.solves, by="Subject")

# calcualte cogntiive performance as number of solves per number of visits (solving rate)
num.visits$solving.rate <- num.vists$num.solves/num.vists$num_visits

# merge into the laterality data frame

laterality_data <- merge(laterality_data, num.visits, by="Subject")

# we use single as the baseline level
laterality_data$body_part_red <- as.factor(laterality_data$body_part_red)
laterality_data$body_part_red <- relevel(laterality_data$body_part_red, ref = "one paw")


# calculate VIFs (variance inflation factors)

check_collinearity(
  lm(rep(1, nrow(laterality_data)) ~ age + reaction + log_cumulative_count + solving.rate + behav + num_trapped,
     data = laterality_data)
)


# Check for Multicollinearity

# Low Correlation
# 
# Term  VIF   VIF 95% CI Increased SE Tolerance Tolerance 95% CI
# age 1.65 [1.56, 1.76]         1.29      0.61     [0.57, 0.64]
# reaction 2.98 [2.78, 3.21]         1.73      0.34     [0.31, 0.36]
# log_cumulative_count 1.90 [1.78, 2.03]         1.38      0.53     [0.49, 0.56]
# solving.rate 2.58 [2.41, 2.78]         1.61      0.39     [0.36, 0.42]
# behav 1.51 [1.43, 1.60]         1.23      0.66     [0.62, 0.70]
# num_trapped 2.11 [1.98, 2.27]         1.45      0.47     [0.44, 0.50]






# 3.2) Run model (single vs both) -----------------------------------------


model_both_single <- brm(  
  body_part_red ~ age + log_cumulative_count + reaction + solving.rate + behav + num_trapped +
    (1  | Subject), 
  data = laterality_data,
  family = bernoulli(),
  cores = 4,
  iter = 4000,
  chains = 4,
  seed = 3,
  control = list(adapt_delta = 0.99)
  
)

#save(model_both_single, file="model output/model_both_single.RDA")
load("model output/model_both_single.RDA")

# let's look at the model output
summary(model_both_single)

# Family: bernoulli 
# Links: mu = logit 
# Formula: body_part_red ~ age + log_cumulative_count + reaction + solving.rate + behav + num_trapped + (1 | Subject) 
# Data: laterality_data (Number of observations: 1955) 
# Draws: 4 chains, each with iter = 4000; warmup = 2000; thin = 1;
# total post-warmup draws = 8000
# 
# Multilevel Hyperparameters:
#   ~Subject (Number of levels: 20) 
# Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# sd(Intercept)     1.08      0.27     0.66     1.71 1.00     3204     4658
# 
# Regression Coefficients:
#   Estimate Est.Error l-95% CI u-95% CI Rhat Bulk_ESS Tail_ESS
# Intercept               -4.36      0.83    -6.06    -2.73 1.00     5047     4803
# ageP                     2.43      0.71     1.09     3.90 1.00     4105     4567
# log_cumulative_count     0.51      0.07     0.38     0.65 1.00     8532     5693
# reactionexit            -0.69      0.53    -1.72     0.37 1.00     5065     5327
# reactionflinch          -0.39      0.55    -1.45     0.71 1.00     5233     5484
# reactionnoreaction      -0.25      0.50    -1.22     0.75 1.00     4962     5365
# solving.rate             0.82      0.35     0.15     1.55 1.00     3467     4966
# behav                    0.11      0.32    -0.51     0.75 1.00     4600     4761
# num_trapped              0.06      0.05    -0.04     0.17 1.00     4117     4820
# 
# Draws were sampled using sampling(NUTS). For each parameter, Bulk_ESS
# and Tail_ESS are effective sample size measures, and Rhat is the potential
# scale reduction factor on split chains (at convergence, Rhat = 1).

round(exp(fixef(model_both_single)),2)

# Estimate Est.Error Q2.5 Q97.5
# Intercept                0.01      2.30 0.00  0.07
# ageP                    11.37      2.04 2.98 49.57
# log_cumulative_count     1.67      1.07 1.46  1.91
# reactionexit             0.50      1.71 0.18  1.45
# reactionflinch           0.67      1.73 0.23  2.03
# reactionnoreaction       0.78      1.66 0.29  2.12
# solving.rate             2.28      1.42 1.16  4.70
# behav                    1.12      1.37 0.60  2.11
# num_trapped              1.07      1.06 0.96  1.19

plogis(fixef(model_both_single))

# Estimate Est.Error        Q2.5      Q97.5
# Intercept            0.01257474 0.6969656 0.002333127 0.06106962
# ageP                 0.91918152 0.6710867 0.748467701 0.98022463
# log_cumulative_count 0.62517594 0.5171078 0.593269549 0.65642490
# reactionexit         0.33305199 0.6306177 0.152161485 0.59202830
# reactionflinch       0.40284312 0.6339590 0.189353889 0.66977772
# reactionnoreaction   0.43691414 0.6235482 0.227721580 0.67995432
# solving.rate         0.69509763 0.5868737 0.537137219 0.82455582
# behav                0.52809393 0.5782337 0.375009052 0.67861295
# num_trapped          0.51608443 0.5134186 0.489653031 0.54258505



# 3.3) Model check and plot -----------------------------------------------

# visual inspection of model fit
pp_check(model_both_single)
# looks like a good fit

# check for stationarity and mixing
plot(model_both_single)

# conditional effects plot
cond.plot <- plot(conditional_effects(model_both_single))

cond.plot$log_cumulative_count$layers[[1]]$aes_params$colour <- "black"
cond.plot$solving.rate$layers[[1]]$aes_params$colour <- "black"


ggarrange(
cond.plot$age+
  theme_bw()+
  labs(x= "age", y= "Probability of using both paws")+
  ylim(c(0,1))+
  scale_x_discrete(labels = c('Adult','Juvenile')),
  
cond.plot$log_cumulative_count + 
  labs(x= "experience [# solves]", y= "")+
  ylim(c(0,1))+
  theme_bw()+
  geom_line(color = "black")+
  scale_x_continuous(breaks = c(0.000000, 2.302585, 4.605170, 6.214608), labels=c(0, 10, 100, 500))+
  theme(axis.text.y=element_blank()),     # Remove legend if unnecessary, # we change the scale to number of solves (rather than log),

cond.plot$solving.rate +
  labs(x="cognitive performance [# solves per visit]", y="")+
  ylim(c(0,1))+
  theme_bw()+
  theme(axis.text.y=element_blank()),


ncol=3,
nrow=1,
#legend="right",
common.legend = F,
labels= c("a", "b", "c")
)  
 

ggsave("Figures output/Figure3.tiff", units="in", width=9, height=4, dpi=300, compression = 'lzw')

