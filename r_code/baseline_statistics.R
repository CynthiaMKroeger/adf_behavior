# ------------------------------------------------------------------------------ #
# 
# Baseline statistics for Table 1
# 
#
# Authors: 
#
# Chen Lyu, Indiana University Bloomington (chenlyu@indiana.edu)
# Cynthia M. Kroeger, University of Sydney (cynthia.kroeger@sydney.edu.au)
#
# 
# ------------------------------------------------------------------------------ #
# Read in data  
# ------------------------------------------------------------------------------ #
adf <- read.csv('./adf_behavior_long_HN_2018-12-05.csv')


# create completer variable
# If participants have at least one measurement on week 48, count as completer 
# Otherwise, count them as non-completer
adf$completer <- 1
id <- as.character(adf$part[((adf$week == 48) & 
                                     is.na(adf$tfeq_rest) & 
                                     is.na(adf$tfeq_uncon) & 
                                     is.na(adf$tfeq_emo) & 
                                     is.na(adf$vas_hung) & 
                                     is.na(adf$vas_satis) & 
                                     is.na(adf$vas_ful) & 
                                     is.na(adf$se_rel) & 
                                     is.na(adf$se_cal) & 
                                     is.na(adf$se_salt) & 
                                     is.na(adf$se_fat))])
adf$completer[adf$part %in% id] <- 0
table(adf$completer[adf$week == 1])


# create non-completer variable
adf$noncompleter <- 1 - adf$completer
table(adf$noncompleter[adf$week == 1])


# ------------------------------------------------------------------------------ #
# Descriptive statistics for each column 
# ------------------------------------------------------------------------------ #
stat <- function(x){
  mean <- sapply(adf[(adf$week == 1) & (x == 1), 38:47], mean, na.rm = TRUE)
  sd <- sapply(adf[(adf$week == 1) & (x == 1), 38:47], sd, na.rm = TRUE)
  out <- cbind(mean, sd)
  print(out)
}
 

# ADF
table(adf$interv_adf[adf$week == 1])
stat(adf$interv_adf)


# CR
table(adf$interv_cr[adf$week == 1])
stat(adf$interv_cr)


# CON
table(adf$interv_con[adf$week == 1])
stat(adf$interv_con)


# All
nrow(adf[adf$week == 1, ])
mean <- sapply(adf[adf$week == 1, 38:47], mean, na.rm = TRUE)
sd <- sapply(adf[adf$week == 1, 38:47], sd, na.rm = TRUE)
cbind(mean, sd)


# Completer
stat(adf$completer)


# Non-completer
stat(adf$noncompleter)


# ------------------------------------------------------------------------------ #
# Test for normality 
# ------------------------------------------------------------------------------ #
# subset week 1 
adf_1 <- subset(adf, week == "1")


# restrained eating 
plot(density(adf_1$tfeq_rest, na.rm = TRUE))
shapiro.test(adf_1$tfeq_rest) # p-value = 0.197
qqnorm(adf_1$tfeq_rest)


# uncontrolled eating
plot(density(adf_1$tfeq_uncon, na.rm = TRUE))
shapiro.test(adf_1$tfeq_uncon) # p-value = 0.1025
qqnorm(adf_1$tfeq_uncon)


# emotional eating
plot(density(adf_1$tfeq_emo, na.rm = TRUE))
shapiro.test(adf_1$tfeq_emo) # p-value = 0.001137
qqnorm(adf_1$tfeq_emo)


# resisting relapse 
plot(density(adf_1$se_rel, na.rm = TRUE))
shapiro.test(adf_1$se_rel) # p-value = 1.38e-06
qqnorm(adf_1$se_rel)


# reducing calories 
plot(density(adf_1$se_cal, na.rm = TRUE))
shapiro.test(adf_1$se_cal) # p-value = 4.277e-06
qqnorm(adf_1$se_cal)


# reducing salt 
plot(density(adf_1$se_salt, na.rm = TRUE))
shapiro.test(adf_1$se_salt) # p-value = 1.641e-09
qqnorm(adf_1$se_salt)


# reducing fat 
plot(density(adf_1$se_fat, na.rm = TRUE))
shapiro.test(adf_1$se_fat) # p-value = 1.916e-09
qqnorm(adf_1$se_fat)


# hunger 
plot(density(adf_1$vas_hung, na.rm = TRUE))
shapiro.test(adf_1$vas_hung) # p-value = 4.024e-05
qqnorm(adf_1$vas_hung)


# satisfaction 
plot(density(adf_1$vas_satis, na.rm = TRUE))
shapiro.test(adf_1$vas_satis) # p-value = 0.01301
qqnorm(adf_1$vas_satis)


# fullness 
plot(density(adf_1$vas_ful, na.rm = TRUE))
shapiro.test(adf_1$vas_ful) # p-value = 0.0004439
qqnorm(adf_1$vas_ful)


# ------------------------------------------------------------------------------ #
# Test for equal variances
# ------------------------------------------------------------------------------ #
# subset by group 
imf <- subset(adf_1, interv_adf == "1")
dcr <- subset(adf_1, interv_cr == "1")
con <- subset(adf_1, interv_con == "1")


# restrained eating 
var.test(imf$tfeq_rest, dcr$tfeq_rest) # p-value = 0.4541
var.test(con$tfeq_rest, dcr$tfeq_rest) # p-value = 0.152
var.test(imf$tfeq_rest, con$tfeq_rest) # p-value = 0.037 


# uncontrolled eating
var.test(imf$tfeq_uncon, dcr$tfeq_uncon) # p-value = 0.01491
var.test(con$tfeq_uncon, dcr$tfeq_uncon) # p-value = 0.9548
var.test(imf$tfeq_uncon, con$tfeq_uncon) # p-value = 0.01895


# emotional eating
var.test(imf$tfeq_emo, dcr$tfeq_emo) # p-value = 0.4725
var.test(con$tfeq_emo, dcr$tfeq_emo) # p-value = 0.7635
var.test(imf$tfeq_emo, con$tfeq_emo) # p-value = 0.7127


# resisting relapse 
var.test(imf$se_rel, dcr$se_rel) # p-value = 0.1379
var.test(con$se_rel, dcr$se_rel) # p-value = 0.2769
var.test(imf$se_rel, con$se_rel) # p-value = 0.01173


# reducing calories 
var.test(imf$se_cal, dcr$se_cal) # p-value = 0.1602
var.test(con$se_cal, dcr$se_cal) # p-value = 0.9815
var.test(imf$se_cal, con$se_cal) # p-value = 0.1772


# reducing salt 
var.test(imf$se_salt, dcr$se_salt) # p-value = 0.8828
var.test(con$se_salt, dcr$se_salt) # p-value = 0.0658
var.test(imf$se_salt, con$se_salt) # p-value = 0.05005


# reducing fat 
var.test(imf$se_fat, dcr$se_fat) # p-value = 0.01021
var.test(con$se_fat, dcr$se_fat) # p-value = 0.1157
var.test(imf$se_fat, con$se_fat) # p-value = 7.984e-05


# hunger 
var.test(imf$vas_hung, dcr$vas_hung) # p-value = 0.8688
var.test(con$vas_hung, dcr$vas_hung) # p-value = 0.9084
var.test(imf$vas_hung, con$vas_hung) # p-value = 0.9669


# satisfaction 
var.test(imf$vas_satis, dcr$vas_satis) # p-value = 0.7625
var.test(con$vas_satis, dcr$vas_satis) # p-value = 0.4601
var.test(imf$vas_satis, con$vas_satis) # p-value = 0.3117


# fullness 
var.test(imf$vas_ful, dcr$vas_ful) # p-value = 0.3782
var.test(con$vas_ful, dcr$vas_ful) # p-value = 0.7706
var.test(imf$vas_ful, con$vas_ful) # p-value = 0.2697


# ------------------------------------------------------------------------------ #
# Test for differences among interventions and control  
# ------------------------------------------------------------------------------ #
# One way ANOVA tfeq_rest
adf_1
write.csv(adf_1, 
          file = "adf_baseline.csv")
adf_baseline <- read.csv('./adf_baseline.csv')


# Create 'group' column 
adf_baseline_g <- adf_baseline %>% 
  mutate(group = 
           case_when(interv_adf == 1 ~ "adf", 
                     interv_cr == 1 ~ "cr", 
                     interv_con == 1 ~ "con"))
adf_baseline_g 
write.csv(adf_baseline_g, 
          file = "adf_baseline_g.csv")


library(dplyr)
group_by(adf_baseline_g, group) %>%
        summarise(
                count = n(),
                mean = mean(tfeq_rest, na.rm = TRUE), 
                sd = sd(tfeq_rest, na.rm = TRUE)
        )


# One way ANOVA tfeq_rest
rest_aov <- aov(tfeq_rest ~ group, data = adf_baseline_g)
summary(rest_aov)


# One way ANOVA tfeq_uncon
uncon_aov <- aov(tfeq_uncon ~ group, data = adf_baseline_g)
summary(uncon_aov)


# One way ANOVA tfeq_emo
emo_aov <- aov(tfeq_emo ~ group, data = adf_baseline_g)
summary(emo_aov)


# One way ANOVA se_rel
rel_aov <- aov(se_rel ~ group, data = adf_baseline_g)
summary(rel_aov)


# One way ANOVA se_cal
cal_aov <- aov(se_cal ~ group, data = adf_baseline_g)
summary(cal_aov)
TukeyHSD(cal_aov)


# One way ANOVA se_salt
salt_aov <- aov(se_salt ~ group, data = adf_baseline_g)
summary(salt_aov)
TukeyHSD(salt_aov)


# One way ANOVA se_fat
fat_aov <- aov(se_fat ~ group, data = adf_baseline_g)
summary(fat_aov)
TukeyHSD(fat_aov)


# One way ANOVA vas_hung
hung_aov <- aov(vas_hung ~ group, data = adf_baseline_g)
summary(hung_aov)


# One way ANOVA vas_satis
satis_aov <- aov(vas_satis ~ group, data = adf_baseline_g)
summary(satis_aov)
TukeyHSD(satis_aov)


# One way ANOVA vas_ful
ful_aov <- aov(vas_ful ~ group, data = adf_baseline_g)
summary(ful_aov)
TukeyHSD(ful_aov)


# ------------------------------------------------------------------------------ #
# Test for differences among completers and non-completers  
# ------------------------------------------------------------------------------ #
# T-test tfeq_rest
t.test(adf_baseline$tfeq_rest ~ adf_baseline$completer)


# T-test tfeq_uncon
t.test(adf_baseline$tfeq_uncon ~ adf_baseline$completer)


# T-test tfeq_emo
t.test(adf_baseline$tfeq_emo ~ adf_baseline$completer)


# T-test se_rel
t.test(adf_baseline$se_rel ~ adf_baseline$completer)


# T-test se_cal
t.test(adf_baseline$se_cal ~ adf_baseline$completer)


# T-test se_salt
t.test(adf_baseline$se_salt ~ adf_baseline$completer)


# T-test se_fat
t.test(adf_baseline$se_fat ~ adf_baseline$completer)


# T-test vas_hung
t.test(adf_baseline$vas_hung ~ adf_baseline$completer)


# T-test vas_satis
t.test(adf_baseline$vas_satis ~ adf_baseline$completer)


# T-test vas_ful
t.test(adf_baseline$vas_ful ~ adf_baseline$completer)
