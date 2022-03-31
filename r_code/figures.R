# ------------------------------------------------------------------------------ #
# 
# Figure generation for adf_behavior
# 
#
# Authors: 
#
# Chen Lyu, Indiana University Bloomington (chenlyu@indiana.edu)
# Cynthia M. Kroeger, University of Sydney (cynthia.kroeger@sydney.edu.au)
#
# 
# ------------------------------------------------------------------------------ #
# Dependencies  
# ------------------------------------------------------------------------------ #
library(readxl)
library(ggpubr)


# ------------------------------------------------------------------------------ #
# Read in data  
# ------------------------------------------------------------------------------ #
file_tfeq_rest <- "Pooled lsmeans Tfeq_rest Regular.csv"
tfeq_rest <- read.csv(file_tfeq_rest)
head(tfeq_rest)
summary(tfeq_rest)


file_tfeq_uncon <- "Pooled lsmeans Tfeq_uncon Regular.csv"
tfeq_uncon <- read.csv(file_tfeq_uncon)
head(tfeq_uncon)
summary(tfeq_uncon)


file_tfeq_emo <- "Pooled lsmeans Tfeq_emo Regular.csv"
tfeq_emo <- read.csv(file_tfeq_emo)
head(tfeq_emo)
summary(tfeq_emo)


file_se_rel <- "Pooled lsmeans SE_Rel Regular.csv"
se_rel <- read.csv(file_se_rel)
head(se_rel)
summary(se_rel)


file_se_salt <- "Pooled lsmeans SE_Salt Regular.csv"
se_salt <- read.csv(file_se_salt)
head(se_salt)
summary(se_salt)


file_se_cal <- "Pooled lsmeans SE_Cal Regular.csv"
se_cal <- read.csv(file_se_cal)
head(se_cal)
summary(se_cal)


file_se_fat <- "Pooled lsmeans SE_Fat Regular.csv"
se_fat <- read.csv(file_se_fat)
head(se_fat)
summary(se_fat)


file_vas_hung <- "Pooled lsmeans Vas_hung Regular.csv"
vas_hung <- read.csv(file_vas_hung)
head(vas_hung)
summary(vas_hung)


file_vas_satis <- "Pooled lsmeans Vas_satis Regular.csv"
vas_satis <- read.csv(file_vas_satis)
head(vas_satis)
summary(vas_satis)


file_vas_ful <- "Pooled lsmeans Vas_ful Regular.csv"
vas_ful <- read.csv(file_vas_ful)
head(vas_ful)
summary(vas_ful)


# ------------------------------------------------------------------------------ #
# Plot for tfeq
# ------------------------------------------------------------------------------ #
pd <- position_dodge(2)


rest <- ggplot(tfeq_rest, 
               aes(x = week, 
                   y = Estimate, 
                   group = interv)) + 
        geom_errorbar(aes(ymin = Estimate - StdErr, 
                          ymax = Estimate + StdErr), 
                      width = 0.2, 
                      position = pd) +
        ylab("Restrained Eating") +
        xlab("Week") +
        scale_x_continuous(limits = c(0, 50), 
                           breaks = c(1, 12, 24, 36, 48)) + 
        ylim(0, 100) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        geom_line(aes(linetype = interv), position = pd) +
        geom_point(position = pd) +
        scale_linetype_manual(values = c("longdash", "solid","dotted")) 


uncon <- ggplot(tfeq_uncon, 
                aes(x = week, 
                    y = Estimate, 
                    group = interv)) + 
        geom_errorbar(aes(ymin = Estimate - StdErr, 
                          ymax = Estimate + StdErr), 
                      width = 0.2, 
                      position = pd) +
        ylab("Uncontrolled Eating") +
        xlab("Week") +
        scale_x_continuous(limits = c(0, 50), 
                           breaks = c(1, 12, 24, 36, 48)) + 
        ylim(0, 100) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        geom_line(aes(linetype = interv), position = pd) +
        geom_point(position = pd) +
        scale_linetype_manual(values = c("longdash", "solid","dotted")) 


emo <- ggplot(tfeq_emo, 
              aes(x = week, 
                  y = Estimate, 
                  group = interv)) + 
        geom_errorbar(aes(ymin = Estimate - StdErr, 
                          ymax = Estimate + StdErr), 
                      width = 0.2, 
                      position = pd) +
        ylab("Emotional Eating") +
        xlab("Week") +
        scale_x_continuous(limits = c(0, 50), 
                           breaks = c(1, 12, 24, 36, 48)) + 
        ylim(0, 100) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        geom_line(aes(linetype = interv), position = pd) +
        geom_point(position = pd) +
        scale_linetype_manual(values = c("longdash", "solid","dotted")) 


ggarrange(rest, 
          uncon, 
          emo, 
          labels = c("A", "B", "C"),
          ncol = 3, 
          common.legend = TRUE)


# ------------------------------------------------------------------------------ #
# Plot for se
# ------------------------------------------------------------------------------ #
rel <- ggplot(se_rel, 
              aes(x = week, 
                  y = Estimate, 
                  group = interv)) + 
        geom_errorbar(aes(ymin = Estimate - StdErr, 
                          ymax = Estimate + StdErr), 
                      width = 0.2, 
                      position = pd) +
        ylab("Resisting Relapse") +
        xlab("Week") +
        scale_x_continuous(limits = c(0, 50), 
                           breaks = c(1, 12, 24, 36, 48)) + 
        ylim(0, 100) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        geom_line(aes(linetype = interv), position = pd) +
        geom_point(position = pd) +
        scale_linetype_manual(values = c("longdash", "solid","dotted")) 


salt <- ggplot(se_salt, 
               aes(x = week, 
                   y = Estimate, 
                   group = interv)) + 
        geom_errorbar(aes(ymin = Estimate - StdErr, 
                          ymax = Estimate + StdErr), 
                      width = 0.2, 
                      position = pd) +
        ylab("Reducing Salt") +
        xlab("Week") +
        scale_x_continuous(limits = c(0, 50), 
                           breaks = c(1, 12, 24, 36, 48)) + 
        ylim(0, 100) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        geom_line(aes(linetype = interv), position = pd) +
        geom_point(position = pd) +
        scale_linetype_manual(values = c("longdash", "solid", "dotted")) 


cal <- ggplot(se_cal, 
              aes(x = week, 
                  y = Estimate, 
                  group = interv)) + 
        geom_errorbar(aes(ymin = Estimate - StdErr, 
                          ymax = Estimate + StdErr), 
                      width = 0.2, 
                      position = pd) +
        ylab("Reducing Calories") +
        xlab("Week") +
        scale_x_continuous(limits = c(0, 50),
                           breaks = c(1, 12, 24, 36, 48)) + 
        ylim(0, 100) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        geom_line(aes(linetype = interv), position = pd) +
        geom_point(position = pd) +
        scale_linetype_manual(values = c("longdash", "solid", "dotted")) 


fat <- ggplot(se_fat, 
              aes(x = week, 
                  y = Estimate, 
                  group = interv)) + 
        geom_errorbar(aes(ymin = Estimate - StdErr, 
                          ymax = Estimate + StdErr), 
                      width = 0.2, 
                      position = pd) +
        ylab("Reducing Fat") +
        xlab("Week") +
        scale_x_continuous(limits = c(0, 50), 
                           breaks = c(1, 12, 24, 36, 48)) + 
        ylim(0, 100) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        geom_line(aes(linetype = interv), position = pd) +
        geom_point(position = pd) +
        scale_linetype_manual(values = c("longdash", "solid", "dotted")) 


ggarrange(rel, 
          salt, 
          cal, 
          fat, 
          labels = c("A", "B", "C", "D"),
          ncol = 4,
          common.legend = TRUE)


# ------------------------------------------------------------------------------ #
# Plot for vas
# ------------------------------------------------------------------------------ #
hung <- ggplot(vas_hung, 
               aes(x = week, 
                   y = Estimate, 
                   group = interv)) + 
        geom_errorbar(aes(ymin = Estimate - StdErr, 
                          ymax = Estimate + StdErr), 
                      width = 0.2, 
                      position = pd) +
        ylab("Hunger") +
        xlab("Week") +
        scale_x_continuous(limits = c(0, 50), 
                           breaks = c(1, 12, 24, 36, 48)) + 
        ylim(0, 10) + 
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        geom_line(aes(linetype = interv), position = pd) +
        geom_point(position = pd) +
        scale_linetype_manual(values = c("longdash", "solid","dotted")) 


satis <- ggplot(vas_satis, 
                aes(x = week, 
                    y = Estimate, 
                    group = interv)) + 
        geom_errorbar(aes(ymin = Estimate - StdErr, 
                          ymax = Estimate + StdErr), 
                      width = 0.2, 
                      position = pd) +
        ylab("Satisfaction") +
        xlab("Week") +
        scale_x_continuous(limits = c(0, 50), 
                           breaks = c(1, 12, 24, 36, 48)) +
        ylim(0, 10) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        geom_line(aes(linetype = interv), position = pd) +
        geom_point(position = pd) +
        scale_linetype_manual(values = c("longdash", "solid","dotted")) 


ful <- ggplot(vas_ful, 
              aes(x = week, 
                  y = Estimate, 
                  group = interv)) + 
        geom_errorbar(aes(ymin = Estimate - StdErr, 
                          ymax = Estimate + StdErr), 
                      width = 0.2, 
                      position = pd) +
        ylab("Fullness") +
        xlab("Week") +
        scale_x_continuous(limits = c(0, 50), 
                           breaks = c(1, 12, 24, 36, 48)) +
        ylim(0, 10) +
        theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              panel.background = element_blank(), 
              axis.line = element_line(colour = "black")) +
        geom_line(aes(linetype = interv), position = pd) +
        geom_point(position = pd) +
        scale_linetype_manual(values = c("longdash", "solid","dotted")) 


ggarrange(hung, 
          satis, 
          ful, 
          labels = c("A", "B", "C"),
          ncol = 3, 
          common.legend = TRUE)

