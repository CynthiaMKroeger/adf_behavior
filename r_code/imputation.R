# ------------------------------------------------------------------------------ #
# 
# Code to impute missing data for adf_behavior
# 
#
# Authors: 
#
# Hiroki Naganobori, Indiana University Bloomington (hnaganob@indiana.edu)
# Cynthia M. Kroeger, University of Sydney (cynthia.kroeger@sydney.edu.au)
#
# 
# ------------------------------------------------------------------------------ #
# Dependencies  
# ------------------------------------------------------------------------------ #
# install.packages("mice")
# install.packages("reshape2")
# install.packages("car")
library(mice)
library(reshape2)
library(car)


# ------------------------------------------------------------------------------ #
# Read in data  
# ------------------------------------------------------------------------------ #
file_name <- "adf_behavior_long_HN_2018-12-05.csv"
dat <- read.csv(file_name)
head(dat)
summary(dat)


# ------------------------------------------------------------------------------ #
# Recode variables
# ------------------------------------------------------------------------------ #
behav <- dat


# interv: ADF, CR, CON
behav$interv <- factor(ifelse(behav$interv_adf == 1,
                              "ADF",
                              ifelse(behav$interv_cr == 1,
                                     "CR",
                                     "CON")))
behav$interv <- factor(behav$interv,
                       levels(behav$interv)[c(2, 1, 3)])


# sex: F, M
behav$sex <- factor(ifelse(behav$sex_m == 1,
                           "M",
                           "F"))


# ethn: AA, CAUC, AS, HISP
behav$ethn <- factor(ifelse(behav$ethn_aa == 1,
                            "AA",
                            ifelse(behav$ethn_cauc == 1,
                                   "CAUC",
                                   ifelse(behav$ethn_as == 1,
                                          "AS",
                                          "HISP"))))


# ------------------------------------------------------------------------------ #
# Choose variables of interest
# ------------------------------------------------------------------------------ #
colNames <- colnames(behav)
colNames


# Time-Invariant Variables
which_tiv <- c(which(colNames == "part"),
               which(colNames == "age"),
               which(colNames == "sex"),
               which(colNames == "ethn"),
               which(colNames == "interv"),
               which(colNames == "bmi_bl_pr"),
               which(colNames == "wt_bl_pr"))


# Time-Variant Variables
which_tv <- c(which(colNames == "wt"),
              # which(colNames == "hr"),
              # which(colNames == "sbp"),
              # which(colNames == "dbp"),
              which(colNames == "fm"),
              which(colNames == "tc"),
              which(colNames == "ldl"),
              which(colNames == "hdl"),
              # which(colNames == "tag"),
              which(colNames == "gluc"),
              # which(colNames == "ins"),
              which(colNames == "adip"),
              which(colNames == "lep"),
              # which(colNames == "res"),
              which(colNames == "tnf"),
              which(colNames == "igf"),
              # which(colNames == "il6"),
              which(colNames == "crp"),
              which(colNames == "hcy"),
              which(colNames == "aee"),
              which(colNames == "lm"),
              which(colNames == "vfat"),
              which(colNames == "tee"),
              which(colNames == "tfeq_rest"),
              which(colNames == "tfeq_uncon"),
              which(colNames == "tfeq_emo"),
              which(colNames == "vas_hung"),
              which(colNames == "vas_satis"),
              which(colNames == "vas_ful"),
              which(colNames == "se_rel"),
              which(colNames == "se_cal"),
              which(colNames == "se_salt"),
              which(colNames == "se_fat"))


# Time
which_t <- which(colNames == "week")


# ------------------------------------------------------------------------------ #
# New data frame for analysis 
# ------------------------------------------------------------------------------ #
behavior <- as.data.frame(behav[ , c(which_tiv, 
                                     which_tv, 
                                     which_t)])
summary(behavior)


# ------------------------------------------------------------------------------ #
# Wide Form
# ------------------------------------------------------------------------------ #
wide_form <- list()
tv_list <- colnames(behav)[which_tv]
tiv_list <- colnames(behav)[which_tiv]


tiv_formula <- tiv_list[1]
for( i in seq(along = tiv_list)[-1])
{
  tiv_formula <- paste(tiv_formula, 
                       tiv_list[i], 
                       sep = "+")
}
tiv_formula


dcast_formula <- paste(tiv_formula, 
                       "week", 
                       sep = "~")


for( i in seq(along = tv_list))
{
  wide_form[[tv_list[i]]] <- dcast(behav, 
                                   dcast_formula, 
                                   value.var = tv_list[i])
  colnames(wide_form[[tv_list[i]]]) <- c(tiv_list, 
                                         paste(tv_list[i],
                                               unique(behav$week),
                                               sep = "_"))
}


behav_w <- wide_form[[1]]
for( i in seq(along = tv_list)[-1])
{
  behav_w <- cbind(behav_w, 
                   wide_form[[tv_list[i]]][ , -c(1:length(tiv_list))])
}


summary(behav_w)


# ------------------------------------------------------------------------------ #
# Imputation (using wide form)
# ------------------------------------------------------------------------------ #
md.pattern(behav_w)  # missing data pattern


# view missing data another way to determine proportion of missingness 
# this will help inform the number of imputations needed 

# install.packages("VIM")
library(VIM)
behav_w_aggr <- aggr(behav_w,
                     col = mdc(1:2), 
                     numbers = TRUE, 
                     sortVars = TRUE, 
                     labels = names(behav_w), 
                     cex.axis = 0.7, 
                     gap = 3, 
                     ylab = c("Proportion of Missingness", "Missingness Pattern"))


# vas_hung_36; vas_satis_36; vas_full_36 ~52% missingness
# thus, we will impute 52 datasets 


# ------------------------------------------------------------------------------ #
# Impute missing data 
# ------------------------------------------------------------------------------ #
imp_data_w <- mice(behav_w, 
                   m = 52,   
                   maxit = 25, 
                   method = "pmm", 
                   seed = 999)


# list of imputed variables
imp_data_w$imp


# ------------------------------------------------------------------------------ #
# Extract 40 complete data sets as one data frame
# ------------------------------------------------------------------------------ #
imp_data_w_all <- complete(imp_data_w, 
                           "long")


# ------------------------------------------------------------------------------ #
# Write data frame to .csv 
# ------------------------------------------------------------------------------ #
write.csv(imp_data_w_all, 
          file = "imp_data_w_all.csv")


# ------------------------------------------------------------------------------ #
# Covert wide data to long format 
# ------------------------------------------------------------------------------ #
library(reshape2)
library(car)


# wt
restruct_wt <- melt(imp_data_w_all,
                    id = c(".imp", 
                           ".id",
                           "part", 
                           # "age", 
                           "sex", 
                           "ethn", 
                           "interv", 
                           "bmi_bl_pr", 
                           "wt_bl_pr"), 
                    measure.vars = c("wt_1", 
                                     "wt_12", 
                                     "wt_24", 
                                     "wt_36", 
                                     "wt_48"), 
                    variable.name = "week", 
                    value.name = "wt")


restruct_wt$week <- recode(restruct_wt$week, 
                           "'wt_1' = 1;
                           'wt_12' = 12;
                           'wt_24' = 24;
                           'wt_36' = 36;
                           'wt_48' = 48")


# fm
restruct_fm <- melt(imp_data_w_all,
                    id = c(".imp", 
                           ".id",
                           "part", 
                           "age", 
                           "sex", 
                           "ethn", 
                           "interv", 
                           "bmi_bl_pr", 
                           "wt_bl_pr"), 
                    measure.vars = c("fm_1", 
                                     "fm_12", 
                                     "fm_24", 
                                     "fm_36", 
                                     "fm_48"), 
                    variable.name = "week", 
                    value.name = "fm")


restruct_fm$week <- recode(restruct_fm$week, 
                           "'fm_1' = 1;
                           'fm_12' = 12;
                           'fm_24' = 24;
                           'fm_36' = 36;
                           'fm_48' = 48")


# tc
restruct_tc <- melt(imp_data_w_all,
                    id = c(".imp", 
                           ".id",
                           "part", 
                           # "age", 
                           "sex", 
                           "ethn", 
                           "interv", 
                           "bmi_bl_pr", 
                           "wt_bl_pr"), 
                    measure.vars = c("tc_1", 
                                     "tc_12", 
                                     "tc_24", 
                                     "tc_36", 
                                     "tc_48"), 
                    variable.name = "week", 
                    value.name = "tc")


restruct_tc$week <- recode(restruct_tc$week, 
                           "'tc_1' = 1;
                           'tc_12' = 12;
                           'tc_24' = 24;
                           'tc_36' = 36;
                           'tc_48' = 48")


# ldl
restruct_ldl <- melt(imp_data_w_all,
                     id = c(".imp", 
                            ".id",
                            "part", 
                            "age", 
                            "sex", 
                            "ethn", 
                            "interv", 
                            "bmi_bl_pr", 
                            "wt_bl_pr"), 
                     measure.vars = c("ldl_1", 
                                      "ldl_12", 
                                      "ldl_24", 
                                      "ldl_36", 
                                      "ldl_48"), 
                     variable.name = "week", 
                     value.name = "ldl")


restruct_ldl$week <- recode(restruct_ldl$week, 
                            "'ldl_1' = 1;
                            'ldl_12' = 12;
                            'ldl_24' = 24;
                            'ldl_36' = 36;
                            'ldl_48' = 48")


# hdl
restruct_hdl <- melt(imp_data_w_all,
                     id = c(".imp", 
                            ".id",
                            "part", 
                            # "age", 
                            "sex", 
                            "ethn", 
                            "interv", 
                            "bmi_bl_pr", 
                            "wt_bl_pr"), 
                     measure.vars = c("hdl_1", 
                                      "hdl_12", 
                                      "hdl_24", 
                                      "hdl_36", 
                                      "hdl_48"), 
                     variable.name = "week", 
                     value.name = "hdl")


restruct_hdl$week <- recode(restruct_hdl$week, 
                            "'hdl_1' = 1;
                            'hdl_12' = 12;
                            'hdl_24' = 24;
                            'hdl_36' = 36;
                            'hdl_48' = 48")


# gluc
restruct_gluc <- melt(imp_data_w_all,
                      id = c(".imp", 
                             ".id",
                             "part", 
                            # "age", 
                             "sex", 
                             "ethn", 
                             "interv", 
                             "bmi_bl_pr", 
                             "wt_bl_pr"), 
                      measure.vars = c("gluc_1", 
                                       "gluc_12", 
                                       "gluc_24", 
                                       "gluc_36", 
                                       "gluc_48"), 
                      variable.name = "week", 
                      value.name = "gluc")


restruct_gluc$week <- recode(restruct_gluc$week, 
                             "'gluc_1' = 1;
                             'gluc_12' = 12;
                             'gluc_24' = 24;
                             'gluc_36' = 36;
                             'gluc_48' = 48")


# adip
restruct_adip <- melt(imp_data_w_all,
                      id = c(".imp", 
                             ".id",
                             "part", 
                             "age", 
                             "sex", 
                             "ethn", 
                             "interv", 
                             "bmi_bl_pr", 
                             "wt_bl_pr"), 
                      measure.vars = c("adip_1", 
                                       "adip_12", 
                                       "adip_24", 
                                       "adip_36", 
                                       "adip_48"), 
                      variable.name = "week", 
                      value.name = "adip")


restruct_adip$week <- recode(restruct_adip$week, 
                             "'adip_1' = 1;
                             'adip_12' = 12;
                             'adip_24' = 24;
                             'adip_36' = 36;
                             'adip_48' = 48")


# lep
restruct_lep <- melt(imp_data_w_all,
                     id = c(".imp", 
                            ".id",
                            "part", 
                            "age", 
                            "sex", 
                            "ethn", 
                            "interv", 
                            "bmi_bl_pr", 
                            "wt_bl_pr"), 
                     measure.vars = c("lep_1", 
                                      "lep_12", 
                                      "lep_24", 
                                      "lep_36", 
                                      "lep_48"), 
                     variable.name = "week", 
                     value.name = "lep")


restruct_lep$week <- recode(restruct_lep$week, 
                            "'lep_1' = 1;
                            'lep_12' = 12;
                            'lep_24' = 24;
                            'lep_36' = 36;
                            'lep_48' = 48")


# tnf
restruct_tnf <- melt(imp_data_w_all,
                     id = c(".imp", 
                            ".id",
                            "part", 
                            "age", 
                            "sex", 
                            "ethn", 
                            "interv", 
                            "bmi_bl_pr", 
                            "wt_bl_pr"), 
                     measure.vars = c("tnf_1", 
                                      "tnf_12", 
                                      "tnf_24", 
                                      "tnf_36", 
                                      "tnf_48"), 
                     variable.name = "week", 
                     value.name = "tnf")


restruct_tnf$week <- recode(restruct_tnf$week, 
                            "'tnf_1' = 1;
                            'tnf_12' = 12;
                            'tnf_24' = 24;
                            'tnf_36' = 36;
                            'tnf_48' = 48")


# igf
restruct_igf <- melt(imp_data_w_all,
                     id = c(".imp", 
                            ".id",
                            "part", 
                            # "age", 
                            "sex", 
                            "ethn", 
                            "interv", 
                            "bmi_bl_pr", 
                            "wt_bl_pr"), 
                     measure.vars = c("igf_1", 
                                      "igf_12", 
                                      "igf_24", 
                                      "igf_36", 
                                      "igf_48"), 
                     variable.name = "week", 
                     value.name = "igf")


restruct_igf$week <- recode(restruct_igf$week, 
                            "'igf_1' = 1;
                            'igf_12' = 12;
                            'igf_24' = 24;
                            'igf_36' = 36;
                            'igf_48' = 48")


# crp
restruct_crp <- melt(imp_data_w_all,
                     id = c(".imp", 
                            ".id",
                            "part", 
                            "age", 
                            "sex", 
                            "ethn", 
                            "interv", 
                            "bmi_bl_pr", 
                            "wt_bl_pr"), 
                     measure.vars = c("crp_1", 
                                      "crp_12", 
                                      "crp_24", 
                                      "crp_36", 
                                      "crp_48"), 
                     variable.name = "week", 
                     value.name = "crp")


restruct_crp$week <- recode(restruct_crp$week, 
                            "'crp_1' = 1;
                            'crp_12' = 12;
                            'crp_24' = 24;
                            'crp_36' = 36;
                            'crp_48' = 48")


# hcy
restruct_hcy <- melt(imp_data_w_all,
                     id = c(".imp", 
                            ".id",
                            "part", 
                            "age", 
                            "sex", 
                            "ethn", 
                            "interv", 
                            "bmi_bl_pr", 
                            "wt_bl_pr"), 
                     measure.vars = c("hcy_1", 
                                      "hcy_12", 
                                      "hcy_24", 
                                      "hcy_36", 
                                      "hcy_48"), 
                     variable.name = "week", 
                     value.name = "hcy")


restruct_hcy$week <- recode(restruct_hcy$week, 
                            "'hcy_1' = 1;
                            'hcy_12' = 12;
                            'hcy_24' = 24;
                            'hcy_36' = 36;
                            'hcy_48' = 48")


# aee
restruct_aee <- melt(imp_data_w_all,
                     id = c(".imp", 
                            ".id",
                            "part", 
                            "age", 
                            "sex", 
                            "ethn", 
                            "interv", 
                            "bmi_bl_pr", 
                            "wt_bl_pr"), 
                     measure.vars = c("aee_1", 
                                      "aee_12", 
                                      "aee_24", 
                                      "aee_36", 
                                      "aee_48"), 
                     variable.name = "week", 
                     value.name = "aee")


restruct_aee$week <- recode(restruct_aee$week, 
                            "'aee_1' = 1;
                            'aee_12' = 12;
                            'aee_24' = 24;
                            'aee_36' = 36;
                            'aee_48' = 48")


# lm
restruct_lm <- melt(imp_data_w_all,
                    id = c(".imp", 
                           ".id",
                           "part", 
                           "age", 
                           "sex", 
                           "ethn", 
                           "interv", 
                           "bmi_bl_pr", 
                           "wt_bl_pr"), 
                    measure.vars = c("lm_1", 
                                     "lm_12", 
                                     "lm_24", 
                                     "lm_36", 
                                     "lm_48"), 
                    variable.name = "week", 
                    value.name = "lm")


restruct_lm$week <- recode(restruct_lm$week, 
                           "'lm_1' = 1;
                           'lm_12' = 12;
                           'lm_24' = 24;
                           'lm_36' = 36;
                           'lm_48' = 48")


# vfat
restruct_vfat <- melt(imp_data_w_all,
                      id = c(".imp", 
                             ".id",
                             "part", 
                             "age", 
                             "sex", 
                             "ethn", 
                             "interv", 
                             "bmi_bl_pr", 
                             "wt_bl_pr"), 
                      measure.vars = c("vfat_1", 
                                       "vfat_12", 
                                       "vfat_24", 
                                       "vfat_36", 
                                       "vfat_48"), 
                      variable.name = "week", 
                      value.name = "vfat")


restruct_vfat$week <- recode(restruct_vfat$week, 
                             "'vfat_1' = 1;
                             'vfat_12' = 12;
                             'vfat_24' = 24;
                             'vfat_36' = 36;
                             'vfat_48' = 48")


# tee
restruct_tee <- melt(imp_data_w_all,
                     id = c(".imp", 
                            ".id",
                            "part", 
                            "age", 
                            "sex", 
                            "ethn", 
                            "interv", 
                            "bmi_bl_pr", 
                            "wt_bl_pr"), 
                     measure.vars = c("tee_1", 
                                      "tee_12", 
                                      "tee_24", 
                                      "tee_36", 
                                      "tee_48"), 
                     variable.name = "week", 
                     value.name = "tee")


restruct_tee$week <- recode(restruct_tee$week, 
                            "'tee_1' = 1;
                            'tee_12' = 12;
                            'tee_24' = 24;
                            'tee_36' = 36;
                            'tee_48' = 48")


# tfeq_rest
restruct_tfeq_rest <- melt(imp_data_w_all,
                           id = c(".imp", 
                                  ".id",
                                  "part", 
                                  # "age", 
                                  "sex", 
                                  "ethn", 
                                  "interv", 
                                  "bmi_bl_pr", 
                                  "wt_bl_pr"), 
                           measure.vars = c("tfeq_rest_1", 
                                            "tfeq_rest_12", 
                                            "tfeq_rest_24", 
                                            "tfeq_rest_36", 
                                            "tfeq_rest_48"), 
                           variable.name = "week", 
                           value.name = "tfeq_rest")


restruct_tfeq_rest$week <- recode(restruct_tfeq_rest$week, 
                                  "'tfeq_rest_1' = 1;
                                  'tfeq_rest_12' = 12;
                                  'tfeq_rest_24' = 24;
                                  'tfeq_rest_36' = 36;
                                  'tfeq_rest_48' = 48")


# tfeq_uncon
restruct_tfeq_uncon <- melt(imp_data_w_all,
                            id = c(".imp", 
                                   ".id", 
                                   "part", 
                                   # "age", 
                                   "sex", 
                                   "ethn", 
                                   "interv", 
                                   "bmi_bl_pr", 
                                   "wt_bl_pr"), 
                            measure.vars = c("tfeq_uncon_1", 
                                             "tfeq_uncon_12", 
                                             "tfeq_uncon_24", 
                                             "tfeq_uncon_36", 
                                             "tfeq_uncon_48"), 
                            variable.name = "week", 
                            value.name = "tfeq_uncon")


restruct_tfeq_uncon$week <- recode(restruct_tfeq_uncon$week, 
                                   "'tfeq_uncon_1' = 1;
                                   'tfeq_uncon_12' = 12;
                                   'tfeq_uncon_24' = 24;
                                   'tfeq_uncon_36' = 36;
                                   'tfeq_uncon_48' = 48")


# tfeq_emo
restruct_tfeq_emo <- melt(imp_data_w_all,
                          id = c(".imp", 
                                 ".id",
                                 "part", 
                                 # "age", 
                                 "sex", 
                                 "ethn", 
                                 "interv", 
                                 "bmi_bl_pr", 
                                 "wt_bl_pr"), 
                          measure.vars = c("tfeq_emo_1", 
                                           "tfeq_emo_12", 
                                           "tfeq_emo_24", 
                                           "tfeq_emo_36", 
                                           "tfeq_emo_48"), 
                          variable.name = "week", 
                          value.name = "tfeq_emo")


restruct_tfeq_emo$week <- recode(restruct_tfeq_emo$week, 
                                 "'tfeq_emo_1' = 1;
                                 'tfeq_emo_12' = 12;
                                 'tfeq_emo_24' = 24;
                                 'tfeq_emo_36' = 36;
                                 'tfeq_emo_48' = 48")


# vas_hung
restruct_vas_hung <- melt(imp_data_w_all,
                          id = c(".imp", 
                                 ".id",
                                 "part", 
                                 # "age", 
                                 "sex", 
                                 "ethn", 
                                 "interv", 
                                 "bmi_bl_pr", 
                                 "wt_bl_pr"), 
                          measure.vars = c("vas_hung_1", 
                                           "vas_hung_12", 
                                           "vas_hung_24", 
                                           "vas_hung_36", 
                                           "vas_hung_48"), 
                          variable.name = "week", 
                          value.name = "vas_hung")


restruct_vas_hung$week <- recode(restruct_vas_hung$week, 
                                 "'vas_hung_1' = 1;
                                 'vas_hung_12' = 12;
                                 'vas_hung_24' = 24;
                                 'vas_hung_36' = 36;
                                 'vas_hung_48' = 48")


# vas_satis
restruct_vas_satis <- melt(imp_data_w_all,
                           id = c(".imp", 
                                  ".id", 
                                  "part", 
                                 # "age", 
                                  "sex", 
                                  "ethn", 
                                  "interv", 
                                  "bmi_bl_pr", 
                                  "wt_bl_pr"), 
                           measure.vars = c("vas_satis_1", 
                                            "vas_satis_12", 
                                            "vas_satis_24", 
                                            "vas_satis_36", 
                                            "vas_satis_48"), 
                           variable.name = "week", 
                           value.name = "vas_satis")


restruct_vas_satis$week <- recode(restruct_vas_satis$week, 
                                  "'vas_satis_1' = 1;
                                  'vas_satis_12' = 12;
                                  'vas_satis_24' = 24;
                                  'vas_satis_36' = 36;
                                  'vas_satis_48' = 48")


# vas_ful
restruct_vas_ful <- melt(imp_data_w_all,
                         id = c(".imp", 
                                ".id",
                                "part", 
                                # "age", 
                                "sex", 
                                "ethn", 
                                "interv", 
                                "bmi_bl_pr", 
                                "wt_bl_pr"), 
                         measure.vars = c("vas_ful_1", 
                                          "vas_ful_12", 
                                          "vas_ful_24", 
                                          "vas_ful_36", 
                                          "vas_ful_48"), 
                         variable.name = "week", 
                         value.name = "vas_ful")


restruct_vas_ful$week <- recode(restruct_vas_ful$week, 
                                "'vas_ful_1' = 1;
                                'vas_ful_12' = 12;
                                'vas_ful_24' = 24;
                                'vas_ful_36' = 36;
                                'vas_ful_48' = 48")


# se_rel
restruct_se_rel <- melt(imp_data_w_all,
                        id = c(".imp", 
                               ".id",
                               "part", 
                              # "age", 
                               "sex", 
                               "ethn", 
                               "interv", 
                               "bmi_bl_pr", 
                               "wt_bl_pr"), 
                        measure.vars = c("se_rel_1", 
                                         "se_rel_12", 
                                         "se_rel_24", 
                                         "se_rel_36", 
                                         "se_rel_48"), 
                        variable.name = "week", 
                        value.name = "se_rel")


restruct_se_rel$week <- recode(restruct_se_rel$week, 
                               "'se_rel_1' = 1;
                               'se_rel_12' = 12;
                               'se_rel_24' = 24;
                               'se_rel_36' = 36;
                               'se_rel_48' = 48")


# se_cal
restruct_se_cal <- melt(imp_data_w_all,
                        id = c(".imp", 
                               ".id",
                               "part", 
                               # "age", 
                               "sex", 
                               "ethn", 
                               "interv", 
                               "bmi_bl_pr", 
                               "wt_bl_pr"), 
                        measure.vars = c("se_cal_1", 
                                         "se_cal_12", 
                                         "se_cal_24", 
                                         "se_cal_36", 
                                         "se_cal_48"), 
                        variable.name = "week", 
                        value.name = "se_cal")


restruct_se_cal$week <- recode(restruct_se_cal$week, 
                               "'se_cal_1' = 1;
                               'se_cal_12' = 12;
                               'se_cal_24' = 24;
                               'se_cal_36' = 36;
                               'se_cal_48' = 48")


# se_salt
restruct_se_salt <- melt(imp_data_w_all,
                         id = c(".imp", 
                                ".id",
                                "part", 
                                # "age", 
                                "sex", 
                                "ethn", 
                                "interv", 
                                "bmi_bl_pr", 
                                "wt_bl_pr"), 
                         measure.vars = c("se_salt_1", 
                                          "se_salt_12", 
                                          "se_salt_24", 
                                          "se_salt_36", 
                                          "se_salt_48"), 
                         variable.name = "week", 
                         value.name = "se_salt")


restruct_se_salt$week <- recode(restruct_se_salt$week, 
                                "'se_salt_1' = 1;
                                'se_salt_12' = 12;
                                'se_salt_24' = 24;
                                'se_salt_36' = 36;
                                'se_salt_48' = 48")


# se_fat
restruct_se_fat <- melt(imp_data_w_all,
                        id = c(".imp", 
                               ".id",
                               "part", 
                               # "age", 
                               "sex", 
                               "ethn", 
                               "interv", 
                               "bmi_bl_pr", 
                               "wt_bl_pr"), 
                        measure.vars = c("se_fat_1", 
                                         "se_fat_12", 
                                         "se_fat_24", 
                                         "se_fat_36", 
                                         "se_fat_48"), 
                        variable.name = "week", 
                        value.name = "se_fat")


restruct_se_fat$week <- recode(restruct_se_fat$week, 
                               "'se_fat_1' = 1;
                               'se_fat_12' = 12;
                               'se_fat_24' = 24;
                               'se_fat_36' = 36;
                               'se_fat_48' = 48")

# merge all 
intersect(names(restruct_wt), 
          names(restruct_fm))
merged_fm <- merge(restruct_wt, 
                   restruct_fm,
                   all = TRUE)
merged_tc <- merge(merged_fm, 
                   restruct_tc, 
                   all = TRUE) 
merged_ldl <- merge(merged_tc, 
                    restruct_ldl, 
                    all = TRUE)
merged_hdl <- merge(merged_ldl, 
                    restruct_hdl, 
                    all = TRUE)
merged_gluc <- merge(merged_hdl, 
                    restruct_gluc, 
                    all = TRUE) 
merged_adip <- merge(merged_gluc, 
                    restruct_adip, 
                    all = TRUE) 
merged_lep <- merge(merged_adip, 
                    restruct_lep, 
                    all = TRUE) 
merged_tnf <- merge(merged_lep, 
                    restruct_tnf, 
                    all = TRUE) 
merged_igf <- merge(merged_tnf, 
                    restruct_igf, 
                    all = TRUE) 
merged_crp <- merge(merged_igf, 
                    restruct_crp, 
                    all = TRUE) 
merged_hcy <- merge(merged_crp, 
                    restruct_hcy, 
                    all = TRUE) 
merged_aee <- merge(merged_hcy, 
                    restruct_aee, 
                    all = TRUE) 
merged_lm <- merge(merged_aee, 
                   restruct_lm, 
                   all = TRUE)
merged_vfat <- merge(merged_lm, 
                     restruct_vfat, 
                     all = TRUE)
merged_tee <- merge(merged_vfat, 
                    restruct_tee, 
                    all = TRUE)
merged_tfeq_rest <- merge(merged_tee, 
                          restruct_tfeq_rest, 
                          all = TRUE)
merged_tfeq_uncon <- merge(merged_tfeq_rest, 
                           restruct_tfeq_uncon, 
                           all = TRUE)
merged_tfeq_emo <- merge(merged_tfeq_uncon, 
                         restruct_tfeq_emo, 
                         all = TRUE)
merged_vas_hung <- merge(merged_tfeq_emo, 
                         restruct_vas_hung, 
                         all = TRUE)
merged_vas_satis <- merge(merged_vas_hung, 
                          restruct_vas_satis, 
                          all = TRUE)
merged_vas_ful <- merge(merged_vas_satis, 
                        restruct_vas_ful, 
                        all = TRUE)
merged_se_rel <- merge(merged_vas_ful, 
                       restruct_se_rel, 
                       all = TRUE)
merged_se_cal <- merge(merged_se_rel, 
                       restruct_se_cal, 
                       all = TRUE)
merged_se_salt <- merge(merged_se_cal, 
                        restruct_se_salt, 
                        all = TRUE)
merged_se_fat <- merge(merged_se_salt, 
                       restruct_se_fat, 
                       all = TRUE)


imp_data_l_all <- merged_se_fat


# write csv with original and all 52 imputed datasets for analysis 
write.csv(imp_data_l_all, 
          file = "imp_data_l_all_14_dec_2018.csv")
