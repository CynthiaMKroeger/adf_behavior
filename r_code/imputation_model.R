# ------------------------------------------------------------------------------ #
# 
# Code to run statistical models 
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
library(lme4)
library(nlme)
library(emmeans)


source("LMEModelSummary.R")
source("ImpLMEModelSummary.R")


# ------------------------------------------------------------------------------ #
# Read in data 
# ------------------------------------------------------------------------------ #
filename <- "imp_data_52.csv"
dat <- read.csv(filename)
head(dat)
summary(dat)

imp_all <- dat

# "interv": CON (1), ADF (2), CR (3)
imp_all$interv <- factor(imp_all$interv,
                         levels(imp_all$interv)[c(2, 1, 3)])


# ------------------------------------------------------------------------------ #
# Run tfeq_rest model and write results 
# ------------------------------------------------------------------------------ #
fixed_left <- "tfeq_rest"


fixed_right <- as.formula( ~ interv * factor(week) + tfeq_rest_1)


random <- as.formula( ~1 | part )
Data <- imp_all  
m <- max(Data$.imp)
specs = as.formula( ~ interv | factor(week) )


imp_model_summary_alldv <- list()


for(dv in seq(along = fixed_left))
{
        
        fixed <- as.formula(paste(fixed_left[dv], 
                                  fixed_right, 
                                  sep = "~")[2])
        
        model_summary_all <- list()
        for(i in 1:m)
        {
                model_summary <- LMEModelSummary(Fixed = fixed, 
                                                 Random = random, 
                                                 Data = Data[Data$.imp == i, ], 
                                                 Specs = specs)
                
                model_summary_all[[i]] <- model_summary
                rm(model_summary)
        }
        
        
        imp_model_summary <- ImpLMEModelSummary(ModelSummaryAll = model_summary_all, 
                                                m = m)
        imp_model_summary_alldv[[fixed_left[dv]]] <- imp_model_summary
        
        print(imp_model_summary)
        
        write.csv(imp_model_summary$ImpModelSummary, 
                  paste(fixed_left[dv],
                        "lme_model.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpContSummary, 
                  paste(fixed_left[dv],
                        "lme_cont.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpEmmSummary, 
                  paste(fixed_left[dv],
                        "lme_emm.csv",
                        sep = "_"))
        
}



# ------------------------------------------------------------------------------ #
# Run tfeq_uncon model and write results 
# ------------------------------------------------------------------------------ #
fixed_left <- "tfeq_uncon"


fixed_right <- as.formula( ~ interv * factor(week) + tfeq_uncon_1)


random <- as.formula( ~1 | part )
Data <- imp_all  
m <- max(Data$.imp)
specs = as.formula( ~ interv | factor(week) )


imp_model_summary_alldv <- list()


for(dv in seq(along = fixed_left))
{
        
        fixed <- as.formula(paste(fixed_left[dv], 
                                  fixed_right, 
                                  sep = "~")[2])
        
        model_summary_all <- list()
        for(i in 1:m)
        {
                model_summary <- LMEModelSummary(Fixed = fixed, 
                                                 Random = random, 
                                                 Data = Data[Data$.imp == i, ], 
                                                 Specs = specs)
                
                model_summary_all[[i]] <- model_summary
                rm(model_summary)
        }
        
        
        imp_model_summary <- ImpLMEModelSummary(ModelSummaryAll = model_summary_all, 
                                                m = m)
        imp_model_summary_alldv[[fixed_left[dv]]] <- imp_model_summary
        
        print(imp_model_summary)
        
        write.csv(imp_model_summary$ImpModelSummary, 
                  paste(fixed_left[dv],
                        "lme_model.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpContSummary, 
                  paste(fixed_left[dv],
                        "lme_cont.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpEmmSummary, 
                  paste(fixed_left[dv],
                        "lme_emm.csv",
                        sep = "_"))
        
}


# ------------------------------------------------------------------------------ #
# Run tfeq_emo model and write results 
# ------------------------------------------------------------------------------ #
fixed_left <- "tfeq_emo"


fixed_right <- as.formula( ~ interv * factor(week) + tfeq_emo_1)


random <- as.formula( ~1 | part )
Data <- imp_all  
m <- max(Data$.imp)
specs = as.formula( ~ interv | factor(week) )


imp_model_summary_alldv <- list()


for(dv in seq(along = fixed_left))
{
        
        fixed <- as.formula(paste(fixed_left[dv], 
                                  fixed_right, 
                                  sep = "~")[2])
        
        model_summary_all <- list()
        for(i in 1:m)
        {
                model_summary <- LMEModelSummary(Fixed = fixed, 
                                                 Random = random, 
                                                 Data = Data[Data$.imp == i, ], 
                                                 Specs = specs)
                
                model_summary_all[[i]] <- model_summary
                rm(model_summary)
        }
        
        
        imp_model_summary <- ImpLMEModelSummary(ModelSummaryAll = model_summary_all, 
                                                m = m)
        imp_model_summary_alldv[[fixed_left[dv]]] <- imp_model_summary
        
        print(imp_model_summary)
        
        write.csv(imp_model_summary$ImpModelSummary, 
                  paste(fixed_left[dv],
                        "lme_model.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpContSummary, 
                  paste(fixed_left[dv],
                        "lme_cont.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpEmmSummary, 
                  paste(fixed_left[dv],
                        "lme_emm.csv",
                        sep = "_"))
        
}


# ------------------------------------------------------------------------------ #
# Run vas_hung model and write results 
# ------------------------------------------------------------------------------ #
fixed_left <- "vas_hung"


fixed_right <- as.formula( ~ interv * factor(week) + vas_hung_1)


random <- as.formula( ~1 | part )
Data <- imp_all  
m <- max(Data$.imp)
specs = as.formula( ~ interv | factor(week) )


imp_model_summary_alldv <- list()


for(dv in seq(along = fixed_left))
{
        
        fixed <- as.formula(paste(fixed_left[dv], 
                                  fixed_right, 
                                  sep = "~")[2])
        
        model_summary_all <- list()
        for(i in 1:m)
        {
                model_summary <- LMEModelSummary(Fixed = fixed, 
                                                 Random = random, 
                                                 Data = Data[Data$.imp == i, ], 
                                                 Specs = specs)
                
                model_summary_all[[i]] <- model_summary
                rm(model_summary)
        }
        
        
        imp_model_summary <- ImpLMEModelSummary(ModelSummaryAll = model_summary_all, 
                                                m = m)
        imp_model_summary_alldv[[fixed_left[dv]]] <- imp_model_summary
        
        print(imp_model_summary)
        
        write.csv(imp_model_summary$ImpModelSummary, 
                  paste(fixed_left[dv],
                        "lme_model.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpContSummary, 
                  paste(fixed_left[dv],
                        "lme_cont.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpEmmSummary, 
                  paste(fixed_left[dv],
                        "lme_emm.csv",
                        sep = "_"))
        
}


# ------------------------------------------------------------------------------ #
# Run vas_satis model and write results 
# ------------------------------------------------------------------------------ #
fixed_left <- "vas_satis"


fixed_right <- as.formula( ~ interv * factor(week) + vas_satis_1)


random <- as.formula( ~1 | part )
Data <- imp_all  
m <- max(Data$.imp)
specs = as.formula( ~ interv | factor(week) )


imp_model_summary_alldv <- list()


for(dv in seq(along = fixed_left))
{
        
        fixed <- as.formula(paste(fixed_left[dv], 
                                  fixed_right, 
                                  sep = "~")[2])
        
        model_summary_all <- list()
        for(i in 1:m)
        {
                model_summary <- LMEModelSummary(Fixed = fixed, 
                                                 Random = random, 
                                                 Data = Data[Data$.imp == i, ], 
                                                 Specs = specs)
                
                model_summary_all[[i]] <- model_summary
                rm(model_summary)
        }
        
        
        imp_model_summary <- ImpLMEModelSummary(ModelSummaryAll = model_summary_all, 
                                                m = m)
        imp_model_summary_alldv[[fixed_left[dv]]] <- imp_model_summary
        
        print(imp_model_summary)
        
        write.csv(imp_model_summary$ImpModelSummary, 
                  paste(fixed_left[dv],
                        "lme_model.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpContSummary, 
                  paste(fixed_left[dv],
                        "lme_cont.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpEmmSummary, 
                  paste(fixed_left[dv],
                        "lme_emm.csv",
                        sep = "_"))
        
}


# ------------------------------------------------------------------------------ #
# Run vas_ful model and write results 
# ------------------------------------------------------------------------------ #
fixed_left <- "vas_ful"


fixed_right <- as.formula( ~ interv * factor(week) + vas_ful_1)


random <- as.formula( ~1 | part )
Data <- imp_all  
m <- max(Data$.imp)
specs = as.formula( ~ interv | factor(week) )


imp_model_summary_alldv <- list()


for(dv in seq(along = fixed_left))
{
        
        fixed <- as.formula(paste(fixed_left[dv], 
                                  fixed_right, 
                                  sep = "~")[2])
        
        model_summary_all <- list()
        for(i in 1:m)
        {
                model_summary <- LMEModelSummary(Fixed = fixed, 
                                                 Random = random, 
                                                 Data = Data[Data$.imp == i, ], 
                                                 Specs = specs)
                
                model_summary_all[[i]] <- model_summary
                rm(model_summary)
        }
        
        
        imp_model_summary <- ImpLMEModelSummary(ModelSummaryAll = model_summary_all, 
                                                m = m)
        imp_model_summary_alldv[[fixed_left[dv]]] <- imp_model_summary
        
        print(imp_model_summary)
        
        write.csv(imp_model_summary$ImpModelSummary, 
                  paste(fixed_left[dv],
                        "lme_model.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpContSummary, 
                  paste(fixed_left[dv],
                        "lme_cont.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpEmmSummary, 
                  paste(fixed_left[dv],
                        "lme_emm.csv",
                        sep = "_"))
        
}


# ------------------------------------------------------------------------------ #
# Run se_rel model and write results 
# ------------------------------------------------------------------------------ #
fixed_left <- "se_rel"


fixed_right <- as.formula( ~ interv * factor(week) + se_rel_1)


random <- as.formula( ~1 | part )
Data <- imp_all  
m <- max(data$.imp)
specs = as.formula( ~ interv | factor(week) )


imp_model_summary_alldv <- list()


for(dv in seq(along = fixed_left))
{
        
        fixed <- as.formula(paste(fixed_left[dv], 
                                  fixed_right, 
                                  sep = "~")[2])
        
        model_summary_all <- list()
        for(i in 1:m)
        {
                model_summary <- LMEModelSummary(Fixed = fixed, 
                                                 Random = random, 
                                                 Data = Data[Data$.imp == i, ], 
                                                 Specs = specs)
                
                model_summary_all[[i]] <- model_summary
                rm(model_summary)
        }
        
        
        imp_model_summary <- ImpLMEModelSummary(ModelSummaryAll = model_summary_all, 
                                                m = m)
        imp_model_summary_alldv[[fixed_left[dv]]] <- imp_model_summary
        
        print(imp_model_summary)
        
        write.csv(imp_model_summary$ImpModelSummary, 
                  paste(fixed_left[dv],
                        "lme_model.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpContSummary, 
                  paste(fixed_left[dv],
                        "lme_cont.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpEmmSummary, 
                  paste(fixed_left[dv],
                        "lme_emm.csv",
                        sep = "_"))
        
}


# ------------------------------------------------------------------------------ #
# Run se_cal model and write results 
# ------------------------------------------------------------------------------ #
fixed_left <- "se_cal"


fixed_right <- as.formula( ~ interv * factor(week) + se_cal_1)


random <- as.formula( ~1 | part )
Data <- imp_all  
m <- max(Data$.imp)
specs = as.formula( ~ interv | factor(week) )


imp_model_summary_alldv <- list()


for(dv in seq(along = fixed_left))
{
        
        fixed <- as.formula(paste(fixed_left[dv], 
                                  fixed_right, 
                                  sep = "~")[2])
        
        model_summary_all <- list()
        for(i in 1:m)
        {
                model_summary <- LMEModelSummary(Fixed = fixed, 
                                                 Random = random, 
                                                 Data = Data[Data$.imp == i, ], 
                                                 Specs = specs)
                
                model_summary_all[[i]] <- model_summary
                rm(model_summary)
        }
        
        
        imp_model_summary <- ImpLMEModelSummary(ModelSummaryAll = model_summary_all, 
                                                m = m)
        imp_model_summary_alldv[[fixed_left[dv]]] <- imp_model_summary
        
        print(imp_model_summary)
        
        write.csv(imp_model_summary$ImpModelSummary, 
                  paste(fixed_left[dv],
                        "lme_model.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpContSummary, 
                  paste(fixed_left[dv],
                        "lme_cont.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpEmmSummary, 
                  paste(fixed_left[dv],
                        "lme_emm.csv",
                        sep = "_"))
        
}


# ------------------------------------------------------------------------------ #
# Run se_salt model and write results 
# ------------------------------------------------------------------------------ #
fixed_left <- "se_salt"


fixed_right <- as.formula( ~ interv * factor(week) + se_salt_1)


random <- as.formula( ~1 | part )
Data <- imp_all  
m <- max(Data$.imp)
specs = as.formula( ~ interv | factor(week) )


imp_model_summary_alldv <- list()


for(dv in seq(along = fixed_left))
{
        
        fixed <- as.formula(paste(fixed_left[dv], 
                                  fixed_right, 
                                  sep = "~")[2])
        
        model_summary_all <- list()
        for(i in 1:m)
        {
                model_summary <- LMEModelSummary(Fixed = fixed, 
                                                 Random = random, 
                                                 Data = Data[Data$.imp == i, ], 
                                                 Specs = specs)
                
                model_summary_all[[i]] <- model_summary
                rm(model_summary)
        }
        
        
        imp_model_summary <- ImpLMEModelSummary(ModelSummaryAll = model_summary_all, 
                                                m = m)
        imp_model_summary_alldv[[fixed_left[dv]]] <- imp_model_summary
        
        print(imp_model_summary)
        
        write.csv(imp_model_summary$ImpModelSummary, 
                  paste(fixed_left[dv],
                        "lme_model.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpContSummary, 
                  paste(fixed_left[dv],
                        "lme_cont.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpEmmSummary, 
                  paste(fixed_left[dv],
                        "lme_emm.csv",
                        sep = "_"))
        
}


# ------------------------------------------------------------------------------ #
# Run se_fat model and write results 
# ------------------------------------------------------------------------------ #
fixed_left <- "se_fat"


fixed_right <- as.formula( ~ interv * factor(week) + se_fat_1)


random <- as.formula( ~1 | part )
Data <- imp_all  
m <- max(Data$.imp)
specs = as.formula( ~ interv | factor(week) )


imp_model_summary_alldv <- list()


for(dv in seq(along = fixed_left))
{
        
        fixed <- as.formula(paste(fixed_left[dv], 
                                  fixed_right, 
                                  sep = "~")[2])
        
        model_summary_all <- list()
        for(i in 1:m)
        {
                model_summary <- LMEModelSummary(Fixed = fixed, 
                                                 Random = random, 
                                                 Data = Data[Data$.imp == i, ], 
                                                 Specs = specs)
                
                model_summary_all[[i]] <- model_summary
                rm(model_summary)
        }
        
        
        imp_model_summary <- ImpLMEModelSummary(ModelSummaryAll = model_summary_all, 
                                                m = m)
        imp_model_summary_alldv[[fixed_left[dv]]] <- imp_model_summary
        
        print(imp_model_summary)
        
        write.csv(imp_model_summary$ImpModelSummary, 
                  paste(fixed_left[dv],
                        "lme_model.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpContSummary, 
                  paste(fixed_left[dv],
                        "lme_cont.csv",
                        sep = "_"))
        write.csv(imp_model_summary$ImpEmmSummary, 
                  paste(fixed_left[dv],
                        "lme_emm.csv",
                        sep = "_"))
        
}

