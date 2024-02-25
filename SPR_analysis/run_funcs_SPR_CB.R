library(tidyverse)
library(stringr)
library(ggplot2)
library(scales)
library(readxl)
library(dplyr)
library(nortest)
library(janitor)
library(data.table)
library("gridExtra") 
library("lme4")
library("plotrix") 
source("./SP_filter_plot.R")


#Self-Paced Braille Reading===================
ORIG_SENTENCES = 1840
ORIG_SEGS = 17750

ALL_CONDS = c('GP', 'NG', 'MV', 'NMV', 'FL','DGI','DGP','DBI','DBP')
CONDS = c('GP', 'NG', 'MV', 'NMV')
CONDS1 = c('GP', 'NG', 'MV', 'NMV', 'FL')
GP_CRIT <- list(c('GP','seg07'), c('NG','seg08'),  c('GP','seg08'), c('NG','seg09'))
MV_CRIT <- list(c('MV','CRIT01'), c('MV','CRIT02'),c('MV','CRIT03'),c('NMV','CRIT01'),c('NMV','CRIT02'),c('NMV','CRIT03'))

GP_first <- list(c('GP','seg07'), c('NG','seg08'))
ANALYSIS_CONDS <- as.vector(c('GP', 'NG', 'MV', 'NMV'))

raw_SPR_CB <- read_xlsx("FINAL_SPR_CB.xlsx")

raw_SPR_CB_acc <- accuracy_by_cond(data = raw_SPR_CB, acc_conds = ALL_CONDS, within_subject = FALSE)
plot_acc(acc.data = raw_SPR_CB_acc, conds = ALL_CONDS, rounding_decimal = 2, title = "Raw Accuracy",within_subject = FALSE)
plot_acc(acc.data = raw_SPR_CB_acc, conds = CONDS, rounding_decimal = 2, title = "Raw Accuracy",within_subject = FALSE)
raw_SPR_CB_acc_ind <- accuracy_by_cond(data = raw_SPR_CB, acc_conds = ALL_CONDS, within_subject = TRUE)
plot_acc(acc.data = raw_SPR_CB_acc_ind, conds = c('FL'), rounding_decimal = 3, title = "Filler Accuracy within Condition",within_subject = TRUE, by_id = TRUE)
plot_acc(acc.data = raw_SPR_CB_acc_ind, conds = CONDS1, rounding_decimal = 1, title = "Individual Accuracy within Condition",within_subject = TRUE, by_id = TRUE)
plot_acc(acc.data = raw_SPR_CB_acc_ind, conds = c('GP', 'NG'), rounding_decimal = 1, title = "GP, NG Accuracy within Condition",within_subject = TRUE, by_id = TRUE)
plot_acc(acc.data = raw_SPR_CB_acc_ind, conds = c('MV', 'NMV'), rounding_decimal = 1, title = "MV, NMV Accuracy within Condition",within_subject = TRUE, by_id = TRUE)


resp_boxplot(data = raw_SPR_CB, title = "Raw GP Question Response Time", conds = c("GP"),within_subject = TRUE)
resp_boxplot(data = raw_SPR_CB, title = "Raw NG Question Response Time", conds = c("NG"),within_subject = TRUE)
resp_boxplot(data = raw_SPR_CB, title = "Raw GP, NGP Question Response Time", conds = c("GP","NG"),within_subject = FALSE)
resp_boxplot(data = raw_SPR_CB, title = "Raw GP, NGP Question Response Time", conds = c("GP","NG"),within_subject = TRUE)

resp_boxplot(data = raw_SPR_CB, title = "Raw MV Question Response Time", conds = c("MV"),within_subject = TRUE)
resp_boxplot(data = raw_SPR_CB, title = "Raw NMV Question Response Time", conds = c("NMV"),within_subject = TRUE)
resp_boxplot(data = raw_SPR_CB, title = "Raw MV, NMV Question Response Time", conds = c("MV","NMV"),within_subject = FALSE)
resp_boxplot(data = raw_SPR_CB, title = "Raw MV, NMV Question Response Time", conds = c("MV","NMV"),within_subject = TRUE)

resp_boxplot(data = raw_SPR_CB, title = "Raw Question Response Time", conds = c('GP', 'NG', 'MV', 'NMV', 'FL'), order = c('GP', 'NG', 'MV', 'NMV', 'FL'), within_subject = TRUE)
resp_boxplot(data = raw_SPR_CB, title = "Raw Question Response Time", conds = c('GP', 'NG', 'MV', 'NMV', 'FL'), order = c('GP', 'NG', 'MV', 'NMV', 'FL'), within_subject = FALSE)


#STEP1: exclude subjects with fillers < 80%-=========

#==========Q.Step1=========
Q_step1_8s_SPR_CB <- subset(raw_SPR_CB, (Seg  == 'question' & RT <= 8000) | Seg != 'question')

#BELOW 2 LINES OPTION STEP CHECKING NUMBERS
excl_8s <- perc_excluded(raw_data = raw_SPR_CB, filtered_data = Q_step1_8s_SPR_CB, conds = c("GP","NG"), within_subject = FALSE)
excl_8s_indv <- perc_excluded(raw_data = raw_SPR_CB, filtered_data = Q_step1_8s_SPR_CB, conds = c("GP","NG"),within_subject = TRUE)

excl_8s_all <- perc_excluded(raw_data = raw_SPR_CB, filtered_data = Q_step1_8s_SPR_CB, conds = c("GP","NG","MV","NMV"), within_subject = FALSE)

plot_perc_excluded(raw_data = raw_SPR_CB, filtered_data = Q_step1_8s_SPR_CB, conds = c("GP","NG"),remain_or_lost = "remain",title = "GP, NG filtered Question RT <= 8s",within_subject = TRUE)
plot_perc_excluded(raw_data = raw_SPR_CB, filtered_data = Q_step1_8s_SPR_CB, conds = c("MV","NMV"),remain_or_lost = "remain",title = "MV, NMV filtered Question RT <= 8s",within_subject = TRUE)

plot_perc_excluded(raw_SPR_CB, Q_step1_8s_SPR_CB,conds=c("GP","NG","MV","NMV"),remain_or_lost = "remain",title="4 Exp Conds filtered Question RT <= 8s",within_subject = FALSE, within_cond = TRUE)


resp_boxplot(data = Q_step1_8s_SPR_CB, title = "<=8S Question Response Time", conds = c('GP', 'NG', 'MV', 'NMV', 'FL'), order = c('GP', 'NG', 'MV', 'NMV', 'FL'), within_subject = TRUE)
resp_boxplot(data = Q_step1_8s_SPR_CB, title = "<=8S Question Response Time", conds = c('GP', 'NG', 'MV', 'NMV', 'FL'), order = c('GP', 'NG', 'MV', 'NMV', 'FL'), within_subject = FALSE)


#==============Q.Step2===============
#replace question RTs 3SD above or below with the subject RT mean within condition
qSD_list <- list(list('GP','question'),list('NG','question'),list('MV','question'),list('NMV','question'))
qSD_filters <- generate_filters(data = Q_step1_8s_SPR_CB,filter_segs = qSD_list, range = 3, method="sd")

q_replaced <- data.frame(matrix(ncol = ncol(Q_step1_8s_SPR_CB), nrow = 0))
colnames(q_replaced) <- colnames(Q_step1_8s_SPR_CB)
Q_step2_3SD_SPR_CB <- Q_step1_8s_SPR_CB
for (q_filt in qSD_filters){
  list <- replaceRT(Q_step2_3SD_SPR_CB, q_filt, 3, replace_w_mean = TRUE)
  Q_step2_3SD_SPR_CB <- list$data
  q_replaced <- rbind(q_replaced, list$replaced)
}

excl_q3SD_from_step1 <- perc_excluded(raw_data = Q_step1_8s_SPR_CB, filtered_data = q_replaced, conds = c("GP","NG","MV","NMV"), within_subject = FALSE)
excl_q3SD_from_raw <- perc_excluded(raw_data = raw_SPR_CB, filtered_data = q_replaced, conds = c("GP","NG","MV","NMV"), within_subject = FALSE)


plot_perc_excluded(raw_data = Q_step1_8s_SPR_CB, filtered_data = q_replaced, conds = c("GP","NG"),digits = 0.1, remain_or_lost = "lost",title = "% GP, NG Question RTs replaced with mean RT", within_subject = TRUE)
plot_perc_excluded(raw_data = Q_step1_8s_SPR_CB, filtered_data = q_replaced, conds = c("GP","NG"),remain_or_lost = "lost",title = "% GP, NG Question RTs replaced with mean RT",within_subject = FALSE)

plot_perc_excluded(raw_data = Q_step1_8s_SPR_CB, filtered_data = q_replaced, conds = c("MV","NMV"),digits = 0.1, remain_or_lost = "lost",title = "% MV, NMV Question RTs replaced with mean RT",within_subject = TRUE)
plot_perc_excluded(raw_data = Q_step1_8s_SPR_CB, filtered_data = q_replaced, conds = c("MV","NMV"),remain_or_lost = "lost",title = "% MV, NMV Question RTs replaced with mean RT",within_subject = FALSE)

plot_perc_excluded(raw_data = Q_step1_8s_SPR_CB, filtered_data = q_replaced, conds = c('GP','NG',"MV","NMV"),remain_or_lost = "lost",title = "Filtered % Question RTs replaced with mean RT",within_subject = FALSE)

resp_boxplot(data = Q_step2_3SD_SPR_CB, title = "Filtered and Replaced (with Mean) GP, NGP Question Response Time", conds = c("GP","NG"),within_subject = FALSE)
resp_boxplot(data = Q_step2_3SD_SPR_CB, title = "Filtered and Replaced (with Mean) GP, NGP Question Response Time", conds = c("GP","NG"),within_subject = TRUE)

resp_boxplot(data = Q_step2_3SD_SPR_CB, title = "Filtered and Replaced (with Mean) MV, NMV Question Response Time", conds = c("MV","NMV"),within_subject = FALSE)
resp_boxplot(data = Q_step2_3SD_SPR_CB, title = "Filtered and Replaced (with Mean) MV, NMV Question Response Time", conds = c("MV","NMV"),within_subject = TRUE)

#After Q.Step2: check question RT final percentage=============
q_replaced <- rbind(q_replaced, subset(raw_SPR_CB,Seg == 'question' & RT > 8000))
excl_q_from_raw <- perc_excluded(raw_data = raw_SPR_CB, filtered_data = q_replaced, conds = c("GP","NG","MV","NMV"), within_subject = FALSE)

plot_perc_excluded(raw_data = raw_SPR_CB, filtered_data = q_replaced, conds = c("GP","NG"),remain_or_lost = "lost",title = "Final GP, NG Question % filtered or replaced\n baseline: raw results", within_subject = TRUE)
plot_perc_excluded(raw_data = raw_SPR_CB, filtered_data = q_replaced, conds = c("MV","NMV"),remain_or_lost = "lost",title = "Final MV, NMV Question % filtered or replaced\n baseline: raw results",within_subject = TRUE)

plot_perc_excluded(raw_data = raw_SPR_CB, filtered_data = q_replaced, conds = c("GP","NG","MV","NMV"),remain_or_lost = "lost",title = "Final % Question RT filtered or replaced\n baseline: raw results",within_subject = FALSE)



#S.Step1 ==================replace individual segment outliers beyond 3SD with the mean================================
#reading only: do not filter out "hard" slow outliers
segs_list <- list(list('GP','seg01'),list('GP','seg02'),list('GP','seg03'),list('GP','seg04'),list('GP','seg05'),list('GP','seg06'),list('GP','seg07'), list('GP','seg08'), list('GP','seg09'),  
                  list('NG','seg01'),  list('NG','seg02'),list('NG','seg03'),  list('NG','seg04'),list('NG','seg05'),  list('NG','seg06'),list('NG','seg07'),  list('NG','seg08'),list('NG','seg09'),  list('NG','seg10'),
                  list('MV','seg01'),  list('MV','seg02'),list('MV','seg03'),  list('MV','seg04'),list('MV','seg05'),  list('MV','seg06'),list('MV','seg07'),  list('MV','seg08'),list('MV','seg09'),  list('MV','seg10'),list('MV','seg11'),list('MV','seg12'),list('MV','seg13'),list('MV','seg14'),list('MV','seg15'),list('MV','seg16'),list('MV','seg17'),list('MV','CRIT01'),list('MV','CRIT02'),list('MV','CRIT03'),list('MV','total_duration'),
                  list('NMV','seg01'),  list('NMV','seg02'),list('NMV','seg03'),  list('NMV','seg04'),list('NMV','seg05'),  list('NMV','seg06'),list('NMV','seg07'),  list('NMV','seg08'),list('NMV','seg09'),  list('NMV','seg10'),list('NMV','seg11'),list('NMV','seg12'),list('NMV','seg13'),list('NMV','seg14'),list('NMV','seg15'),list('NMV','seg16'),list('NMV','seg17'),list('NMV','CRIT01'),list('NMV','CRIT02'),list('NMV','CRIT03'),list('NMV','total_duration')
)

S_step1_3SD_SPR_CB <- raw_SPR_CB
seg_filters <- generate_filters(data = S_step1_3SD_SPR_CB,filter_segs = segs_list, range = 3, method="sd")
segs_replaced <- data.frame(matrix(ncol = ncol(S_step1_3SD_SPR_CB), nrow = 0))
colnames(segs_replaced) <- colnames(S_step1_3SD_SPR_CB)
for (s_f in seg_filters){
  s_f <- na.omit(s_f)
  list <- replaceRT(S_step1_3SD_SPR_CB, s_f, 3)
  S_step1_3SD_SPR_CB <- list$data
  segs_replaced <- rbind(segs_replaced, list$replaced)
}

plot_perc_excluded(raw_data = raw_SPR_CB, filtered_data = segs_replaced, conds = c("GP","NG"),digits = 0.1, remain_or_lost = "lost",title = "GP, NG indv seg RT beyond 3SD that were replaced", dropped_single_seg = TRUE,within_subject = TRUE)
plot_perc_excluded(raw_data = raw_SPR_CB, filtered_data = segs_replaced, conds = c("MV","NMV"),digits = 0.1,remain_or_lost = "lost",title = "MV, NMV indv seg RT beyond 3SD that were replaced",dropped_single_seg = TRUE,within_subject = TRUE)

excl_s_all_from_raw <- perc_excluded(raw_data = raw_SPR_CB, filtered_data = segs_replaced, conds = c("GP","NG",'MV','NMV'), isQuestion=FALSE,within_subject = FALSE)
excl_s_all_ind_from_raw <- perc_excluded(raw_data = raw_SPR_CB, filtered_data = segs_replaced, conds = c("GP","NG",'MV','NMV'), isQuestion=FALSE,within_subject = TRUE)

plot_perc_excluded(raw_data = raw_SPR_CB, filtered_data = segs_replaced, conds = c("GP","NG","MV","NMV"),remain_or_lost = "lost",title = "Final Segment RT after outlier replacement/nDenominator: raw results/nIncluded % from step 1",dropped_single_seg = TRUE,within_subject = FALSE)

final_SPR_CB_Q <- subset(Q_step2_3SD_SPR_CB, CONDITION %in% c('GP','NG','FL','MV','NMV') & Seg == "question")
final_SPR_CB_S <- subset(S_step1_3SD_SPR_CB,CONDITION %in% c('GP','NG','FL','MV','NMV') & Seg != "question")

#====================

crit.rt <- crit_rt(data=S_step1_3SD_SPR_CB,segs = GP_CRIT, by_seg = TRUE, within_subject = TRUE)
plot_crit_rt(data = S_step1_3SD_SPR_CB,segs = list(c('GP','seg07'), c('NG','seg08')), by_seg = TRUE, title = "Filtered GP, NG critical segs RT", within_subject = TRUE)
plot_crit_rt(data = S_step1_3SD_SPR_CB,segs = list(c('GP','seg08'), c('NG','seg09')), by_seg = TRUE, title = "Filtered GP, NG first posterior seg RT", within_subject = TRUE)
plot_crit_rt(data = S_step1_3SD_SPR_CB,segs = list(c('GP','seg09'), c('NG','seg10')), by_seg = TRUE, title = "Filtered GP, NG second posterior seg RT", within_subject = TRUE)
plot_crit_rt(data = S_step1_3SD_SPR_CB,segs = list(c('GP','seg08'),c('GP','seg09'), c('NG','seg09'),c('NG','seg10')), by_seg = TRUE, title = "GP posterior (seg08,seg09) vs NG posterior (seg09,seg10) RT", within_subject = TRUE)


#===========After step3
resp_boxplot(data = final_SPR_CB_Q, title = "Filtered GP, NGP Question Response Time", conds = c("GP","NG"),within_subject = FALSE)
resp_boxplot(data = final_SPR_CB_Q, title = "Filtered GP, NGP Question Response Time", conds = c("GP","NG"),within_subject = TRUE)

filtered_SPR_CB_acc_ind <- accuracy_by_cond(data = final_SPR_CB_Q, acc_conds = ALL_CONDS, within_subject = TRUE)
plot_acc(acc.data = filtered_SPR_CB_acc_ind, conds = c('GP', 'NG'), rounding_decimal = 1, title = "Filtered GP, NG Accuracy within Condition",within_subject = TRUE, by_id = TRUE)
plot_acc(acc.data = filtered_SPR_CB_acc_ind, conds = c('MV', 'NMV'), rounding_decimal = 1, title = "Filtered MV, NMV Accuracy within Condition",within_subject = TRUE, by_id = TRUE)
filtered_SPR_CB_acc <- accuracy_by_cond(data = final_SPR_CB_Q, acc_conds = ALL_CONDS, within_subject = FALSE)
plot_acc(acc.data = filtered_SPR_CB_acc, conds = c('GP', 'NG'), rounding_decimal = 1, title = "Filtered GP, NG Accuracy within Condition",within_subject = FALSE, by_id = TRUE)
plot_acc(acc.data = filtered_SPR_CB_acc, conds = c('GP', 'NG','MV','NMV'), rounding_decimal = 1, title = "Filtered GP, NG Accuracy within Condition",within_subject = FALSE, by_id = TRUE)



#============FILTERED PLOTS
#============critical seg RT=======
#crit.rt <- crit_rt(data=final_SPR_CB_S,segs = CRIT_SEGMENTS, by_seg = TRUE, within_subject = TRUE)
plot_crit_rt(data = final_SPR_CB_S,segs = list(c('GP','seg07'), c('NG','seg08')), by_seg = TRUE, title = "FILTERED GP, NG critical segs RT", within_subject = TRUE)
rt_boxplot(data = final_SPR_CB_S, title = "FILTERED GP seg07 (critical segment) vs NG seg08 (control) RT",segs = list(c('GP','seg07'), c('NG','seg08')), by_seg = TRUE, within_subject = TRUE)

rt_boxplot(data = final_SPR_CB_S, title = "FILTERED GP seg07 (critical segment) vs NG seg08 (control) RT",segs = list(c('GP','seg07'), c('NG','seg08')), by_seg = TRUE, within_subject = FALSE)

rt_boxplot(data = final_SPR_CB_S, title = "FILTERED GP posterior (seg08,seg09) vs NG posterior (seg09,seg10) RT",segs = list(c('GP','seg08'),c('GP','seg09'), c('NG','seg09'),c('NG','seg10')), by_seg = FALSE, within_subject = TRUE)

rt_boxplot(data = final_SPR_CB_S, title = "FILTERED GP before region (seg04,seg05,seg06) vs NG before region (seg05,seg06,seg07) RT",segs = list(c('GP','seg04'),c('GP','seg05'),c('GP','seg06'), c('NG','seg05'),c('NG','seg06'),c('NG','seg07')), by_seg = FALSE, within_subject = TRUE)

rt_boxplot(data = final_SPR_CB_S, title = "FILTERED GP vs NG Crit+Posterior RT",segs = list(c('GP','seg07'), c('NG','seg08'),c('GP','seg08'),c('GP','seg09'), c('NG','seg09'),c('NG','seg10')), by_seg = FALSE, within_subject = TRUE)

rt_boxplot(data = final_SPR_CB_S, title = "FILTERED GP vs NG Crit+Posterior RT",segs = list(c('GP','seg07'), c('NG','seg08'),c('GP','seg08'),c('GP','seg09'), c('NG','seg09'),c('NG','seg10')), by_seg = FALSE, within_subject = FALSE)
