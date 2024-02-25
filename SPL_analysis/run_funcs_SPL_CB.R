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

#================SPL CB================
ALL_CONDS = c('GP', 'NG', 'MV', 'NMV', 'FL')
CONDS = c('GP', 'NG', 'MV', 'NMV')
ANALYSIS_CONDS <- as.vector(c('GP', 'NG', 'MV', 'NMV'))
#=======GP=======
GP_CRIT <- list(c('GP','seg06'), c('NG','seg07'))
GP_PRE1 <- list(c('GP','seg04'),  c('NG','seg05'))
GP_PRE2 <- list(c('GP','seg05'),  c('NG','seg06'))
GP_POST <- list(c('GP','seg07'), c('NG','seg08'))

#======MOVE=======
MV_CRIT1 <- list(c('MV','seg06'),c('NMV','seg06'))
MV_CRTI2<- list(c('MV','seg07'),c('NMV','seg07'))

raw_SPL_CB <- read_xlsx("FINAL_SPL_CB.xlsx")
raw_SPL_CB <- reorderConds(raw_SPL_CB, ALL_CONDS)

raw_SPL_CB_acc <- accuracy_by_cond(data = raw_SPL_CB, acc_conds = ALL_CONDS, within_subject = FALSE)
plot_acc(acc.data = raw_SPL_CB_acc, conds = ALL_CONDS, rounding_decimal = 2, title = "Raw Accuracy",within_subject = FALSE)
plot_acc(acc.data = raw_SPL_CB_acc, conds = CONDS, rounding_decimal = 2, title = "Raw Accuracy",within_subject = FALSE)
raw_SPL_CB_acc_ind <- accuracy_by_cond(data = raw_SPL_CB, acc_conds = ALL_CONDS, within_subject = TRUE)
plot_acc(acc.data = raw_SPL_CB_acc_ind, conds = c('FL'), rounding_decimal = 3, title = "Filler Accuracy within Condition",within_subject = TRUE, by_id = TRUE)
#plot_acc(acc.data = raw_acc_ind, conds = CONDS1, rounding_decimal = 1, title = "Individual Accuracy within Condition",within_subject = TRUE, by_id = TRUE)
plot_acc(acc.data = raw_SPL_CB_acc_ind, conds = c('GP', 'NG'), rounding_decimal = 1, title = "GP, NG Accuracy within Condition",within_subject = TRUE, by_id = TRUE)
plot_acc(acc.data = raw_SPL_CB_acc_ind, conds = c('MV', 'NMV'), rounding_decimal = 1, title = "MV, NMV Accuracy within Condition",within_subject = TRUE, by_id = TRUE)

resp_boxplot(data = raw_SPL_CB, title = "Raw GP Question Response Time", conds = c("GP"),within_subject = TRUE)
resp_boxplot(data = raw_SPL_CB, title = "Raw NG Question Response Time", conds = c("NG"),within_subject = TRUE)
resp_boxplot(data = raw_SPL_CB, title = "Raw GP, NGP Question Response Time", conds = c("GP","NG"),within_subject = FALSE)
resp_boxplot(data = raw_SPL_CB, title = "Raw GP, NGP Question Response Time", conds = c("GP","NG"),within_subject = TRUE)

resp_boxplot(data = raw_SPL_CB, title = "Raw MV Question Response Time", conds = c("MV"),within_subject = TRUE)
resp_boxplot(data = raw_SPL_CB, title = "Raw NMV Question Response Time", conds = c("NMV"),within_subject = TRUE)
resp_boxplot(data = raw_SPL_CB, title = "Raw MV, NMV Question Response Time", conds = c("MV","NMV"),within_subject = FALSE)
resp_boxplot(data = raw_SPL_CB, title = "Raw MV, NMV Question Response Time", conds = c("MV","NMV"),within_subject = TRUE)

resp_boxplot(data = raw_SPL_CB, title = "Raw Question Response Time", conds = c('GP', 'NG', 'MV', 'NMV', 'FL'), order = c('GP', 'NG', 'MV', 'NMV', 'FL'), within_subject = TRUE)
resp_boxplot(data = raw_SPL_CB, title = "Raw Question Response Time", conds = c('GP', 'NG', 'MV', 'NMV', 'FL'), order = c('GP', 'NG', 'MV', 'NMV', 'FL'), within_subject = FALSE)

#Q.STEP1
#==========Question Step1 8000ms=========
resp_boxplot(data = raw_SPL_CB, title = "Raw Question Response Time", conds = c('GP', 'NG', 'MV', 'NMV', 'FL'), order = c('GP', 'NG', 'MV', 'NMV', 'FL'), within_subject = FALSE)
Q_step1_8s_SPL_CB <- subset(raw_SPL_CB, (Seg  == 'question' & RT <= 8000) | Seg != 'question')

excl_8s <- perc_excluded(raw_data = raw_SPL_CB, filtered_data = Q_step1_8s_SPL_CB, conds = c("GP","NG","MV","NMV"), within_subject = FALSE)
excl_8s_indv <- perc_excluded(raw_data = raw_SPL_CB, filtered_data = Q_step1_8s_SPL_CB, conds = c("GP","NG",'MV','NMV'),within_subject = TRUE)

plot_perc_excluded(raw_data = raw_SPL_CB, filtered_data = Q_step1_8s_SPL_CB, conds = c("GP","NG"),remain_or_lost = "remain",title = "GP, NG filtered Question RT <= 8s",within_subject = TRUE)
plot_perc_excluded(raw_data = raw_SPL_CB, filtered_data = Q_step1_8s_SPL_CB, conds = c("GP","NG"),remain_or_lost = "remain",title = "GP, NG filtered Question RT <= 8s", within_subject = FALSE)

plot_perc_excluded(raw_data = raw_SPL_CB, filtered_data = Q_step1_8s_SPL_CB, conds = c("MV","NMV"),remain_or_lost = "remain",title = "MV, NMV filtered Question RT <= 8s",within_subject = TRUE)
plot_perc_excluded(raw_data = raw_SPL_CB, filtered_data = Q_step1_8s_SPL_CB, conds = c("MV","NMV"),remain_or_lost = "remain",title = "MV, NMV data loss Question RT <= 8s",within_subject = FALSE)

plot_perc_excluded(raw_SPL_CB, Q_step1_8s_SPL_CB,conds=c("GP","NG","MV","NMV"),remain_or_lost = "remain",title="4 Exp Conds filtered Question RT <= 8s",within_subject = TRUE, within_cond = TRUE)
plot_perc_excluded(raw_SPL_CB, Q_step1_8s_SPL_CB,conds=c("GP","NG","MV","NMV"),remain_or_lost = "remain",title="4 Exp Conds filtered Question RT <= 8s",within_subject = FALSE, within_cond = TRUE)

resp_boxplot(data = Q_step1_8s_SPL_CB, title = "<=8S Question Response Time", conds = c('GP', 'NG', 'MV', 'NMV', 'FL'), order = c('GP', 'NG', 'MV', 'NMV', 'FL'), within_subject = TRUE)
resp_boxplot(data = Q_step1_8s_SPL_CB, title = "<=8S Question Response Time", conds = c('GP', 'NG', 'MV', 'NMV', 'FL'), order = c('GP', 'NG', 'MV', 'NMV', 'FL'), within_subject = FALSE)

#Q.STEP2
#QStep2===============replace question RTs that are 3SD above or below with the mean RT within subject within condition===============
qSD_list <- list(list('GP','question'),list('NG','question'),list('MV','question'),list('NMV','question'))
qSD_filters <- generate_filters(data = Q_step1_8s_SPL_CB,filter_segs = qSD_list, range = 3, method="sd")

qSD_replaced <- data.frame(matrix(ncol = ncol(Q_step1_8s_SPL_CB), nrow = 0))
colnames(qSD_replaced) <- colnames(Q_step1_8s_SPL_CB)
Q_step2_3SD_SPL_CB <- Q_step1_8s_SPL_CB
for (q_filt in qSD_filters){
  list <- replaceRT(Q_step2_3SD_SPL_CB, q_filt, 3,replace_w_mean = TRUE)
  Q_step2_3SD_SPL_CB <- list$data
  qSD_replaced <- rbind(qSD_replaced, list$replaced)
}

excl_q3SD <- perc_excluded(raw_data = raw_SPL_CB, filtered_data = qSD_replaced, conds = c("GP","NG","MV","NMV"), within_subject = FALSE)
excl_q3SD_indv <- perc_excluded(raw_data = raw_SPL_CB, filtered_data = qSD_replaced, conds = c("GP","NG",'MV','NMV'),within_subject = TRUE)

plot_perc_excluded(raw_data = Q_step1_8s_SPL_CB, filtered_data = qSD_replaced, conds = c("GP","NG"),remain_or_lost = "lost",title = "% GP, NG Question RTs Replaced with the Mean", within_subject = TRUE)
plot_perc_excluded(raw_data = Q_step1_8s_SPL_CB, filtered_data = qSD_replaced, conds = c("GP","NG"),remain_or_lost = "lost",title = "% GP, NG Question RTs Replaced with the Mean",within_subject = FALSE)

plot_perc_excluded(raw_data = Q_step1_8s_SPL_CB, filtered_data = qSD_replaced, conds = c("MV","NMV"),remain_or_lost = "lost",title = "% MV, NMV RTs Replaced with the Mean",within_subject = TRUE)
plot_perc_excluded(raw_data = Q_step1_8s_SPL_CB, filtered_data = qSD_replaced, conds = c("MV","NMV"),remain_or_lost = "lost",title = "% MV, NMV Question RTs Replaced with the Mean",within_subject = FALSE)

plot_perc_excluded(raw_data = Q_step1_8s_SPL_CB, filtered_data = qSD_replaced, conds = c("GP","NG","MV","NMV"),remain_or_lost = "lost",title = "GP, NG, MV, NMV Question RT beyond 3SD that were replaced with the mean",within_subject = FALSE)
qSD_replaced <- rbind(qSD_replaced, subset(raw_SPL_CB, (Seg == "question" & RT > 8000)))
excl_q_all <- perc_excluded(raw_data = raw_SPL_CB, filtered_data = qSD_replaced, conds = c("GP","NG","MV","NMV"), within_subject = FALSE)
excl_q_all_indv <- perc_excluded(raw_data = raw_SPL_CB, filtered_data = qSD_replaced, conds = c("GP","NG",'MV','NMV'),within_subject = TRUE)

plot_perc_excluded(raw_data = raw_SPL_CB, filtered_data = qSD_replaced, conds = c("GP","NG","MV","NMV"),remain_or_lost = "lost",title = "Final filtered Questions RTs\nDenominator: Raw data\nstep 2(<= 8s)\nstep 3(within 3SD)",within_subject = FALSE)

resp_boxplot(data = Q_step2_3SD_SPL_CB, title = "Final Filtered Question Response Time", conds = c('GP', 'NG'), order = c('GP', 'NG'), within_subject = TRUE)

resp_boxplot(data = Q_step2_3SD_SPL_CB, title = "Final Filtered Question Response Time", conds = c('MV', 'NMV'), order = c('MV', 'NMV'), within_subject = TRUE)

resp_boxplot(data = Q_step2_3SD_SPL_CB, title = "Final Filtered Question Response Time", conds = c('GP', 'NG', 'MV', 'NMV'), order = c('GP', 'NG', 'MV', 'NMV'), within_subject = FALSE)

#S.STEP1
#========Sstep1: remove segment RT > 3000ms========
S_step1_3s_SPL_CB <- subset(raw_SPL_CB, (Seg  != 'question' & Seg != 'combined_crit' & RT <= 3000) | (Seg == 'combined_crit' & RT <= 6000))

excl_s_3s <- perc_excluded(raw_data = raw_SPL_CB, filtered_data = S_step1_3s_SPL_CB, conds = c("GP","NG",'MV','NMV'), isQuestion=FALSE, within_subject = FALSE)
excl_s_3s_ind <- perc_excluded(raw_data = raw_SPL_CB, filtered_data = S_step1_3s_SPL_CB, conds = c("GP","NG",'MV','NMV'), isQuestion=FALSE,within_subject = TRUE)

plot_perc_excluded(raw_data = raw_SPL_CB, filtered_data = S_step1_3s_SPL_CB, conds = c("GP","NG"),remain_or_lost = "remain",title = "GP, NG after dropping individual segments > 3.0s", dropped_single_seg = TRUE, within_subject = TRUE)
plot_perc_excluded(raw_data = raw_SPL_CB, filtered_data = S_step1_3s_SPL_CB, conds = c("GP","NG"),remain_or_lost = "remain",title = "GP, NG after dropping individual segments > 3.0s", dropped_single_seg = TRUE,within_subject = FALSE)

plot_perc_excluded(raw_data = raw_SPL_CB, filtered_data = S_step1_3s_SPL_CB, conds = c("MV","NMV"),remain_or_lost = "remain",title = "MV, NMV after dropping individual segments > 3.0s",dropped_single_seg = TRUE,within_subject = TRUE)
plot_perc_excluded(raw_data = raw_SPL_CB, filtered_data = S_step1_3s_SPL_CB, conds = c("MV","NMV"),remain_or_lost = "remain",title = "MV, NMV after dropping individual segments > 3.0s",dropped_single_seg = TRUE,within_subject = FALSE)

plot_perc_excluded(raw_data = raw_SPL_CB, filtered_data = S_step1_3s_SPL_CB, conds = c("GP","NG","MV","NMV"), remain_or_lost = "remain",title = "Experimental Conditions after dropping individual Segments RT > 3.0s",dropped_single_seg = TRUE,within_subject = FALSE)

#S.STEP2
#Sstep2: replace individual segment RT beyond 3SD with the mean within subject within segment ================================
segs_list <- list(list('GP','seg01'),list('GP','seg02'),list('GP','seg03'),list('GP','seg04'),list('GP','seg05'),list('GP','seg06'),list('GP','seg07'),list('GP','combined_crit'),  
                  list('NG','seg01'),  list('NG','seg02'),list('NG','seg03'),  list('NG','seg04'),list('NG','seg05'),  list('NG','seg06'),list('NG','seg07'),  list('NG','seg08'),list('NG','combined_crit'),  
                  list('MV','seg01'),  list('MV','seg02'),list('MV','seg03'),  list('MV','seg04'),list('MV','seg05'),  list('MV','seg06'),list('MV','seg07'),  list('MV','seg08'),list('MV','combined_crit'),
                  list('NMV','seg01'),  list('NMV','seg02'),list('NMV','seg03'),  list('NMV','seg04'),list('NMV','seg05'),  list('NMV','seg06'),list('NMV','seg07'),  list('NMV','seg08'),list('NMV','combined_crit'))
seg_filters <- generate_filters(data = S_step1_3s_SPL_CB,filter_segs = segs_list, range = 3, method="sd")

segs_replaced <- data.frame(matrix(ncol = ncol(S_step1_3s_SPL_CB), nrow = 0))
colnames(segs_replaced) <- colnames(S_step1_3s_SPL_CB)
S_step2_3SD_SPL_CB <- S_step1_3s_SPL_CB
for (s_f in seg_filters){
  s_f <- na.omit(s_f)
  list <- replaceRT(S_step2_3SD_SPL_CB, s_f, 3, replace_w_mean = TRUE)
  S_step2_3SD_SPL_CB <- list$data
  segs_replaced <- rbind(segs_replaced, list$replaced)
}
excl_s_3SD <- perc_excluded(raw_data = raw_SPL_CB, filtered_data = segs_replaced, conds = c("GP","NG",'MV','NMV'), isQuestion=FALSE, within_subject = FALSE)
excl_s_3SD_ind <- perc_excluded(raw_data = raw_SPL_CB, filtered_data = segs_replaced, conds = c("GP","NG",'MV','NMV'), isQuestion=FALSE,within_subject = TRUE)

segs_replaced <- rbind(segs_replaced, subset(raw_SPL_CB, (Seg != 'question' & Seg != 'combined_crit' & RT > 3000)| (Seg == 'combined_crit' & RT > 6000)))
excl_s_all <- perc_excluded(raw_data = raw_SPL_CB, filtered_data = segs_replaced, conds = c("GP","NG",'MV','NMV'), isQuestion=FALSE, within_subject = FALSE)
excl_s_all_ind <- perc_excluded(raw_data = raw_SPL_CB, filtered_data = segs_replaced, conds = c("GP","NG",'MV','NMV'), isQuestion=FALSE,within_subject = TRUE)

plot_perc_excluded(raw_data = raw_SPL_CB, filtered_data = segs_replaced, digits = 0.1,conds = c("GP","NG"),remain_or_lost = "lost",title = "% GP, NG segment RTs replaced with the mean", dropped_single_seg = TRUE,within_subject = TRUE)
plot_perc_excluded(raw_data = raw_SPL_CB, filtered_data = segs_replaced, digits = 0.1, conds = c("MV","NMV"),remain_or_lost = "lost",title = "% MV, NMV segment RTs replaced with the mean",dropped_single_seg = TRUE,within_subject = TRUE)
plot_perc_excluded(raw_data = S_step1_3s_SPL_CB, filtered_data = segs_replaced, conds = c("GP","NG","MV","NMV"),remain_or_lost = "lost",title = "% Indv segs with RTs beyond +- 3SD replaced",dropped_single_seg = TRUE,within_subject = FALSE)

final_SPL_CB_Q <- subset(Q_step2_3SD_SPL_CB, Seg == "question")
final_SPL_CB_S <- S_step2_3SD_SPL_CB



#=======Adding Offset Segment=======#
####add the 2000ms offset for log-transform, not here
# final_SPL_CB_Q$RT_o <- final_SPL_CB_Q$RT
# final_SPL_CB_Q$RT <- final_SPL_CB_Q$RT + 2000
# final_SPL_CB_S$RT_o <- final_SPL_CB_S$RT
# final_SPL_CB_S$RT <- final_SPL_CB_S$RT + 2000


#After All filtering steps:===================
filt_SPL_CB_acc <- accuracy_by_cond(data = Q_step2_3SD_SPL_CB, acc_conds = ALL_CONDS, within_subject = FALSE)
filt_SPL_CB_acc_ind <- accuracy_by_cond(data = Q_step2_3SD_SPL_CB, acc_conds = ALL_CONDS, within_subject = TRUE)
plot_acc(acc.data = filt_SPL_CB_acc_ind, conds = c('GP', 'NG'), rounding_decimal = 2, title = "Filtered GP, NG Accuracy within Condition",within_subject = TRUE, by_id = TRUE)
plot_acc(acc.data = filt_SPL_CB_acc_ind, conds = c('MV', 'NMV'), rounding_decimal = 2, title = "Filtered MV, NMV Accuracy within Condition",within_subject = TRUE, by_id = TRUE)

plot_acc(acc.data = filt_SPL_CB_acc, conds = c('GP', 'NG','MV','NMV'), rounding_decimal = 2, title = "Filtered Accuracy within Condition",within_subject = FALSE, by_id = TRUE)

crit.rt <- crit_rt(data=S_step2_3SD_SPL_CB,segs = GP_CRIT, by_seg = TRUE, within_subject = TRUE)
plot_crit_rt(data = S_step2_3SD_SPL_CB,segs = list(c('GP','seg06'), c('NG','seg07')), by_seg = TRUE, title = "Filtered GP, NG critical segs RT", within_subject = TRUE)

plot_crit_rt(data = S_step2_3SD_SPL_CB,segs = list(c('GP','seg06'), c('NG','seg07')), by_seg = TRUE, title = "FILTERED GP, NG critical segs RT", within_subject = TRUE)

