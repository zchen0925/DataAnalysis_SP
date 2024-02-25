#h3 Welch's 2-sample t-test in RT analysis

q_gp_cb_df <- subset(final_SPL_CB_Q[c("CONDITION", "Item", "ID","Correct", "RT", "RT+Duration")], CONDITION %in% c("GP", "NG"))
q_gp_sc_df <- subset(final_SPL_SC_Q[c("CONDITION", "Item", "ID","Correct","RT", "RT+Duration")], CONDITION %in% c("GP", "NG"))
q_gp_cb_df$Group = 0
q_gp_sc_df$Group = 1
rt_q_gp <- data.frame(rbind(q_gp_cb_df, q_gp_sc_df))
rt_q_gp$ln_RT <- log(rt_q_gp$RT+2000)
rt_q_gp$ln_RT_Duration <- log(rt_q_gp$RT.Duration)

mv_log <-subset(rt_q_mv, CONDITION == "MV")
nmv_log <-subset(rt_q_mv, CONDITION == "NMV")

mv_cb_log <-subset(mv_log, Group == "0")
mv_sc_log <-subset(mv_log, Group == "1")
nmv_cb_log <-subset(nmv_log, Group == "0")
nmv_sc_log <-subset(nmv_log, Group == "1")

mean(mv_sc_log$ln_RT)
sd(mv_sc_log$ln_RT)



mean(ng_log$ln_RT)
sd(ng_log$ln_RT)

ng_sc_log <-subset(ng_log, Group == "1")

mean(ng_sc_log$ln_RT)
sd(ng_sc_log$ln_RT)

print("t-test GP condition CB vs SC")
t.test(subset(rt_q_mv, CONDITION == 'MV' & Group == 0)$ln_RT, subset(rt_q_mv, CONDITION == 'MV' & Group == 1)$ln_RT)
print("t-test CB Group GP vs NG")
t.test(subset(rt_q_mv, CONDITION == 'MV' & Group == 0)$ln_RT, subset(rt_q_mv, CONDITION == 'NMV' & Group == 0)$ln_RT)
print("t-test NG condition CB vs SC")
t.test(subset(rt_q_mv, CONDITION == 'NMV' & Group == 0)$ln_RT, subset(rt_q_mv, CONDITION == 'NMV' & Group == 1)$ln_RT)
print("t-test SC Group GP vs NG")
t.test(subset(rt_q_mv, CONDITION == 'MV' & Group == 1)$ln_RT, subset(rt_q_mv, CONDITION == 'NMV' & Group == 1)$ln_RT)
print("CB GP SD")
sd(subset(rt_q_gp, CONDITION == 'GP' & Group == 0)$ln_RT)
print("CB NG SD")
sd(subset(rt_q_gp, CONDITION == 'NG' & Group == 0)$ln_RT)
print("SC GP SD")
sd(subset(rt_q_gp, CONDITION == 'GP' & Group == 1)$ln_RT)
print("SC NG SD")
sd(subset(rt_q_gp, CONDITION == 'NG' & Group == 1)$ln_RT)







print("t-test GP condition CB vs Blind")
t.test(gp_CB_Q$RT, gp_SC_Q$RT)

#Across Condition
print("t-test CB Group GP vs NGP")
t.test(gp_CB_Q$RT, ng_CB_Q$RT)

print("t-test NG condition CB vs Blind")
t.test(ng_CB_Q$RT, ng_SC_Q$RT)

print("t-test SC Group GP vs NGP")
t.test(gp_SC_Q$RT, ng_SC_Q$RT)