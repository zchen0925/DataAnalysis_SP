# load libraries
library(dplyr)
library(nortest)
library(effects)
install.packages("effects")
library(sjPlot)

#Analise individual de cada grupo

q_gp_sc <- subset(final_SPL_SC_Q, CONDITION == "GP" | CONDITION == "NG")

q_mv_sc <- subset(final_SPL_SC_Q, CONDITION == "MV" | CONDITION == "NMV")


write.csv(final_SPL_CB_Q, file = "final_SPL_CB_Q.csv")


  ylab("Scaled Temperature")

q_gp_new <- subset(q_analysis, CONDITION == "GP" | CONDITION == "NG")

q_mv_new <- subset(q_analysis, CONDITION == "MV" | CONDITION == "NMV")

#log to seg.post

hist(q_gp$RT_cons)

lillie.test(q_gp$RT_cons)

hist(q_mv$log)

mv_analysis$log <- log(mv_analysis$RT)

lillie.test(mv_analysis$log)

hist(mv_analysis$RT)
boxplot(crt.post$RT, range = 3)

summary(crt.post$RT)

Lsup_post <- 588 + (588 - 400)*1.5

crt.post2 <- crt.post %>% filter(RT < 870)

boxplot(q_gp$RT_cons, range = 3)

ans <- acc_rt_new %>% select(Group,CONDITION, Correct, ID)
ans$ct <- 1
mean_ind <- ans %>% 
  group_by(Group, CONDITION, ID) %>% 
  summarize(Accuracy = sum(Correct)/sum(ct),
            Subtotal = sum(ct),
            Correct = sum(Correct))


q_rt <- acc_rt_new %>%
  group_by(CONDITION, Group) %>% 
  summarize(Count = sum(count), 
            q.RT = sum(as.numeric(RT)),
            avg.RT = (sum(q.RT) / Count),
            sd = sd(RT))


q_rt_mv <- q_mv %>%
  group_by(Group, CONDITION) %>%
  summarize(count = n(),
            mean = mean(RT, na.rm = TRUE),
            std_dev = sd(RT, na.rm = TRUE),
            se = std.error(RT, na.rm = TRUE))

q_rt_mv

write.csv(q_rt_mv, file = "q_rt_mv.csv")


ggplot(q_gp, aes(x = CONDITION)) +
  geom_boxplot(aes(y = RT), color = "blue", fill = "gold") +
  theme_bw()


q_rt %>%
  group_by(Group, CONDITION)%>%
  ggplot(aes(CONDITION, mean)) +
  geom_errorbar(aes(ymin = std_dev, ymax = std_dev, color = CONDITION), size = 1) +
  geom_point(aes(color = CONDITION), size = 5)

q_rt %>%
  group_by(Group, CONDITION)%>%
  geom_boxplot(.,aes(x = CONDITION, y = mean))

mean_CI_RT %>%
  ggplot(aes(cond, mean)) +
  geom_errorbar(aes(ymin = CILow, ymax = CIHigh, color = cond), size = 1) +
  geom_point(aes(color = cond), size = 5)

geom_errorbar()
lillie.test(crt.post2$RT)

shapiro.test(crt.post2$RT)

crt.post2$log2 <- log(crt.post2$RT)

lillie.test(crt.post2$log2)

crt.post2$sqrt2 <- sqrt(crt.post2$RT)

lillie.test(crt.post2$sqrt2)

hist(crt.post2$log2)

mean(crt.post2$log2)

abline(v = 6.16, col = "red")

mod_segmpost <- lmer(log2 ~ Condition + (1|Item) + (1|ID), data = crt.post2, REML = FALSE)
jarque.bera.test(residuals(mod_segmpost))

qqnorm(residuals(mod_segmpost))
qqline(residuals(mod_segmpost))

summary(mod_segmpost)
mod_segmpost_null <- lmer(log2 ~ 1 + (1|Item) + (1|ID), data = crt.post2, REML = FALSE)
anova(mod_segmpost_null,mod_segmpost)
summary(mod_segmpost)


mv.seg <- subset(results_filtered, 
                 (
                   ((Condition == 'MOVE') & (Parameter == '8'))
                   | 
                     ((Condition == 'NO-MOVE') & (Parameter == '8'))
                 )
)

mv.seg$count <- 1
mv.seg.avg <- mv.seg %>%
  group_by(Condition) %>%
  summarize(Count  = sum(count),
            total.Time = sum(RT),
            seg.Avg = total.Time / Count)

mv.seg9 <- subset(results_filtered, 
                  (
                    ((Condition == 'MOVE') & (Parameter == '9'))
                    | 
                      ((Condition == 'NO-MOVE') & (Parameter == '9'))
                  )
)

mv.seg9$count <- 1
mv.seg9.avg <- mv.seg9 %>%
  group_by(Condition) %>%
  summarize(Count  = sum(count),
            total.Time = sum(RT),
            seg.Avg = total.Time / Count)

#log to mv.seg_8

hist(mv.seg$RT)

boxplot(mv.seg$RT, range = 3)

summary(mv.seg$RT)

Lsup_mv <- 568 + (568 - 399)*1.5

mv.seg2 <- mv.seg %>% filter(RT < 822)

hist(mv.seg2$RT)

boxplot(mv.seg2$RT, range = 3)

summary(mv.seg2$RT)

lillie.test(mv.seg2$RT)

shapiro.test(mv.seg2$RT)

mv.seg2$log2 <- log(mv.seg2$RT)

lillie.test(mv.seg2$log2)

mv.seg2$sqrt2 <- sqrt(mv.seg2$RT)

lillie.test(mv.seg2$sqrt2)

hist(mv.seg2$log2)

mean(mv.seg2$log2)

abline(v = 6.12, col = "red")

mod_mv.seg_8 <- lmer(log2 ~ Condition + (1|Item) + (1|ID), data = mv.seg2, REML = FALSE)
jarque.bera.test(residuals(mod_mv.seg_8))

qqnorm(residuals(mod_mv.seg_8))
qqline(residuals(mod_mv.seg_8))

summary(mod_mv.seg_8)
mod_mv.seg_null <- lmer(log2 ~ 1 + (1|Item) + (1|ID), data = mv.seg2, REML = FALSE)
anova(mod_mv.seg_null,mod_mv.seg_8)
summary(mod_segmpost)

#log to mv.seg9

hist(mv.seg9$RT)

boxplot(mv.seg9$RT, range = 3)

summary(mv.seg9$RT)

Lsup_mv9 <- 556 + (556 - 376)*1.5

mv.seg9_2 <- mv.seg9 %>% filter(RT < 826)

hist(mv.seg9_2$RT)

boxplot(mv.seg9_2$RT, range = 3)

summary(mv.seg9_2$RT)

lillie.test(mv.seg9_2$RT)

shapiro.test(mv.seg9_2$RT)

mv.seg9_2$log2 <- log(mv.seg9_2$RT)

lillie.test(mv.seg9_2$log2)

mv.seg9_2$sqrt2 <- sqrt(mv.seg9_2$RT)

lillie.test(mv.seg9_2$sqrt2)

hist(mv.seg9_2$log2)

mean(mv.seg9_2$log2)

abline(v = 6.09, col = "red")

mod_mv.seg_9 <- lmer(log2 ~ Condition + (1|Item) + (1|ID), data = mv.seg9_2, REML = FALSE)
jarque.bera.test(residuals(mod_mv.seg_9))

qqnorm(residuals(mod_mv.seg_9))
qqline(residuals(mod_mv.seg_9))

summary(mod_mv.seg_9)
mod_mv.seg9_null <- lmer(log2 ~ 1 + (1|Item) + (1|ID), data = mv.seg9_2, REML = FALSE)
anova(mod_mv.seg9_null,mod_mv.seg_9)

# Criar gráficos para reportar resultados de VDs numéricas.
plot(allEffects(mod.acur.gp), grid = T, multiline = T)
plot(allEffects(mod_segmcrit3))

plot(allEffects(mod_segmpost), grid = T, multiline = T)
plot(allEffects(mod_segmpost))

plot(allEffects(mod_mv.seg_8), grid = T, multiline = T)
plot(allEffects(mod_mv.seg_8))

plot(allEffects(mod_mv.seg_9), grid = T, multiline = T)
plot(allEffects(mod_mv.seg_9))

tab_model(mod.acur.gp)
tab_model(mod_segmpost)
tab_model(mod_mv.seg_8)
tab_model(mod_mv.seg_9)
