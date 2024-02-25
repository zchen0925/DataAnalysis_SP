# load libraries
library(tidyverse)
library(stringr)
library(ggplot2)
library(scales)
library(readxl)
library(dplyr)
library("plotrix")      
#detach(package:plyr)

# round all numeric variables
# x: data frame 
# digits: number of digits to round
round_df <- function(x, digits) {
  numeric_columns <- sapply(x, class) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

reorderConds <- function(df, order){
  df$CONDITION <- factor(df$CONDITION, levels = order)
  return(df)
}

#PLOT DISTRIBUTION OF QUESTION RESPONSE TIME
resp_boxplot <- function(data, title, conds, order = conds, within_subject = TRUE){
  rt <- subset(data, CONDITION %in% conds & Seg == 'question')
  #rt <- reorderConds(rt)
  #rt <- transform(rt, CONDITION=reorder(CONDITION, order)) 
  if (within_subject){
    ggplot(data = rt, aes(fill = CONDITION, x = ID, y = RT)) +
      geom_boxplot(aes(fill = CONDITION), notch = FALSE) +
      ggtitle(title) +
      scale_fill_brewer(palette = "Pastel1") +
      stat_summary(fun.y=mean, aes(fill = CONDITION),geom="point", shape=23, size=2) 
  } else {
    ggplot(data = rt, aes(x = CONDITION, fill = CONDITION, y = RT)) +
      geom_boxplot(aes(fill = CONDITION),notch = TRUE) +
      ggtitle(title) +
      scale_fill_brewer(palette = "Pastel1") +
      stat_summary(fun.y=mean, aes(fill = CONDITION), geom="point", shape=23, size=2) 
  }
}

#CALCULATE PERCENTAGE OF DATA LOSS
perc_excluded <- function(raw_data, filtered_data, conds, isQuestion = TRUE, within_subject =TRUE){
  if (within_subject){
    ind_perc <- data.frame(matrix(ncol = 6, nrow = 0))
    
    for (id in unique(raw_data$ID)){
      for (cond in conds){
        if (isQuestion){
          prev = nrow(subset(raw_data, ID == id & CONDITION == cond & Seg == "question"))
          filt = nrow(subset(filtered_data, ID == id & CONDITION == cond & Seg == "question"))          
        } else {
          prev = nrow(subset(raw_data, ID == id & CONDITION == cond & Seg != "question"))
          filt = nrow(subset(filtered_data, ID == id & CONDITION == cond & Seg != "question"))
        }
        row <- c(id, cond, prev, filt, (filt/prev), (1-(filt/prev)))
        ind_perc <- rbind(ind_perc, row)
      } 
    }
    x <- c("ID","CONDITION", "Previous", "Filtered","Remaining","Lost")
    colnames(ind_perc) <- x
    ind_perc$Remaining <- as.numeric(ind_perc$Remaining)
    ind_perc$Lost <- as.numeric(ind_perc$Lost)
    return(ind_perc)
  } else {
    perc <- data.frame(matrix(ncol = 5, nrow = 0))
    for (cond in conds){
      if (isQuestion){
        prev = nrow(subset(raw_data, CONDITION == cond & Seg == "question"))
        filt = nrow(subset(filtered_data, CONDITION == cond & Seg == "question"))          
      } else {
        prev = nrow(subset(raw_data, CONDITION == cond & Seg != "question"))
        filt = nrow(subset(filtered_data, CONDITION == cond & Seg != "question"))
      }
      row <- c(cond, prev, filt, (filt/prev), (1-(filt/prev)))
      perc <- rbind(perc, row)
    }
    x <- c("CONDITION", "Previous", "Filtered","Remaining","Lost")
    colnames(perc) <- x
    perc$Remaining <- as.numeric(perc$Remaining)
    perc$Lost <- as.numeric(perc$Lost)
    return(perc)
  }
}

#PLOT PERCENTAGE OF DATA LOSS
plot_perc_excluded <- function(raw_data, filtered_data, conds, digits=0.01, remain_or_lost, title, within_subject =TRUE, within_cond=TRUE, dropped_single_seg=FALSE){
  if (within_subject){
    ind_perc <- data.frame(matrix(ncol = 6, nrow = 0))
    if (within_cond){
      for (id in unique(raw_data$ID)){
        for (cond in conds){
          if (dropped_single_seg){          
            prev = nrow(subset(raw_data, ID == id & CONDITION == cond & Seg != 'question'))
            filt = nrow(subset(filtered_data, ID == id & CONDITION == cond & Seg != 'question'))
            row <- c(id, cond, prev, filt, (filt/prev), (1-(filt/prev)))
            ind_perc <- rbind(ind_perc, row)
          } else {
            prev = nrow(subset(raw_data, ID == id & CONDITION == cond & Seg == "question"))
            filt = nrow(subset(filtered_data, ID == id & CONDITION == cond & Seg == "question"))
            row <- c(id, cond, prev, filt, (filt/prev), (1-(filt/prev)))
            ind_perc <- rbind(ind_perc, row)            
          }
        } 
      }
      x <- c("ID","CONDITION", "Previous", "Filtered","Remaining","Lost")
      colnames(ind_perc) <- x
      ind_perc$Remaining <- as.numeric(ind_perc$Remaining)
      ind_perc$Lost <- as.numeric(ind_perc$Lost)
      if (remain_or_lost=="remain"){
        ggplot(data = ind_perc)+
          geom_bar(position="dodge", stat = "identity", aes(x = ID, y = Remaining, fill  = CONDITION), color = "black") +
          scale_fill_hue(c=45, l=80) +
          #scale_fill_brewer(palette = "Pastel1") +
          ggtitle(title)+
          geom_text(aes(x=ID,y=Remaining,fill=CONDITION,label=(label_percent(accuracy=digits)(Remaining))), position=position_dodge(width=1), vjust=-0.25) 
      } else {
        ggplot(data = ind_perc)+
          geom_bar(position="dodge", stat = "identity", aes(x = ID, y = Lost, fill  = CONDITION), color = "black") +
          scale_fill_hue(c=45, l=80)+
          #scale_fill_brewer(palette = "Pastel1") +
          ggtitle(title)+
          geom_text(aes(x=ID,y=Lost,fill=CONDITION,label=(label_percent(accuracy=digits)(Lost))), position=position_dodge(width=1), vjust=-0.25) 
      } 
    } else {
      for (id in unique(raw_data$ID)){
        for (cond in conds){
          if (dropped_single_seg){          
            prev = nrow(subset(raw_data, ID == id & CONDITION == cond & Seg != "question"))
            filt = nrow(subset(filtered_data, ID == id & CONDITION == cond) & Seg != 'question')
            row <- c(id, cond, prev, filt, (filt/prev), (1-(filt/prev)))
            ind_perc <- rbind(ind_perc, row)
          } else {
            prev = nrow(subset(raw_data, ID == id & CONDITION == cond & Seg == "question"))
            filt = nrow(subset(filtered_data, ID == id & CONDITION == cond & Seg == "question"))
            row <- c(id, cond, prev, filt, (filt/prev), (1-(filt/prev)))
            ind_perc <- rbind(ind_perc, row)            
          }
        } 
      }
      x <- c("ID","CONDITION", "Previous", "Filtered","Remaining","Lost")
      colnames(ind_perc) <- x
      ind_perc$Remaining <- as.numeric(ind_perc$Remaining)
      ind_perc$Lost <- as.numeric(ind_perc$Lost)
      ind_perc <- ind_perc %>%
        group_by(ID) %>%
        summarize(Previous=sum(as.numeric(Previous)),
                  Filtered=sum(as.numeric(Filtered)),
                  Remaining=(Filtered/Previous),
                  Lost=1-Remaining)
      if (remain_or_lost=="remain"){
        ggplot(data = ind_perc)+
          geom_bar(position="dodge", stat = "identity", aes(x = ID, y = Remaining, fill=ID), color = "black") +
          scale_fill_hue(c=45, l=80) +
          #scale_fill_brewer(palette = "Pastel1") +
          ggtitle(title)+
          geom_text(aes(x=ID,y=Remaining,label=(label_percent(accuracy=digits)(Remaining))), position=position_dodge(width=1), vjust=-0.25) 
      } else {
        ggplot(data = ind_perc)+
          geom_bar(position="dodge", stat = "identity", aes(x = ID, y = Lost,fill = ID), color = "black") +
          scale_fill_hue(c=45, l=80)+
          #scale_fill_brewer(palette = "Pastel1") +
          ggtitle(title)+
          geom_text(aes(x=ID,y=Lost,label=(label_percent(accuracy=digits)(Lost))), position=position_dodge(width=1), vjust=-0.25) 
      }
    }
  } else {
    perc <- data.frame(matrix(ncol = 5, nrow = 0))
    for (cond in conds){
      if (dropped_single_seg){          
        prev = nrow(subset(raw_data, CONDITION == cond & Seg != "question"))
        filt = nrow(subset(filtered_data, CONDITION == cond & Seg != "question"))
        row <- c(cond, prev, filt, (filt/prev), (1-(filt/prev)))
        perc <- rbind(perc, row)
      } else {
        prev = nrow(subset(raw_data, CONDITION == cond & Seg == "question"))
        filt = nrow(subset(filtered_data, CONDITION == cond & Seg == "question"))
        row <- c(cond, prev, filt, (filt/prev), (1-(filt/prev)))
        perc <- rbind(perc, row)            
      }
    }
    x <- c("CONDITION", "Previous", "Filtered","Remaining","Lost")
    colnames(perc) <- x
    perc$Remaining <- as.numeric(perc$Remaining)
    perc$Lost <- as.numeric(perc$Lost)
    perc %>%
      mutate(CONDITION = factor(CONDITION, levels = conds)) %>%
      arrange(CONDITION)
    perc <- reorderConds(perc, order = conds)
    if (remain_or_lost=="remain"){
      ggplot(data = perc)+
        geom_bar(position="dodge", stat = "identity", aes(x = CONDITION, y = Remaining, fill  = CONDITION), color = "black") +
        scale_fill_brewer(palette = "Pastel1") +
        ggtitle(title)+
        geom_text(aes(x=factor(CONDITION, conds),y=Remaining,label=(label_percent(accuracy=digits)(Remaining))), position=position_dodge(width=1), vjust=-0.25) 
    } else {
      ggplot(data = perc)+
        geom_bar(position="dodge", stat = "identity", aes(x = CONDITION, y = Lost, fill  = CONDITION), color = "black") +
        scale_fill_brewer(palette = "Pastel1") +
        ggtitle(title)+
        geom_text(aes(x=factor(CONDITION, conds),y=Lost,label=(label_percent(accuracy=digits)(Lost))), position=position_dodge(width=1), vjust=-0.25) 
    }
  }
}

#function calculates accuracy by condition, optionally within subject
#@acc_conds: a list of conditions for which accuracy will be calculated
#@return: a dataframe summarizing accuracies within condition and optionally within subject
accuracy_by_cond <- function(data, acc_conds = ALL_CONDS, within_subject = TRUE){
  ans <- subset(data, Seg == 'question' & CONDITION %in% acc_conds)
  ans <- ans %>% select(CONDITION, Correct, ID)
  ans$ct <- 1
  if (within_subject == TRUE){
    cond.acc <- ans %>% 
      group_by(ID, CONDITION) %>% 
      summarize(Accuracy = sum(Correct)/sum(ct),
                Subtotal = sum(ct),
                Correct = sum(Correct))
  } else {
    temp <- ans %>% 
      group_by(ID, CONDITION) %>% 
      summarize(Accuracy = sum(Correct)/sum(ct))
    sd_acc <- c()
    se_acc <- c()
    for (cond in unique(temp$CONDITION)){
      sd <- sd(subset(temp, CONDITION == cond)$Accuracy)
      sd_acc <- c(sd_acc, sd)
      se <- std.error(subset(temp, CONDITION == cond)$Accuracy)
      se_acc <- c(se_acc, se)
    }
    cond.acc <- ans %>% 
      group_by(CONDITION) %>% 
      summarize(Accuracy = sum(Correct)/sum(ct),
                Subtotal = sum(ct),
                Correct = sum(Correct))
    cond.acc$SD <- sd_acc
    cond.acc$se <- se_acc
  }
  cond.acc <- round_df(cond.acc, 4)
  cond.acc$perc <- label_percent()(cond.acc$Accuracy)
  cond.acc$CONDITION <- factor(cond.acc$CONDITION, levels = acc_conds)
  return(cond.acc)
}

#PLOT HISTOGRAM OF ACCURACY
plot_acc <- function(acc.data, conds, rounding_decimal = 3, title, within_subject = TRUE, by_id = FALSE){
  cond.acc <- subset(acc.data, CONDITION %in% conds)
  cond.acc <- round_df(cond.acc, (rounding_decimal+1))
  if (within_subject == TRUE){
    if (by_id){
      ggplot(data = cond.acc)+
        geom_bar(position="dodge", stat = "identity", aes(x = ID, y = Accuracy, fill=CONDITION), color = "black")+
        ggtitle(title) +
        scale_fill_brewer(palette = "Pastel1") +
        #scale_fill_hue(c=45, l=80)+
        geom_text(aes(x=ID, y=Accuracy, fill=CONDITION, label=(Accuracy*100)), position=position_dodge(width=1), vjust=-0.25) 
    } else {
      ggplot(data = cond.acc)+
        geom_bar(position="dodge", stat = "identity", aes(x = CONDITION, y = Accuracy, fill=ID), color = "black")+
        ggtitle(title) +
        scale_fill_brewer(palette = "Pastel1") +
        #scale_fill_hue(c=45, l=80)+
        geom_text(aes(x=CONDITION, y=Accuracy, fill=ID, label=(Accuracy*100)), position=position_dodge(width=1), vjust=-0.25)  
    }
  } else {
    ggplot(data = cond.acc)+
      geom_bar(position="dodge", stat = "identity", aes(x = CONDITION, y = Accuracy, fill=CONDITION), color = "black")+
        ggtitle(title) +
        scale_fill_brewer(palette = "Pastel1") +
        #geom_errorbar(aes(x=CONDITION, ymin=Accuracy-SD, ymax=Accuracy+SD), width=.2)+
        geom_errorbar(aes(x=CONDITION, ymin=Accuracy-se, ymax=Accuracy+se), width=.2)+
        geom_text(aes(x=CONDITION, Accuracy,label=(Accuracy*100)), position=position_dodge(width=1), vjust=-0.25)
  }
}


#CALCULATE CRITICAL SEGMENT READING TIME
crit_rt <- function(data, segs = CRIT_SEGMENTS, by_seg = TRUE, within_subject = TRUE){
  rt <- data.frame(matrix(ncol = ncol(data), nrow = 0))
  for (seg in segs){
    cond <- seg[1]
    seg <- seg[2]
    df <- subset(data, CONDITION == cond & Seg == seg)
    rt <- rbind(rt, df)
  }
  rt$count <- 1
  if (within_subject == TRUE){
    if (by_seg == TRUE){
      seg.rt <- rt %>%
        group_by(ID, CONDITION, Seg) %>% 
        summarize(Count = sum(count), 
                  seg.RT = sum(as.numeric(RT)),
                  avg.RT = (sum(seg.RT) / Count),
                  sd = sd(RT))
    } else {
      seg.rt <- rt %>%
        group_by(ID, CONDITION) %>% 
        summarize(Count = sum(count), 
                  seg.RT = sum(as.numeric(RT)),
                  avg.RT = (sum(seg.RT) / Count),
                  sd = sd(RT)) 
    }
  } else {
    if (by_seg == TRUE){
      seg.rt <- rt %>%
        group_by(CONDITION, Seg) %>% 
        summarize(Count = sum(count), 
                  seg.RT = sum(as.numeric(RT)),
                  avg.RT = (sum(seg.RT) / Count),
                  sd = sd(RT))
    } else {
      seg.rt <- rt %>%
        group_by(CONDITION) %>% 
        summarize(Count = sum(count), 
                  seg.RT = sum(as.numeric(RT)),
                  avg.RT = (sum(seg.RT) / Count),
                  sd = sd(RT)) 
    }
  }
  seg.rt <- round_df(seg.rt, 1)
  #seg.rt$Condition <- factor(seg.rt$Condition, levels = SPR_CONDS)
  return(seg.rt)
}

#BOXPLOT OF CRITICAL SEGMENT READING TIME
rt_boxplot <- function(aggregated = FALSE, data, title, segs = ANALYSIS, by_seg = TRUE, within_subject = TRUE){
  if (aggregated){
    if (within_subject){
      if (by_seg){
        ggplot(data = data, aes(fill = CONDITION, x = ID, y = avg.RT)) +
          geom_boxplot(notch = TRUE) +
          ggtitle(title) +
          scale_fill_brewer(palette = "Pastel1") +
          stat_summary(fun.y=mean, geom="point", shape=23, size=2) 
      } else {
        ggplot(data = data, aes(fill = CONDITION, x = ID, y = avg.RT)) +
          geom_boxplot(notch = TRUE) +
          ggtitle(title) +
          scale_fill_brewer(palette = "Pastel1") +
          stat_summary(fun.y=mean, geom="point", shape=23, size=2) 
      }
    } else{
      if (by_seg){
      ggplot(data = data, aes(fill = CONDITION, x = Seg, y = avg.RT)) +
        geom_boxplot(notch = TRUE) +
        ggtitle(title) +
        scale_fill_brewer(palette = "Pastel1") +
        stat_summary(fun.y=mean, geom="point", shape=23, size=2) 
    } else {
      ggplot(data = data, aes(x = CONDITION, y = avg.RT, fill = CONDITION)) +
        geom_boxplot(notch = TRUE) +
        ggtitle(title) +
        stat_summary(fun.y=mean, geom="point", shape=23, size=2)  
      }
    }
  } else{
    rt <- data.frame(matrix(ncol = ncol(data), nrow = 0))
    for (seg in segs){
      cond <- seg[1]
      seg <- seg[2]
      df <- subset(data, CONDITION == cond & Seg == seg)
      rt <- rbind(rt, df)
    }
    df$Seg=paste(df$CONDITION,df$Seg)
    df <- round_df(df, 2)
    if (within_subject){
      if (by_seg){
        ggplot(data = rt, aes(fill = CONDITION, x = ID, y = RT)) +
          geom_boxplot(notch = TRUE) +
          ggtitle(title) +
          scale_fill_brewer(palette = "Pastel1") +
          stat_summary(fun.y=mean, geom="point", shape=23, size=2) 
      } else {
        ggplot(data = rt, aes(fill = CONDITION, x = ID, y = RT)) +
          geom_boxplot(notch = TRUE) +
          ggtitle(title) +
          scale_fill_brewer(palette = "Pastel1") +
          stat_summary(fun.y=mean, geom="point", shape=23, size=2) 
      }
    } else {
      if (by_seg){
        ggplot(data = rt, aes(fill = CONDITION, x = CONDITION, y = RT)) +
          geom_boxplot(notch = TRUE) +
          ggtitle(title) +
          scale_fill_brewer(palette = "Pastel1") +
          stat_summary(fun.y=mean, geom="point", shape=23, size=2) 
      } else {
        ggplot(data = rt, aes(x = CONDITION, y = RT, fill = CONDITION)) +
          geom_boxplot(notch = TRUE) +
          ggtitle(title) +
          stat_summary(fun.y=mean, geom="point", shape=23, size=2)  
        
      }
    }
  }
}

#PLOT CRITICAL SEGMENT READING TIME PER CONDITION
plot_crit_rt <- function(aggregated = FALSE, data, segs = CRIT_SEGMENTS, by_seg = TRUE, title, within_subject = FALSE){
  if (aggregated){
    if (within_subject){
      if (by_seg){
        ggplot(data)+
          geom_bar(position="dodge", stat = "identity", aes(x = ID, y = avg.RT, fill=Seg), color = "black") +
          scale_fill_brewer(palette = "Pastel1") +
          geom_errorbar(aes(x=ID, fill=Seg, color=factor(Seg),ymin=avg.RT-sd, ymax=avg.RT+sd), width=.2)+
          ggtitle(title)+
          geom_text(aes(x=ID,y=avg.RT,fill=Seg,label=avg.RT), position=position_dodge(width=1), vjust=-0.25) 
      } else {
        ggplot(data)+
          geom_bar(position="dodge", stat = "identity", aes(x = ID, y = avg.RT, fill=CONDITION), color = "black") +
          scale_fill_brewer(palette = "Pastel1") +
          geom_errorbar(aes(x=ID, fill=CONDITION,color=factor(CONDITION),ymin=avg.RT-sd, ymax=avg.RT+sd), width=.2)+
          ggtitle(title)+
          geom_text(aes(x=ID,y=avg.RT,fill=CONDITION,label=avg.RT), position=position_dodge(width=1), vjust=-0.25) 
      }
    } else {
      if (by_seg){
        ggplot(data)+
          geom_bar(position="dodge", stat = "identity", aes(x = Seg, y = avg.RT, fill=Seg), color = "black") +
          scale_fill_brewer(palette = "Pastel1") +
          geom_errorbar(aes(x=Seg,fill=Seg,color=factor(Seg),ymin=avg.RT-sd, ymax=avg.RT+sd), width=.2)+
          ggtitle(title)+
          geom_text(aes(x=Seg,y=avg.RT,fill=Seg,label=avg.RT), position=position_dodge(width=1), vjust=-0.25) 
      } else {
        ggplot(data)+
          geom_bar(position="dodge", stat = "identity", aes(x = CONDITION, y = avg.RT, fill=CONDITION), color = "black") +
          scale_fill_brewer(palette = "Pastel1") +
          geom_errorbar(aes(x=ID,fill=CONDITION,ymin=avg.RT-sd, ymax=avg.RT+sd), width=.2)+
          ggtitle(title)+
          geom_text(aes(x=CONDITION,y=avg.RT,label=avg.RT), position=position_dodge(width=1), vjust=-0.25)  
      }
    }
  }
  else{
      rt <- data.frame(matrix(ncol = ncol(data), nrow = 0))
      for (seg in segs){
        cond <- seg[1]
        seg <- seg[2]
        df <- subset(data, CONDITION == cond & Seg == seg)
        rt <- rbind(rt, df)
      }
      rt$count <- 1
      if (within_subject == TRUE){
        if (by_seg == TRUE){
          seg.rt <- rt %>%
            group_by(ID, CONDITION, Seg) %>% 
            summarize(Count = sum(count), 
                      seg.RT = sum(as.numeric(RT)),
                      avg.RT = (sum(seg.RT) / Count),
                      sd = sd(RT))
          seg.rt$Seg=paste(seg.rt$CONDITION,seg.rt$Seg)
          seg.rt <- round_df(seg.rt, 2)
          #seg.rt$Condition <- factor(seg.rt$Condition, levels = SPR_CONDS)
          ggplot(seg.rt)+
            geom_bar(position="dodge", stat = "identity", aes(x = ID, y = avg.RT, fill=Seg), color = "black") +
            scale_fill_brewer(palette = "Pastel1") +
            geom_errorbar(aes(x=ID, fill=Seg,color=factor(Seg),ymin=avg.RT-sd, ymax=avg.RT+sd), width=.2)+
            ggtitle(title)+
            geom_text(aes(x=ID,y=avg.RT,fill=Seg,label=avg.RT), position=position_dodge(width=1), vjust=-0.25) 
          
        } else {
          seg.rt <- rt %>%
            group_by(ID, CONDITION) %>% 
            summarize(Count = sum(count), 
                      seg.RT = sum(as.numeric(RT)),
                      avg.RT = (sum(seg.RT) / Count),
                      sd = sd(RT)) 
          seg.rt <- round_df(seg.rt, 2)
          #seg.rt$Condition <- factor(seg.rt$Condition, levels = SPR_CONDS)
          ggplot(seg.rt)+
            geom_bar(position="dodge", stat = "identity", aes(x = ID, y = avg.RT, fill=CONDITION), color = "black") +
            scale_fill_brewer(palette = "Pastel1") +
            geom_errorbar(aes(x=ID, fill=CONDITION,color=factor(CONDITION),ymin=avg.RT-sd, ymax=avg.RT+sd), width=.2)+
            ggtitle(title)+
            geom_text(aes(x=ID,y=avg.RT,fill=CONDITION,label=avg.RT), position=position_dodge(width=1), vjust=-0.25) 
          
        }
      } else {
        if (by_seg == TRUE){
          seg.rt <- rt %>%
            group_by(CONDITION, Seg) %>% 
            summarize(Count = sum(count), 
                      seg.RT = sum(as.numeric(RT)),
                      avg.RT = (sum(seg.RT) / Count),
                      sd = sd(RT))
          seg.rt$Seg=paste(seg.rt$CONDITION,seg.rt$Seg)
          seg.rt <- round_df(seg.rt, 2)
          #seg.rt$Condition <- factor(seg.rt$Condition, levels = SPR_CONDS)
          ggplot(seg.rt)+
            geom_bar(position="dodge", stat = "identity", aes(x = Condition, y = avg.RT, fill=Seg), color = "black") +
            scale_fill_brewer(palette = "Pastel1") +
            geom_errorbar(aes(x=Seg, fill=Seg,ymin=avg.RT-sd, ymax=avg.RT+sd), width=.2)+
            ggtitle(title)+
            geom_text(aes(x=Seg, fill=Seg,y=avg.RT,label=avg.RT), position=position_dodge(width=1), vjust=-0.25) 
        } else {
          seg.rt <- rt %>%
            group_by(CONDITION) %>% 
            summarize(Count = sum(count), 
                      seg.RT = sum(as.numeric(RT)),
                      avg.RT = (sum(seg.RT) / Count),
                      sd = sd(RT)) 
          seg.rt <- round_df(seg.rt, 2)
          #seg.rt$Condition <- factor(seg.rt$Condition, levels = SPR_CONDS)
          ggplot(seg.rt)+
            geom_bar(position="dodge", stat = "identity", aes(x = CONDITION, y = avg.RT, fill=CONDITION), color = "black") +
            scale_fill_brewer(palette = "Pastel1") +
            geom_errorbar(aes(x=CONDITION, fill=CONDITION,ymin=avg.RT-sd, ymax=avg.RT+sd), width=.2)+
            ggtitle(title)+
            geom_text(aes(x=CONDITION, fill=CONDITION,y=avg.RT,label=avg.RT), position=position_dodge(width=1), vjust=-0.25) 
      }
    }
  }
}


#RT FILTERING========================
#function that creates the filter (stats measures for thresholds) on RT, within subject and within condition segment
#@filter_seg: a specific segment for which to generate mean and SD values. 
#@filter_seg should be a list in this format: list(cond_name, seg_name) where filter_seg[1] is the name of the CONDITION and filter_seg[2] is the name of the Seg
#@range: how many SDs away from the mean
#@return: a dataframe where each row contains the mean and SD per subject per condition segment
create_filter <- function(data, filter_seg, range = 3, method){
  cond <- filter_seg[1]
  seg <- filter_seg[2]
  if (method == 'quart'){
    ids <- c()
    Q1s <- c()
    Q3s <- c()
    for (id in unique(data$ID)){
      df = subset(data,
                  (ID == id) &
                    (CONDITION == cond) &
                    (Seg == seg))
      q1 <- summary(df$RT)[["1st Qu."]]
      q3 <- summary(df$RT)[["3rd Qu."]]
      Q1s <- c(Q1s, q1)
      Q3s <- c(Q3s, q3)
     # segs <-c(segs, seg)
     # conds <- c(conds, cond)
      ids <- c(ids, id)
    }
    Q1s <- as.numeric(Q1s)
    Q3s <- as.numeric(Q3s)
    filter <- data.frame(ids, Q1s, Q3s)
    colnames(filter) <- c("ID","Q1", "Q3") 
    filter$CONDITION <- cond
    filter$Seg <- seg
  }
  else if (method == 'sd'){
    ids <- c()
    sds <- c()
    means <- c()
    uppers <- c()
    lowers <- c()
    for (id in unique(data$ID)){
      df = subset(data,
                  (ID == id) &
                    (CONDITION == cond) &
                    (Seg == seg))
      sd <- sd(df$RT)
      mean <- mean(df$RT)
      ids <- c(ids, id)
      sds <- c(sds, sd)
      uppers <- c(uppers, (mean+range*sd))
      lowers <- c(lowers, (mean-range*sd))
      means <- c(means, mean)
    }
    sds <- as.numeric(sds)
    means <- as.numeric(means)
    uppers <- as.numeric(uppers)
    lowers <- as.numeric(lowers)
    filter <- data.frame(ids, means, sds, uppers, lowers)
    colnames(filter) <- c("ID","mean", "sd",paste(range,"sd above mean"), paste(range,"sd below mean"))
    filter$CONDITION <- cond
    filter$Seg <- seg
  }
  return(filter)
}

#function creates a list of dataframes that can serve as SD filters to replace or drop RTs
#this function calls @create_filter and is essentially a shorthand to create multiple filters for multiple segments at once
#@filter_segs: a list of segments for which to generate mean and SD values. 
              #Each segment should be a list in this format: list(cond_name, seg_name)
#@range: for standard deviation filters, specify 2.5 / 3 / 4 ...etc SD away from the mean 
#@return: a list of dataframes containing mean and SD for each segment in @filter_segs
#
generate_filters <- function(data, filter_segs, range, method = 'sd'){
  seg_filters <- list()
  i <- 1
  for (f_seg in filter_segs){
    #print(f_seg)
    filter = create_filter(data, f_seg, range, method)
    seg_filters[[i]] <- filter
    i <- i+1
  }
  return(seg_filters)
}

#function that drops outlier individual segments based on a given criteria
#method is based on a standard deviation threshold
#@filter: a dataframe containing the mean and SD of RT within subject and within specific condition segment
#@range: for standard deviation filters, specify 2.5 / 3 / 4 ...etc SD away from the mean 
#@return: a list with [1]: altered dataframe with outliers excluded, and [2] a list of dropped rows in the original dataframe 
filterRT <- function(data, filter, range){
  excluded <- data.frame(matrix(ncol = ncol(data), nrow = 0))
  excld_all <- data.frame(matrix(ncol = ncol(data), nrow = 0))
  colnames(excluded) <- colnames(data)
  #standard deviation filter calculates endpoints based on the mean and range of SD away from the mean
  for(id in unique(data$ID)){
    for(cond in unique(subset(filter, ID == id)$CONDITION)){
      seg <- unique(subset(filter, ID == id)$Seg)
      mean <- subset(filter, ID == id & CONDITION == cond)$mean
      sd <- subset(filter, ID == id & CONDITION == cond)$sd
      upper <- mean + range*sd
      lower <- mean - range*sd
      excld <- subset(data, ID == id & CONDITION == cond & Seg == seg & (RT > upper | RT < lower))
      excluded <- rbind(excluded, excld)
      #####only individual segments that are above or below the endpoints are dropped; other segments from same sentence are preserved
      data <- subset(data, (ID != id | CONDITION != cond | Seg != seg) | (ID == id & CONDITION == cond & Seg == seg & (RT > lower & RT < upper)))
    }
  }
  return(list('data' = data, 'excluded' = excluded))
}

#function that replaces outlier individual segments based on a given criteria
#method is based on a standard deviation threshold
#@filter: a dataframe containing the mean and SD of RT within subject and within specific condition segment
#@range: for standard deviation filters, specify 2.5 / 3 / 4 ...etc SD away from the mean 
#@return: a list with [1]: altered dataframe where outliers are replaced with the mean within subject within segment, and [2] a list of replaced rows in the original dataframe 
replaceRT <- function(data, filter, range, replace_w_mean = TRUE){
  replaced <- data.frame(matrix(ncol = ncol(data), nrow = 0))
  for(id in unique(data$ID)){
    for(cond in unique(subset(filter, ID == id)$CONDITION)){
      seg <- unique(subset(filter, ID == id & CONDITION == cond)$Seg)
      mean <- subset(filter, ID == id & CONDITION == cond)$mean
      sd <- subset(filter, ID == id & CONDITION == cond)$sd
      upper <- mean + range*sd
      lower <- mean - range*sd
      replc <- subset(data, ID == id & CONDITION == cond & Seg == seg & (RT > upper | RT < lower))
      replaced <- rbind(replaced, replc)
      if (replace_w_mean == TRUE){
        #####replace SD outliers with mean within subject and segment
        if (nrow(data[data$ID == id & data$CONDITION == cond & data$Seg == seg & data$RT < lower, ])>0){
          data[data$ID == id & data$CONDITION == cond & data$Seg == seg & data$RT < lower, ]$RT <- mean 
        }
        if (nrow(data[data$ID == id & data$CONDITION == cond & data$Seg == seg & data$RT > upper, ])>0){
          data[data$ID == id & data$CONDITION == cond & data$Seg == seg & data$RT > upper, ]$RT <- mean  
        }
      } else {
        #####alternative: replace SD outliers with threshold value (at range)
        n <- length(subset(data, ID == id & CONDITION == cond & Seg == seg)$RT)
        low_v <- rep(lower, n)
        upp_v <- rep(upper, n)
        data[data$ID == id & data$CONDITION == cond & data$Seg == seg, ]$RT <- pmax(subset(data, ID == id & CONDITION == cond & Seg == seg)$RT, low_v)
        data[data$ID == id & data$CONDITION == cond & data$Seg == seg, ]$RT <- pmin(subset(data, ID == id & CONDITION == cond & Seg == seg)$RT, upp_v)
      }
    }  
  }
  return(list('data' = data, 'replaced' = replaced))
}
