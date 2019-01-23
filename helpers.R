library(ggplot2)
library(RColorBrewer)
library(colorRamps)
library(plyr)
#setwd("/Volumes/personal/tingon/SV caller evaluation project/SV_simulation_final2/POS_random")
setwd("./data")
df_read <- function(file){
  df <- read.csv(file)
  rownames(df) <- df$X
  df <- df[,-1]
  return(df)
}
SVTYPE_Call_STAT_T60x_N60x <- df_read("SV_Call_STAT_T60x_N60x.csv")
SVTYPE_TP_Sensitivity_T60x_N60x <- df_read("SV_TP_Sensitivity_T60x_N60x.csv")
SVTYPE_TP_Precision_T60x_N60x <- df_read("SV_TP_Precision_T60x_N60x.csv")
SVLEN_STAT_T60x_N60x <- df_read("SVLEN_STAT_T60x_N60x.csv")
SVLEN_TP_Sensitivity_T60x_N60x <- df_read("SVLEN_TP_Sensitivity_T60x_N60x.csv")
SVLEN_TP_Precision_T60x_N60x <- df_read("SVLEN_TP_Precision_T60x_N60x.csv")

SVTYPE_Call_STAT_T60x_N30x <- df_read("SV_Call_STAT_T60x_N30x.csv")
SVTYPE_TP_Sensitivity_T60x_N30x <- df_read("SV_TP_Sensitivity_T60x_N30x.csv")
SVTYPE_TP_Precision_T60x_N30x <- df_read("SV_TP_Precision_T60x_N30x.csv")
SVLEN_STAT_T60x_N30x <- df_read("SVLEN_STAT_T60x_N30x.csv")
SVLEN_TP_Sensitivity_T60x_N30x <- df_read("SVLEN_TP_Sensitivity_T60x_N30x.csv")
SVLEN_TP_Precision_T60x_N30x <- df_read("SVLEN_TP_Precision_T60x_N30x.csv")

SVTYPE_Call_STAT_T60x_N15x <- df_read("SV_Call_STAT_T60x_N15x.csv")
SVTYPE_TP_Sensitivity_T60x_N15x <- df_read("SV_TP_Sensitivity_T60x_N15x.csv")
SVTYPE_TP_Precision_T60x_N15x <- df_read("SV_TP_Precision_T60x_N15x.csv")
SVLEN_STAT_T60x_N15x <- df_read("SVLEN_STAT_T60x_N15x.csv")
SVLEN_TP_Sensitivity_T60x_N15x <- df_read("SVLEN_TP_Sensitivity_T60x_N15x.csv")
SVLEN_TP_Precision_T60x_N15x <- df_read("SVLEN_TP_Precision_T60x_N15x.csv")

SVTYPE_Call_STAT_T30x_N30x <- df_read("SV_Call_STAT_T30x_N30x.csv")
SVTYPE_TP_Sensitivity_T30x_N30x <- df_read("SV_TP_Sensitivity_T30x_N30x.csv")
SVTYPE_TP_Precision_T30x_N30x <- df_read("SV_TP_Precision_T30x_N30x.csv")
SVLEN_STAT_T30x_N30x <- df_read("SVLEN_STAT_T30x_N30x.csv")
SVLEN_TP_Sensitivity_T30x_N30x <- df_read("SVLEN_TP_Sensitivity_T30x_N30x.csv")
SVLEN_TP_Precision_T30x_N30x <- df_read("SVLEN_TP_Precision_T30x_N30x.csv")

SVTYPE_Call_STAT_T30x_N15x <- df_read("SV_Call_STAT_T30x_N15x.csv")
SVTYPE_TP_Sensitivity_T30x_N15x <- df_read("SV_TP_Sensitivity_T30x_N15x.csv")
SVTYPE_TP_Precision_T30x_N15x <- df_read("SV_TP_Precision_T30x_N15x.csv")
SVLEN_STAT_T30x_N15x <- df_read("SVLEN_STAT_T30x_N15x.csv")
SVLEN_TP_Sensitivity_T30x_N15x <- df_read("SVLEN_TP_Sensitivity_T30x_N15x.csv")
SVLEN_TP_Precision_T30x_N15x <- df_read("SVLEN_TP_Precision_T30x_N15x.csv")

SVTYPE_Call_STAT_T60x_N30x_0.5 <- df_read("SV_Call_STAT_T60x_N30x_0.5.csv")
SVTYPE_TP_Sensitivity_T60x_N30x_0.5 <- df_read("SV_TP_Sensitivity_T60x_N30x_0.5.csv")
SVTYPE_TP_Precision_T60x_N30x_0.5 <- df_read("SV_TP_Precision_T60x_N30x_0.5.csv")
SVLEN_STAT_T60x_N30x_0.5 <- df_read("SVLEN_STAT_T60x_N30x_0.5.csv")
SVLEN_TP_Sensitivity_T60x_N30x_0.5 <- df_read("SVLEN_TP_Sensitivity_T60x_N30x_0.5.csv")
SVLEN_TP_Precision_T60x_N30x_0.5 <- df_read("SVLEN_TP_Precision_T60x_N30x_0.5.csv")

SVTYPE_Call_STAT_T60x_N15x_0.5 <- df_read("SV_Call_STAT_T60x_N15x_0.5.csv")
SVTYPE_TP_Sensitivity_T60x_N15x_0.5 <- df_read("SV_TP_Sensitivity_T60x_N15x_0.5.csv")
SVTYPE_TP_Precision_T60x_N15x_0.5 <- df_read("SV_TP_Precision_T60x_N15x_0.5.csv")
SVLEN_STAT_T60x_N15x_0.5 <- df_read("SVLEN_STAT_T60x_N15x_0.5.csv")
SVLEN_TP_Sensitivity_T60x_N15x_0.5 <- df_read("SVLEN_TP_Sensitivity_T60x_N15x_0.5.csv")
SVLEN_TP_Precision_T60x_N15x_0.5 <- df_read("SVLEN_TP_Precision_T60x_N15x_0.5.csv")

SVTYPE_Call_STAT_T30x_N30x_0.5 <- df_read("SV_Call_STAT_T30x_N30x_0.5.csv")
SVTYPE_TP_Sensitivity_T30x_N30x_0.5 <- df_read("SV_TP_Sensitivity_T30x_N30x_0.5.csv")
SVTYPE_TP_Precision_T30x_N30x_0.5 <- df_read("SV_TP_Precision_T30x_N30x_0.5.csv")
SVLEN_STAT_T30x_N30x_0.5 <- df_read("SVLEN_STAT_T30x_N30x_0.5.csv")
SVLEN_TP_Sensitivity_T30x_N30x_0.5 <- df_read("SVLEN_TP_Sensitivity_T30x_N30x_0.5.csv")
SVLEN_TP_Precision_T30x_N30x_0.5 <- df_read("SVLEN_TP_Precision_T30x_N30x_0.5.csv")

SVTYPE_Call_STAT_T30x_N15x_0.5 <- df_read("SV_Call_STAT_T30x_N15x_0.5.csv")
SVTYPE_TP_Sensitivity_T30x_N15x_0.5 <- df_read("SV_TP_Sensitivity_T30x_N15x_0.5.csv")
SVTYPE_TP_Precision_T30x_N15x_0.5 <- df_read("SV_TP_Precision_T30x_N15x_0.5.csv")
SVLEN_STAT_T30x_N15x_0.5 <- df_read("SVLEN_STAT_T30x_N15x_0.5.csv")
SVLEN_TP_Sensitivity_T30x_N15x_0.5 <- df_read("SVLEN_TP_Sensitivity_T30x_N15x_0.5.csv")
SVLEN_TP_Precision_T30x_N15x_0.5 <- df_read("SVLEN_TP_Precision_T30x_N15x_0.5.csv")

SVTYPE_Call_STAT_T60x_N30x_0.2 <- df_read("SV_Call_STAT_T60x_N30x_0.2.csv")
SVTYPE_TP_Sensitivity_T60x_N30x_0.2 <- df_read("SV_TP_Sensitivity_T60x_N30x_0.2.csv")
SVTYPE_TP_Precision_T60x_N30x_0.2 <- df_read("SV_TP_Precision_T60x_N30x_0.2.csv")
SVLEN_STAT_T60x_N30x_0.2 <- df_read("SVLEN_STAT_T60x_N30x_0.2.csv")
SVLEN_TP_Sensitivity_T60x_N30x_0.2 <- df_read("SVLEN_TP_Sensitivity_T60x_N30x_0.2.csv")
SVLEN_TP_Precision_T60x_N30x_0.2 <- df_read("SVLEN_TP_Precision_T60x_N30x_0.2.csv")

SVTYPE_Call_STAT_T60x_N15x_0.2 <- df_read("SV_Call_STAT_T60x_N15x_0.2.csv")
SVTYPE_TP_Sensitivity_T60x_N15x_0.2 <- df_read("SV_TP_Sensitivity_T60x_N15x_0.2.csv")
SVTYPE_TP_Precision_T60x_N15x_0.2 <- df_read("SV_TP_Precision_T60x_N15x_0.2.csv")
SVLEN_STAT_T60x_N15x_0.2 <- df_read("SVLEN_STAT_T60x_N15x_0.2.csv")
SVLEN_TP_Sensitivity_T60x_N15x_0.2 <- df_read("SVLEN_TP_Sensitivity_T60x_N15x_0.2.csv")
SVLEN_TP_Precision_T60x_N15x_0.2 <- df_read("SVLEN_TP_Precision_T60x_N15x_0.2.csv")

SVTYPE_Call_STAT_T30x_N30x_0.2 <- df_read("SV_Call_STAT_T30x_N30x_0.2.csv")
SVTYPE_TP_Sensitivity_T30x_N30x_0.2 <- df_read("SV_TP_Sensitivity_T30x_N30x_0.2.csv")
SVTYPE_TP_Precision_T30x_N30x_0.2 <- df_read("SV_TP_Precision_T30x_N30x_0.2.csv")
SVLEN_STAT_T30x_N30x_0.2 <- df_read("SVLEN_STAT_T30x_N30x_0.2.csv")
SVLEN_TP_Sensitivity_T30x_N30x_0.2 <- df_read("SVLEN_TP_Sensitivity_T30x_N30x_0.2.csv")
SVLEN_TP_Precision_T30x_N30x_0.2 <- df_read("SVLEN_TP_Precision_T30x_N30x_0.2.csv")

SVTYPE_Call_STAT_T30x_N15x_0.2 <- df_read("SV_Call_STAT_T30x_N15x_0.2.csv")
SVTYPE_TP_Sensitivity_T30x_N15x_0.2 <- df_read("SV_TP_Sensitivity_T30x_N15x_0.2.csv")
SVTYPE_TP_Precision_T30x_N15x_0.2 <- df_read("SV_TP_Precision_T30x_N15x_0.2.csv")
SVLEN_STAT_T30x_N15x_0.2 <- df_read("SVLEN_STAT_T30x_N15x_0.2.csv")
SVLEN_TP_Sensitivity_T30x_N15x_0.2 <- df_read("SVLEN_TP_Sensitivity_T30x_N15x_0.2.csv")
SVLEN_TP_Precision_T30x_N15x_0.2 <- df_read("SVLEN_TP_Precision_T30x_N15x_0.2.csv")

SVTYPE_Call_STAT_T60x_N30x_0.1 <- df_read("SV_Call_STAT_T60x_N30x_0.1.csv")
SVTYPE_TP_Sensitivity_T60x_N30x_0.1 <- df_read("SV_TP_Sensitivity_T60x_N30x_0.1.csv")
SVTYPE_TP_Precision_T60x_N30x_0.1 <- df_read("SV_TP_Precision_T60x_N30x_0.1.csv")
SVLEN_STAT_T60x_N30x_0.1 <- df_read("SVLEN_STAT_T60x_N30x_0.1.csv")
SVLEN_TP_Sensitivity_T60x_N30x_0.1 <- df_read("SVLEN_TP_Sensitivity_T60x_N30x_0.1.csv")
SVLEN_TP_Precision_T60x_N30x_0.1 <- df_read("SVLEN_TP_Precision_T60x_N30x_0.1.csv")

SVTYPE_Call_STAT_T60x_N15x_0.1 <- df_read("SV_Call_STAT_T60x_N15x_0.1.csv")
SVTYPE_TP_Sensitivity_T60x_N15x_0.1 <- df_read("SV_TP_Sensitivity_T60x_N15x_0.1.csv")
SVTYPE_TP_Precision_T60x_N15x_0.1 <- df_read("SV_TP_Precision_T60x_N15x_0.1.csv")
SVLEN_STAT_T60x_N15x_0.1 <- df_read("SVLEN_STAT_T60x_N15x_0.1.csv")
SVLEN_TP_Sensitivity_T60x_N15x_0.1 <- df_read("SVLEN_TP_Sensitivity_T60x_N15x_0.1.csv")
SVLEN_TP_Precision_T60x_N15x_0.1 <- df_read("SVLEN_TP_Precision_T60x_N15x_0.1.csv")

SVTYPE_Call_STAT_T30x_N30x_0.1 <- df_read("SV_Call_STAT_T30x_N30x_0.1.csv")
SVTYPE_TP_Sensitivity_T30x_N30x_0.1 <- df_read("SV_TP_Sensitivity_T30x_N30x_0.1.csv")
SVTYPE_TP_Precision_T30x_N30x_0.1 <- df_read("SV_TP_Precision_T30x_N30x_0.1.csv")
SVLEN_STAT_T30x_N30x_0.1 <- df_read("SVLEN_STAT_T30x_N30x_0.1.csv")
SVLEN_TP_Sensitivity_T30x_N30x_0.1 <- df_read("SVLEN_TP_Sensitivity_T30x_N30x_0.1.csv")
SVLEN_TP_Precision_T30x_N30x_0.1 <- df_read("SVLEN_TP_Precision_T30x_N30x_0.1.csv")

SVTYPE_Call_STAT_T30x_N15x_0.1 <- df_read("SV_Call_STAT_T30x_N15x_0.1.csv")
SVTYPE_TP_Sensitivity_T30x_N15x_0.1 <- df_read("SV_TP_Sensitivity_T30x_N15x_0.1.csv")
SVTYPE_TP_Precision_T30x_N15x_0.1 <- df_read("SV_TP_Precision_T30x_N15x_0.1.csv")
SVLEN_STAT_T30x_N15x_0.1 <- df_read("SVLEN_STAT_T30x_N15x_0.1.csv")
SVLEN_TP_Sensitivity_T30x_N15x_0.1 <- df_read("SVLEN_TP_Sensitivity_T30x_N15x_0.1.csv")
SVLEN_TP_Precision_T30x_N15x_0.1 <- df_read("SVLEN_TP_Precision_T30x_N15x_0.1.csv")





color_SVCaller <- brewer.pal(9, "Set1")[c(2:8)]
names(color_SVCaller) <- c("Manta","Lumpy","GRIDSS","BreakDancer","CNVKit","Pindel","SvABA")

Sensitivity_plot <- function(data,caller,type){
  if(type == "SVTYPE"){
    SVTYPE_name <- rownames(data)
    df <- data.frame(
      SV_caller = rep(caller,each=nrow(data)),
      SVTYPE = rep(SVTYPE_name,length(caller)),
      Sensitivity = as.vector(as.matrix(data[,colnames(data) %in% paste0(caller,"_Sensitivity")]))
    )
    df$SVTYPE <- factor(df$SVTYPE, levels = SVTYPE_name)
  }else if(type == "SVLEN"){
    SVLEN_name <- c("(0,50]","(50,100]","(100,500]","(500,1000]","(1000,2000]","(2000,5000]",
                    "(5000,10000]","(10000,15000]","(15000,100000]","(100000,1000000]")
    df <- data.frame(
      SV_caller = rep(caller,each=nrow(data[2:11,])),
      SVLEN = rep(SVLEN_name,length(caller)),
      Sensitivity = as.vector(as.matrix(data[2:11,colnames(data) %in% paste0(caller,"_SVLEN_Sensitivity")]))
    )
    df$SVLEN <- factor(df$SVLEN, levels = SVLEN_name)
  }
  df$SV_caller <- factor(df$SV_caller, levels = caller)
  p <- ggplot(data=df, aes(x=eval(parse(text=type)), y=Sensitivity, group=SV_caller)) +
    geom_point(aes(color=SV_caller),size = 3)+
    scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
    ggtitle(paste(type, "Sensitivity"))+
    theme(axis.text.x = element_text(angle = 50, hjust = 1))+
    scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% caller])+
    labs(x=type)
  return(p)
}

Precision_plot <- function(data,caller,type){
  if(type == "SVTYPE"){
    SVTYPE_name <- rownames(data)
    df <- data.frame(
      SV_caller = rep(caller,each=nrow(data)),
      SVTYPE = rep(SVTYPE_name,length(caller)),
      Precision = as.vector(as.matrix(data[,colnames(data) %in% paste0(caller,"_Precision")]))
    )
    df$SVTYPE <- factor(df$SVTYPE, levels = SVTYPE_name)
  }else if(type == "SVLEN"){
    SVLEN_name <- c("(0,50]","(50,100]","(100,500]","(500,1000]","(1000,2000]","(2000,5000]",
                    "(5000,10000]","(10000,15000]","(15000,100000]","(100000,1000000]")
    df <- data.frame(
      SV_caller = rep(caller,each=nrow(data[2:11,])),
      SVLEN = rep(SVLEN_name,length(caller)),
      Precision = as.vector(as.matrix(data[2:11,colnames(data) %in% paste0(caller,"_SVLEN_Precision")]))
    )
    df$SVLEN <- factor(df$SVLEN, levels = SVLEN_name)
  }
  df$SV_caller <- factor(df$SV_caller, levels = caller)
  p <- ggplot(data=df, aes(x=eval(parse(text=type)), y=Precision, group=SV_caller)) +
    geom_point(aes(color=SV_caller),size = 3)+
    scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
    ggtitle(paste(type, "Precision"))+
    theme(axis.text.x = element_text(angle = 50, hjust = 1))+
    scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% caller])+
    labs(x=type)
  return(p)
}


#caller <- unlist(list("Manta","Lumpy"))
#caller <- c("Manta")
#data <- SV_TP_Sensitivity_T60x_N60x

#SV_TP_Sensitivity_T60x_N60x[,colnames( SV_TP_Sensitivity_T60x_N60x) %in% paste(caller,"_Sensitivity",sep="")]


# Sensitivity_SVTYPE_plot <- function(data,caller){
#   SVTYPE_name <- rownames(data)
# 
#   df <- data.frame(
#     SV_caller = rep(caller,each=nrow(data)),
#     SVTYPE = rep(SVTYPE_name,length(caller)),
#     Sensitivity = as.vector(as.matrix(data[,colnames(data) %in% paste(caller,"_Sensitivity",sep="")])))
#   
#   df$SVTYPE <- factor(df$SVTYPE, levels = SVTYPE_name)
#   df$SV_caller <- factor(df$SV_caller, levels = caller)
#   p <- ggplot(data=df, aes(x=SVTYPE, y=Sensitivity, group=SV_caller)) +
#           geom_point(aes(color=SV_caller),size = 3)+
#           scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#           ggtitle("SVTYPE Sensitivity")+
#           theme(axis.text.x = element_text(angle = 50, hjust = 1))+
#           scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% caller])
#   return(p)
# }
# 
# Precision_SVTYPE_plot <- function(data,caller){
#   SVTYPE_name <- rownames(data)
#   df <- data.frame(
#     SV_caller = rep(caller,each=nrow(data)),
#     SVTYPE = rep(SVTYPE_name,length(caller)),
#     Precision = as.vector(as.matrix(data[,colnames(data) %in% paste(caller,"_Precision",sep="")])))
#   
#   df$SVTYPE <- factor(df$SVTYPE, levels = SVTYPE_name)
#   df$SV_caller <- factor(df$SV_caller, levels = caller)
#   p <- ggplot(data=df, aes(x=SVTYPE, y=Precision, group=SV_caller)) +
#     geom_point(aes(color=SV_caller),size = 3)+
#     scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#     ggtitle("SVTYPE Precision")+
#     theme(axis.text.x = element_text(angle = 50, hjust = 1))+
#     scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% caller])
#   return(p)
# }
# 
# SVLEN_name <- c("(0,50]","(50,100]","(100,500]","(500,1000]","(1000,2000]","(2000,5000]",
#                 "(5000,10000]","(10000,15000]","(15000,100000]","(100000,1000000]")
# caller <- c("Manta","Lumpy")
# data <- SVLEN_TP_Sensitivity_T60x_N60x
# df <- data.frame(
#   SV_caller = rep(caller,each=nrow(data[2:11,])),
#   SVLEN = rep(SVLEN_name,length(caller)),
#   Sensitivity = as.vector(as.matrix(data[2:11,colnames(data) %in% paste(caller,"_SVLEN_Sensitivity",sep="")])))
# 
# df$SVLEN <- factor(df$SVLEN, levels = SVLEN_name)
# df$SV_caller <- factor(df$SV_caller, levels = caller)
# ggplot(data=df, aes(x=SVLEN, y=Sensitivity, group=SV_caller)) +
#   geom_point(aes(color=SV_caller),size = 3)+
#   scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#   ggtitle("SVLEN Sensitivity")+
#   theme(axis.text.x = element_text(angle = 50, hjust = 1))+
#   scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% caller])


#caller <- c("Manta")
#data <- SV_TP_Sensitivity_T60x_N60x

#Sensitivity_SVTYPE_plot(data,caller)

#eval(parse(text="Sensitivity_SVTYPE_plot(data=SV_TP_Sensitivity_T60x_N60x,caller)"))



#caller <- c("Manta","Lumpy")
#type = "SVLEN"
#data <- eval(parse(text=paste0(type,"_TP_Precision_T60x_N60x")))
#Precision_plot(data,caller,type)




