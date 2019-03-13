library(ggplot2)
library(RColorBrewer)
library(colorRamps)
library(plyr)

directory <- "/Volumes/personal/tingon/SV_caller_evaluation_project/SV_simulation_final3/POS_random"

coverage_all <- c("T20xN15x","T20xN30x","T20xN45x","T20xN60x","T20xN75x","T20xN90x",
                  "T30xN15x","T30xN30x","T30xN45x","T30xN60x","T30xN75x","T30xN90x",
                  "T45xN15x","T45xN30x","T45xN45x","T45xN60x","T45xN75x","T45xN90x",
                  "T60xN15x","T60xN30x","T60xN45x","T60xN60x","T60xN75x","T60xN90x",
                  "T75xN15x","T75xN30x","T75xN45x","T75xN60x","T75xN75x","T75xN90x",
                  "T90xN15x","T90xN30x","T90xN45x","T90xN60x","T90xN75x","T90xN90x")
VAF_all <- c(0.05,0.1,0.2,0.5,0.8,1)
BND_threshold_all <- c(2,5,50,150,200)
T_coverage_all <- c(20,30,45,60,75,90)
N_coverage_all <- c(15,30,45,60,75,90)

T_coverage <- rep(T_coverage_all,each=length(N_coverage_all))
N_coverage <- rep(N_coverage_all,length(T_coverage_all))
measurements <- c("Sensitivity","Precision")
color_SVCaller <- brewer.pal(9, "Set1")[c(2:8)]
SV_caller <- c("Manta","Lumpy","GRIDSS","BreakDancer","CNVKit","Pindel","SvABA")
names(color_SVCaller) <- SV_caller

SV_caller <- c("Manta","Lumpy","GRIDSS")
for (i in c(1: length(SV_caller))){
  SVCaller_name <- SV_caller[i]
  setwd(directory)
  setwd(paste0("./",SVCaller_name))
  assign(paste0(SVCaller_name,"_SVTYPE_Call_STAT"),read.csv(paste0(SVCaller_name,"_SV_Call_STAT.csv"),row.names = 1))
  assign(paste0(SVCaller_name,"_SVTYPE_TP_Sensitivity"),read.csv(paste0(SVCaller_name,"_SV_TP_Sensitivity.csv"),row.names = 1))
  assign(paste0(SVCaller_name,"_SVTYPE_TP_Precision"), read.csv(paste0(SVCaller_name,"_SV_TP_Precision.csv"),row.names = 1))
  #assign(paste0(SVCaller_name,"_SVLEN_TP_Sensitivity"),read.csv(paste0(SVCaller_name,"_SVLEN_TP_Sensitivity.csv"),row.names = 1))
  #assign(paste0(SVCaller_name,"_SVLEN_TP_Precision"),read.csv(paste0(SVCaller_name,"_SVLEN_TP_Precision.csv"),row.names = 1))
}
Manta_SVTYPE_Call_STAT <- Manta_SVTYPE_Call_STAT[,!grepl("N120x",colnames(Manta_SVTYPE_Call_STAT))]
Manta_SVTYPE_TP_Sensitivity <- Manta_SVTYPE_TP_Sensitivity[,!grepl("N120x",colnames(Manta_SVTYPE_TP_Sensitivity))]
Manta_SVTYPE_TP_Precision <- Manta_SVTYPE_TP_Precision[,!grepl("N120x",colnames(Manta_SVTYPE_TP_Precision))]

SVCalller_SVTYPE_TP_Sensitivity <- do.call("cbind",lapply(paste0(SV_caller,"_SVTYPE_TP_Sensitivity[-1]"),function(s) eval(parse(text=s))))
tmp1 <- t(eval(parse(text=paste0("SVCalller_SVTYPE_TP_Sensitivity"))))
tmp2 <- data.frame(matrix(unlist(strsplit(colnames(eval(parse(text=paste0("SVCalller_SVTYPE_TP_Sensitivity")))),"_")),
                          nrow = ncol(eval(parse(text=paste0("SVCalller_SVTYPE_TP_Sensitivity")))),byrow=T))
colnames(tmp2) <- c("SVCaller","Measurement","Coverage","VAF","BND_threshold")
df_sensitivity <- data.frame(tmp2,tmp1,
                             T_coverage = rep(paste0(rep(T_coverage,each=2*length(BND_threshold_all)*length(VAF_all)),"x"),length(SV_caller)),
                             N_coverage = rep(paste0(rep(N_coverage,each=2*length(BND_threshold_all)*length(VAF_all)),"x"),length(SV_caller)))
df_sensitivity$N_coverage <- factor(df_sensitivity$N_coverage, levels=paste0(N_coverage_all,"x"))
df_sensitivity$BND_threshold <- factor(df_sensitivity$BND_threshold, levels=paste0(BND_threshold_all,"bp"))


SVCalller_SVTYPE_TP_Precision <- do.call("cbind", lapply(paste0(SV_caller,"_SVTYPE_TP_Precision"),function(s) eval(parse(text=s))))
tmp1 <- t(eval(parse(text=paste0("SVCalller_SVTYPE_TP_Precision"))))
tmp2 <- data.frame(matrix(unlist(strsplit(colnames(eval(parse(text=paste0("SVCalller_SVTYPE_TP_Precision")))),"_")),
                          nrow = ncol(eval(parse(text=paste0("SVCalller_SVTYPE_TP_Precision")))),byrow=T))
colnames(tmp2) <- c("SVCaller","Measurement","Coverage","VAF","BND_threshold")
df_precision <- data.frame(tmp2,tmp1,
                           T_coverage = rep(paste0(rep(T_coverage,each=5*length(BND_threshold_all)*length(VAF_all)),"x"),length(SV_caller)),
                           N_coverage = rep(paste0(rep(N_coverage,each=5*length(BND_threshold_all)*length(VAF_all)),"x"),length(SV_caller)))
df_precision$N_coverage <- factor(df_precision$N_coverage, levels=paste0(N_coverage_all,"x"))
df_precision$BND_threshold <- factor(df_precision$BND_threshold, levels=paste0(BND_threshold_all,"bp"))


# measurement <- "sensitivity"
# input_X_axis <- "VAF"
# 
# plot_generate <- function(measurement,input_X_axis){
#   
# }
# df <- df_sensitivity[df_sensitivity$Measurement == measurement & 
#                        df_sensitivity$SVCaller %in% input$SVCaller2 &
#                        df_sensitivity$BND_threshold == input$BND_threshold2 &
#                        df_sensitivity$T_coverage %in% input$T_coverage2 &
#                        df_sensitivity$N_coverage %in% input$N_coverage2,]
# line_type <- c("solid","longdash","twodash","dashed","dotdash","dotted")
# names(line_type) <- c("90x","75x","60x","45x","30x","20x")
# point_shape <- c(15,16,17,0,1,2,3)
# names(point_shape) <- c("120x","90x","75x","60x","45x","30x","15x")
# ggplot(data=df, aes(x=VAF, y=SUM, colour=SVCaller, group = interaction(SVCaller,T_coverage,N_coverage))) +
#   geom_point(aes(shape=N_coverage),size=3)+
#   geom_line(aes(linetype=T_coverage))+
#   scale_y_continuous(breaks = seq(0, 1, by = 0.2),limits = c(0,1))+
#   scale_color_manual(values=color_SVCaller[names(color_SVCaller) %in% df$SVCaller])+
#   scale_linetype_manual(values=line_type[names(line_type) %in% df$T_coverage])+
#   scale_shape_manual(values=point_shape[names(point_shape) %in% df$N_coverage])+
#   labs(title = paste("SUM",measurement,"v.s.",input$X_axis2),x=input$X_axis2, y = measurement)+
#   theme(axis.text=element_text(size=14),axis.title=element_text(size=14,face="bold"),plot.title = element_text(size=18),
#         legend.title=element_text(size=14),legend.text=element_text(size=14))





