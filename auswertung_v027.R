library(ggplot2)
library(readr)
library(dplyr)
library(readxl)



rm(list = ls()) 


setwd('/home/nutzer/Project/yolov7/Rscripts')

exceldata <- read_excel("Design2.xlsx")  
Design <- data.frame(exceldata)



data <- read.csv ("allresults_header_tab_final_v002.txt",sep="\t")
data1 <- data.frame(data)
data1$versuch <- substr(data1$filename,19,21)

data1$Versuch <- as.numeric(data1$versuch)

tmp <- merge(data1, Design, by='Versuch')

data1 <- tmp

for (i in 1:dim(data1)[1]){
  ver <- substr(data1$filename[i],22,30)
  vers <- strsplit(ver,split="/")
  a<- vers
  s <- a[[1]][1]
  if (s ==""){
    s="1"
  }
  data1$version[i] <- strtoi(s)
}
for (i in 1:dim(data1)[1]){
  ver <- data1$Epoch[i]
  vers <- strsplit(ver,split="/")
  a<- vers
  s <- a[[1]][1]
  
  data1$Epoche[i] <- strtoi(s)
}
##############################################################################################

#subdata <- subset(data1, ((Versuch == 3) | (Versuch == 4)) & (Epoche > 290)  )

subdata <- subset(data1,  (Epoche > 298) & (Epoche < 300) )

subdata$versuch <- factor(subdata$versuch , levels=c("012", "010", "011", "001","004",
                                                "003", "013","005","006","007","008","009"))
ggplot(subdata, aes(x = versuch, y= mAP_95) ) +
  geom_boxplot()
 

data1_f <- data1 %>%
  group_by(versuch,version) %>%
  mutate(MaxValue = max(Epoche))

subset_ff <- subset(data1_f,MaxValue > 290)

data1_s <- subset_ff%>%
  group_by(versuch) %>%
  mutate(SuperRank = dense_rank(version))

data1df <- data1_s

################################################################################################
################################################################################################
###############################################################################################

# Gesamtübersicht: 19 Experimente (mAP_95), 10 Epochen, 5x001, Median-Version, Datensatzumbenennung, finale Version, 1

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("001", "003", "004", "012", "006", "014", "005",
                                                     "015","007","016","009","008",
                                                     "017", "018", "013", "019", "010",
                                                     "020","011"))

subdata$visible_symbols <- ifelse(subdata$versuch == "012", c("DS_b_aug"),
                                  ifelse(subdata$versuch == "010", c("OG","SG", "SG_aug"),
                                         ifelse(subdata$versuch == "011", c("OG", "OG_aug", "SG", "SG_aug"),
                                                ifelse(subdata$versuch == "001", c("B6"),
                                                       ifelse(subdata$versuch == "004", c("DS_b"),
                                                              ifelse(subdata$versuch == "003", c("DS"),
                                                                     ifelse(subdata$versuch == "005", c("SG"),
                                                                            ifelse(subdata$versuch == "006", c("OG"),
                                                                                   ifelse(subdata$versuch == "007", c("OG", "OG_aug"),
                                                                                          ifelse(subdata$versuch == "008", c("OG","SG"),
                                                                                                 ifelse(subdata$versuch == "009", c("SG", "SG_aug"), 
                                                                                                        ifelse(subdata$versuch == "013", c("OG", "OG_aug", "SG"),
                                                                                                               ifelse(subdata$versuch == "014", c("OG_aug"),
                                                                                                                      ifelse(subdata$versuch == "015", c("SG_aug"),
                                                                                                                             ifelse(subdata$versuch == "016", c("OG_aug","SG"),
                                                                                                                                    ifelse(subdata$versuch == "017", c("OG_aug","SG_aug"),
                                                                                                                                           ifelse(subdata$versuch == "018", c("OG","SG_aug"),
                                                                                                                                                  ifelse(subdata$versuch == "019", c("OG_aug","SG","SG_aug"),
                                                                                                                                                         ifelse(subdata$versuch == "020", c("OG","OG_aug","SG_aug"), NA)))))))))))))))))))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_95 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_95 ~ versuch, data = subdata_median, median)

pdf("Abb_Gesamtübersicht_19_mAP_95_10E_5x001_Median_Datensatzumbenennung_final_1.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_95)) + 
  geom_boxplot(outlier.colour = "black", outlier.size = 0.25) +
  geom_point(data = subset(subdata, visible_symbols == "B6"), aes(shape = factor("B6"), y = 1.20), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS"), aes(shape = factor("DS"), y = 1.15), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS_b"), aes(shape = factor("DS_b"), y = 1.10), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS_b_aug"), aes(shape = factor("DS_b_aug"), y = 1.05), size = 2, position = position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG_aug"),aes(shape=factor("OG_aug"),y=1.15),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG_aug"),aes(shape=factor("SG_aug"),y=1.05),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG"),aes(shape=factor("SG"),y=1.10),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(4,3,1,2,8,7,6,5),labels=c("B6","DS","DS_b","DS_b_aug","OG","OG_aug","SG","SG_aug"))+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Gesamtübersicht (alle 19 Experimente): mAP_95 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

##########################################################################################################################

# Gesamtübersicht: 19 Experimente (mAP_50), 10 Epochen, 5x001, Median-Version, Datensatzumbenennung, finale Version, 1

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300))
#subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("001", "003", "004", "012", "006", "014", "005",
                                                     "015","007","016","009","008",
                                                     "017", "018", "013", "019", "010",
                                                     "020","011"))

subdata$visible_symbols <- ifelse(subdata$versuch == "012", c("DS_b_aug"),
                                  ifelse(subdata$versuch == "010", c("OG","SG", "SG_aug"),
                                         ifelse(subdata$versuch == "011", c("OG", "OG_aug", "SG", "SG_aug"),
                                                ifelse(subdata$versuch == "001", c("B6"),
                                                       ifelse(subdata$versuch == "004", c("DS_b"),
                                                              ifelse(subdata$versuch == "003", c("DS"),
                                                                     ifelse(subdata$versuch == "005", c("SG"),
                                                                            ifelse(subdata$versuch == "006", c("OG"),
                                                                                   ifelse(subdata$versuch == "007", c("OG", "OG_aug"),
                                                                                          ifelse(subdata$versuch == "008", c("OG","SG"),
                                                                                                 ifelse(subdata$versuch == "009", c("SG", "SG_aug"), 
                                                                                                        ifelse(subdata$versuch == "013", c("OG", "OG_aug", "SG"),
                                                                                                               ifelse(subdata$versuch == "014", c("OG_aug"),
                                                                                                                      ifelse(subdata$versuch == "015", c("SG_aug"),
                                                                                                                             ifelse(subdata$versuch == "016", c("OG_aug","SG"),
                                                                                                                                    ifelse(subdata$versuch == "017", c("OG_aug","SG_aug"),
                                                                                                                                           ifelse(subdata$versuch == "018", c("OG","SG_aug"),
                                                                                                                                                  ifelse(subdata$versuch == "019", c("OG_aug","SG","SG_aug"),
                                                                                                                                                         ifelse(subdata$versuch == "020", c("OG","OG_aug","SG_aug"), NA)))))))))))))))))))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_50 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_50 ~ versuch, data = subdata_median, median)

pdf("Abb_Gesamtübersicht_19_mAP_50_10E_5x001_Median_Datensatzumbenennung_final_1.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_50)) + 
  geom_boxplot(outlier.colour = "black", outlier.size = 0.25) +
  geom_point(data = subset(subdata, visible_symbols == "B6"), aes(shape = factor("B6"), y = 1.20), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS"), aes(shape = factor("DS"), y = 1.15), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS_b"), aes(shape = factor("DS_b"), y = 1.10), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS_b_aug"), aes(shape = factor("DS_b_aug"), y = 1.05), size = 2, position = position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG_aug"),aes(shape=factor("OG_aug"),y=1.15),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG_aug"),aes(shape=factor("SG_aug"),y=1.05),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG"),aes(shape=factor("SG"),y=1.10),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(4,3,1,2,8,7,6,5),labels=c("B6","DS","DS_b","DS_b_aug","OG","OG_aug","SG","SG_aug"))+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Gesamtübersicht (alle 19 Experimente): mAP_50 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()
#########################################################################################################################

# 4 Experimente (großer Datensatz, mAP_95, 10 Epochen), 5x001, Median-Version, Datensatzumbenennung, finale Version, 1

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("001","003","004",
                                                     "012"))

subdata$visible_symbols <- ifelse(subdata$versuch == "012", c("DS_b_aug"),
                                  ifelse(subdata$versuch == "001", c("B6"),
                                         ifelse(subdata$versuch == "004", c("DS_b"),
                                                ifelse(subdata$versuch == "003", c("DS"), NA))))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_95 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_95 ~ versuch, data = subdata_median, median)

pdf("Abb_DS_mAP_95_10E_5x001_Median_Datensatzumbenennung_final_1.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_95)) + 
  geom_boxplot(outlier.colour = "black", outlier.size = 0.75) +
  geom_point(data = subset(subdata, visible_symbols == "B6"), aes(shape = factor("B6"), y = 1.20), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS"), aes(shape = factor("DS"), y = 1.15), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS_b"), aes(shape = factor("DS_b"), y = 1.10), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS_b_aug"), aes(shape = factor("DS_b_aug"), y = 1.05), size = 2, position = position_dodge(width = 1)) +
  scale_shape_manual(values=c(4,3,1,2), labels=c("B6","DS","DS_b","DS_b_aug")) +
  # Füge die Mediane als kleine rote Punkte hinzu
 
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Datensatz A: mAP_50 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

###########################################################################################################

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(mAP_95 ~ versuch, data = subdata)
kruskal_result

# Convert result to data.frame
kruskal_df <- data.frame(statistic = kruskal_result$statistic,
                         parameter = kruskal_result$parameter,
                         p.value = kruskal_result$p.value,
                         method = kruskal_result$method,
                         data.name = kruskal_result$data.name)

# Load FSA library
library(FSA)

# Perform Dunn test
dunn_result <- dunnTest(mAP_95 ~ versuch, data=subdata, method="bonferroni")

# Calculate effect size r
n <- 10
dunn_result$res$r <- dunn_result$res$Z / sqrt(n)

# Add column indicating significance
alpha <- 0.05
dunn_result$res$significant <- ifelse(dunn_result$res$P.adj < alpha, "Ja", "Nein")

# Add column indicating strength of effect size
dunn_result$res$effect_size_strength <- ifelse(abs(dunn_result$res$r) < 0.1, "vernachlässigbar",
                                               ifelse(abs(dunn_result$res$r) < 0.3, "klein",
                                                      ifelse(abs(dunn_result$res$r) < 0.5, "mittel", "groß")))

# Add column n to the results table before the effect size column
dunn_result$res$n <- n

# Reorder columns to move n before r
dunn_result$res <- dunn_result$res[, c("Comparison", "Z", "P.unadj", "P.adj", "n", "r", "significant", "effect_size_strength")]

# Display results of Dunn test
dunn_result$res

# Install and load writexl package
install.packages("writexl")
library(writexl)

# Create a list of data frames to write to the XLSX file
data_to_write <- list("Kruskal-Wallis" = kruskal_df, "Dunn Test" = dunn_result$res)

# Write data to XLSX file
write_xlsx(data_to_write, "kruskal_dunn_results_4DS_mAP_95_10E_5x001_Median_Datensatzumbenennung_final.xlsx")


##########################################################################################################################################################

# 4 Experimente (großer Datensatz, mAP_50, 10 Epochen), 5x001, Median-Version, Datensatzumbenennung, finale Version, 1

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("001","003","004",
                                                     "012"))

subdata$visible_symbols <- ifelse(subdata$versuch == "012", c("DS_b_aug"),
                                  ifelse(subdata$versuch == "001", c("B6"),
                                         ifelse(subdata$versuch == "004", c("DS_b"),
                                                ifelse(subdata$versuch == "003", c("DS"), NA))))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_50 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_50 ~ versuch, data = subdata_median, median)

pdf("Abb_DS_mAP_50_10E_5x001_Median_Datensatzumbenennung_final_1.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_50)) + 
  geom_boxplot(outlier.colour = "black", outlier.size = 0.75) +
  geom_point(data = subset(subdata, visible_symbols == "B6"), aes(shape = factor("B6"), y = 1.20), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS"), aes(shape = factor("DS"), y = 1.15), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS_b"), aes(shape = factor("DS_b"), y = 1.10), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS_b_aug"), aes(shape = factor("DS_b_aug"), y = 1.05), size = 2, position = position_dodge(width = 1)) +
  scale_shape_manual(values=c(4,3,1,2), labels=c("B6","DS","DS_b","DS_b_aug")) +
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Datensatz A: mAP_50 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

########################################################################################################
# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(mAP_50 ~ versuch, data = subdata)
kruskal_result

# Convert result to data.frame
kruskal_df <- data.frame(statistic = kruskal_result$statistic,
                         parameter = kruskal_result$parameter,
                         p.value = kruskal_result$p.value,
                         method = kruskal_result$method,
                         data.name = kruskal_result$data.name)

# Load FSA library
library(FSA)

# Perform Dunn test
dunn_result <- dunnTest(mAP_50 ~ versuch, data=subdata, method="bonferroni")

# Calculate effect size r
n <- 10
dunn_result$res$r <- dunn_result$res$Z / sqrt(n)

# Add column indicating significance
alpha <- 0.05
dunn_result$res$significant <- ifelse(dunn_result$res$P.adj < alpha, "Ja", "Nein")

# Add column indicating strength of effect size
dunn_result$res$effect_size_strength <- ifelse(abs(dunn_result$res$r) < 0.1, "vernachlässigbar",
                                               ifelse(abs(dunn_result$res$r) < 0.3, "klein",
                                                      ifelse(abs(dunn_result$res$r) < 0.5, "mittel", "groß")))

# Add column n to the results table before the effect size column
dunn_result$res$n <- n

# Reorder columns to move n before r
dunn_result$res <- dunn_result$res[, c("Comparison", "Z", "P.unadj", "P.adj", "n", "r", "significant", "effect_size_strength")]

# Display results of Dunn test
dunn_result$res


# Create a list of data frames to write to the XLSX file
data_to_write <- list("Kruskal-Wallis" = kruskal_df, "Dunn Test" = dunn_result$res)

# Write data to XLSX file
write_xlsx(data_to_write, "kruskal_dunn_results_4DS_mAP_50_10E_5x001_Median_Datensatzumbenennung_final.xlsx")

##############################################################################################################################################

# Effekt der Datensatzgröße:mAP_50, 10 Epochen, 5x001, Median-Version, Datensatzumbenennung, finale Version, 1

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "001","003"))

subdata$visible_symbols <- ifelse(subdata$versuch == "001", c("B6"),
                                  ifelse(subdata$versuch == "003", c("DS"),
                                         
                                         ifelse(subdata$versuch == "006", c("OG"),NA)))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_50 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_50 ~ versuch, data = subdata_median, median)

pdf("Abb_Datensatzgrößeneffekt__mAP_50_10E_5x001_Median_Datensatzumbenennung_final_1.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_50)) + 
  geom_boxplot(outlier.colour = "black", outlier.size = 0.75) +
  geom_point(data = subset(subdata, visible_symbols == "B6"), aes(shape = factor("B6"), y = 1.20), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS"), aes(shape = factor("DS"), y = 1.15), size = 2, position = position_dodge(width = 1)) +
  
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  
  scale_shape_manual(values=c(4,3,8),labels=c("B6","DS","OG"))+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Effekt der Datensatzgröße: mAP_50 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

#######################################################################################################################

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(mAP_50 ~ versuch, data = subdata)
kruskal_result

# Convert result to data.frame
kruskal_df <- data.frame(statistic = kruskal_result$statistic,
                         parameter = kruskal_result$parameter,
                         p.value = kruskal_result$p.value,
                         method = kruskal_result$method,
                         data.name = kruskal_result$data.name)

# Load FSA library
library(FSA)

# Perform Dunn test
dunn_result <- dunnTest(mAP_50 ~ versuch, data=subdata, method="bonferroni")

# Calculate effect size r
n <- 10
dunn_result$res$r <- dunn_result$res$Z / sqrt(n)

# Add column indicating significance
alpha <- 0.05
dunn_result$res$significant <- ifelse(dunn_result$res$P.adj < alpha, "Ja", "Nein")

# Add column indicating strength of effect size
dunn_result$res$effect_size_strength <- ifelse(abs(dunn_result$res$r) < 0.1, "vernachlässigbar",
                                               ifelse(abs(dunn_result$res$r) < 0.3, "klein",
                                                      ifelse(abs(dunn_result$res$r) < 0.5, "mittel", "groß")))

# Add column n to the results table before the effect size column
dunn_result$res$n <- n

# Reorder columns to move n before r
dunn_result$res <- dunn_result$res[, c("Comparison", "Z", "P.unadj", "P.adj", "n", "r", "significant", "effect_size_strength")]

# Display results of Dunn test
dunn_result$res

# Create a list of data frames to write to the XLSX file
data_to_write <- list("Kruskal-Wallis" = kruskal_df, "Dunn Test" = dunn_result$res)

# Write data to XLSX file
write_xlsx(data_to_write, "kruskal_dunn_results_Datensatzgröße_mAP_50_10E_5x001_Median_Datensatzumbenennung_final.xlsx")

##############################################################################################

# Effekt der Datensatzgröße: mAP_95, 10 Epochen, 5x001, Median-Version, Datensatzumbenennung, finale Version, 1

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "001","003"))

subdata$visible_symbols <- ifelse(subdata$versuch == "001", c("B6"),
                                  ifelse(subdata$versuch == "003", c("DS"),
                                         
                                         ifelse(subdata$versuch == "006", c("OG"),NA)))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_95 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_95 ~ versuch, data = subdata_median, median)

pdf("Abb_Datensatzgrößeneffekt__mAP_95_10E_5x001_Median_Datensatzumbenennung_final_1.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_95)) + 
  geom_boxplot(outlier.colour = "black", outlier.size = 0.75) +
  geom_point(data = subset(subdata, visible_symbols == "B6"), aes(shape = factor("B6"), y = 1.20), size = 2, position = position_dodge(width = 1)) +
  geom_point(data = subset(subdata, visible_symbols == "DS"), aes(shape = factor("DS"), y = 1.15), size = 2, position = position_dodge(width = 1)) +
  
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  
  scale_shape_manual(values=c(4,3,8),labels=c("B6","DS","OG"))+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Effekt der Datensatzgröße: mAP_95 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

#########################################################################################################################

# Perform Kruskal-Wallis test
kruskal_result <- kruskal.test(mAP_95 ~ versuch, data = subdata)
kruskal_result

# Convert result to data.frame
kruskal_df <- data.frame(statistic = kruskal_result$statistic,
                         parameter = kruskal_result$parameter,
                         p.value = kruskal_result$p.value,
                         method = kruskal_result$method,
                         data.name = kruskal_result$data.name)

# Load FSA library
library(FSA)

# Perform Dunn test
dunn_result <- dunnTest(mAP_95 ~ versuch, data=subdata, method="bonferroni")

# Calculate effect size r
n <- 10
dunn_result$res$r <- dunn_result$res$Z / sqrt(n)

# Add column indicating significance
alpha <- 0.05
dunn_result$res$significant <- ifelse(dunn_result$res$P.adj < alpha, "Ja", "Nein")

# Add column indicating strength of effect size
dunn_result$res$effect_size_strength <- ifelse(abs(dunn_result$res$r) < 0.1, "vernachlässigbar",
                                               ifelse(abs(dunn_result$res$r) < 0.3, "klein",
                                                      ifelse(abs(dunn_result$res$r) < 0.5, "mittel", "groß")))

# Add column n to the results table before the effect size column
dunn_result$res$n <- n

# Reorder columns to move n before r
dunn_result$res <- dunn_result$res[, c("Comparison", "Z", "P.unadj", "P.adj", "n", "r", "significant", "effect_size_strength")]

# Display results of Dunn test
dunn_result$res

# Create a list of data frames to write to the XLSX file
data_to_write <- list("Kruskal-Wallis" = kruskal_df, "Dunn Test" = dunn_result$res)

# Write data to XLSX file
write_xlsx(data_to_write, "kruskal_dunn_results_Datensatzgröße_mAP_95_10E_5x001_Median_Datensatzumbennenung_final.xlsx")

##############################################################################################################################################

# Datenaugmentation (15): mAP_95, 10 Epochen, 5x001, Median-Version, Datensatzumbenennung, finale Version,1

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "014", "005",
                                                     "015","007","016","009","008",
                                                     "017", "018", "013", "019", "010",
                                                     "020","011"))

subdata$visible_symbols <- ifelse(subdata$versuch == "010", c("OG","SG", "SG_aug"),
                                  ifelse(subdata$versuch == "011", c("OG", "OG_aug", "SG", "SG_aug"),
                                         ifelse(subdata$versuch == "006", c("OG"),
                                                ifelse(subdata$versuch == "005", c("SG"),
                                                       ifelse(subdata$versuch == "007", c("OG", "OG_aug"),
                                                              ifelse(subdata$versuch == "008", c("OG","SG"),
                                                                     ifelse(subdata$versuch == "009", c("SG", "SG_aug"), 
                                                                            ifelse(subdata$versuch == "013", c("OG", "OG_aug", "SG"),
                                                                                   ifelse(subdata$versuch == "014", c("OG_aug"),
                                                                                          ifelse(subdata$versuch == "015", c("SG_aug"),
                                                                                                 ifelse(subdata$versuch == "016", c("OG_aug","SG"),
                                                                                                        ifelse(subdata$versuch == "017", c("OG_aug","SG_aug"),
                                                                                                               ifelse(subdata$versuch == "018", c("OG","SG_aug"),
                                                                                                                      ifelse(subdata$versuch == "019", c("OG_aug","SG","SG_aug"),
                                                                                                                             ifelse(subdata$versuch == "020", c("OG","OG_aug","SG_aug"), NA)))))))))))))))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_95 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_95 ~ versuch, data = subdata_median, median)

pdf("Abb_Datenaugmentation_15_mAP_95_10E_5x001_Median_Datensatzumbenennung_final_1.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_95)) + 
  geom_boxplot(outlier.colour = "black", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG_aug"),aes(shape=factor("OG_aug"),y=1.15),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG_aug"),aes(shape=factor("SG_aug"),y=1.05),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG"),aes(shape=factor("SG"),y=1.10),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,7,6,5),labels=c("OG","OG_aug","SG","SG_aug"))+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Datenaugmentation (Übersicht): mAP_95 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

#########################################################################################################################





######################################################################################################################

# Statistik (15 Datenaugmentations-Versuche): Vergleich linearer Modelle: mAP_50 

# Ändern Sie den Namen der Spalte "old_name" in "new_name"
colnames(subdata)[colnames(subdata) == "X800_SG_au"] <- "X800_SG_aug"

result <- aggregate(mAP_50 ~ X15_OG + X15_OG_aug + X800_SG + X800_SG_aug + version +Versuch, data = subdata, FUN = median)
# View the result
result

# Lineares Modell 1 (Summenmodell-Modell)

lm <- lm(mAP_50 ~ X15_OG + X15_OG_aug + X800_SG + X800_SG_aug,data =result)
summary(lm)
AIC(lm)

#Lineares Modell 2 (Interaktionsmodell)

lmi <- lm(mAP_50 ~ X15_OG * X15_OG_aug * X800_SG * X800_SG_aug,data =result)
summary(lmi)
AIC(lmi)

# Extract coefficients from the model
coefs <- summary(lmi)$coefficients

# Create a data frame with the coefficient names, values, and standard errors
coef_data <- data.frame(Trainingsdatensatzkombinationen = rownames(coefs), Koeffizienten = coefs[, 1], StdErr = coefs[, 2])

# Save the results of the second linear model and the AIC values of both models to an xlsx file
library(writexl)
write_xlsx(list("linear_model_results" = cbind(coef_data, AIC_lm1 = AIC(lm), AIC_lmi = AIC(lmi))), path = "linear_model_results_mAP_50.xlsx")


# Create a PDF file

pdf("Abb_LM_Koeffizienten_mAP_50.pdf",height=5, width=5)

# Create a bar plot of the coefficients with error bars
library(ggplot2)
ggplot(coef_data, aes(x = Trainingsdatensatzkombinationen, y = Koeffizienten)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Koeffizienten - StdErr, ymax = Koeffizienten + StdErr), width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add a title to the plot
  ggtitle("Koeffizienten des linearen Interaktionsmodells mit Standardfehlern: mAP_50") +
  theme(plot.title = element_text(size = 9, color = "black"))

dev.off()
#######################################################################################################################

# Statistik (15 Datenaugmentations-Versuche): Vergleich linearer Modelle: mAP_50 (angepasst Version)

# Ändern Sie den Namen der Spalte "old_name" in "new_name"
colnames(subdata)[colnames(subdata) == "X15_OG"] <- "OG"
colnames(subdata)[colnames(subdata) == "X15_OG_aug"] <- "OG_aug"
colnames(subdata)[colnames(subdata) == "X800_SG"] <- "SG"
colnames(subdata)[colnames(subdata) == "X800_SG_aug"] <- "SG_aug"

result <- aggregate(mAP_50 ~ OG + OG_aug + SG + SG_aug + version +Versuch, data = subdata, FUN = median)
# View the result
result

# Lineares Modell 1 (Summenmodell-Modell)

lm <- lm(mAP_50 ~ OG + OG_aug + SG + SG_aug,data =result)
summary(lm)
AIC(lm)

#Lineares Modell 2 (Interaktionsmodell)

lmi <- lm(mAP_50 ~ OG * OG_aug * SG * SG_aug,data =result)
summary(lmi)
AIC(lmi)

# Extract coefficients from the model
coefs <- summary(lmi)$coefficients

# Create a data frame with the coefficient names, values, and standard errors
coef_data <- data.frame(Trainingsdatensatzkombinationen = rownames(coefs), Koeffizienten = coefs[, 1], StdErr = coefs[, 2])

# Save the results of the second linear model and the AIC values of both models to an xlsx file
library(writexl)
write_xlsx(list("linear_model_results" = cbind(coef_data, AIC_lm1 = AIC(lm), AIC_lmi = AIC(lmi))), path = "linear_model_results_mAP_50_angepasst.xlsx")


# Create a PDF file

pdf("Abb_LM_Koeffizienten_mAP_50_angepasst.pdf",height=5, width=5)

# Create a bar plot of the coefficients with error bars
library(ggplot2)
ggplot(coef_data, aes(x = Trainingsdatensatzkombinationen, y = Koeffizienten)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Koeffizienten - StdErr, ymax = Koeffizienten + StdErr), width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add a title to the plot
  ggtitle("Koeffizienten des linearen Interaktionsmodells mit Standardfehlern: mAP_50") +
  theme(plot.title = element_text(size = 9, color = "black"))

dev.off()
########################################################################################################################

# Statistik (15 Datenaugmentations-Versuche): Vergleich linearer Modelle: mAP_95 

# Ändern Sie den Namen der Spalte "old_name" in "new_name"
colnames(subdata)[colnames(subdata) == "X800_SG_au"] <- "X800_SG_aug"

result <- aggregate(mAP_95 ~ X15_OG + X15_OG_aug + X800_SG + X800_SG_aug + version +Versuch, data = subdata, FUN = median)
# View the result
result

# Lineares Modell 1 (Summen-Modell)

lm <- lm(mAP_95 ~ X15_OG + X15_OG_aug + X800_SG + X800_SG_aug,data =result)
summary(lm)
AIC(lm)

#Lineares Modell 2 (Interaktionsmodell)

lmi <- lm(mAP_95 ~ X15_OG * X15_OG_aug * X800_SG * X800_SG_aug,data =result)
summary(lmi)
AIC(lmi)

# Extract coefficients from the model
coefs <- summary(lmi)$coefficients

# Create a data frame with the coefficient names, values, and standard errors
coef_data <- data.frame(Trainingsdatensatzkombinationen = rownames(coefs), Koeffizienten = coefs[, 1], StdErr = coefs[, 2])

# Save the results of the second linear model and the AIC values of both models to an xlsx file
library(writexl)
write_xlsx(list("linear_model_results" = cbind(coef_data, AIC_lm1 = AIC(lm), AIC_lmi = AIC(lmi))), path = "linear_model_results_mAP_95.xlsx")


# Create a PDF file

pdf("Abb_LM_Koeffizienten_mAP_95.pdf",height=5, width=5)

# Create a bar plot of the coefficients with error bars
library(ggplot2)
ggplot(coef_data, aes(x = Trainingsdatensatzkombinationen, y = Koeffizienten)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Koeffizienten - StdErr, ymax = Koeffizienten + StdErr), width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add a title to the plot
  ggtitle("Koeffizienten des linearen Interaktionsmodells mit Standardfehlern: mAP_95") +
  theme(plot.title = element_text(size = 9, color = "black"))

dev.off()

#####################################################################################################################

# Statistik (15 Datenaugmentations-Versuche): Vergleich linearer Modelle: mAP_95 (angepasst Version)

# Ändern Sie den Namen der Spalte "old_name" in "new_name"
colnames(subdata)[colnames(subdata) == "X15_OG"] <- "OG"
colnames(subdata)[colnames(subdata) == "X15_OG_aug"] <- "OG_aug"
colnames(subdata)[colnames(subdata) == "X800_SG"] <- "SG"
colnames(subdata)[colnames(subdata) == "X800_SG_aug"] <- "SG_aug"

result <- aggregate(mAP_95 ~ OG + OG_aug + SG + SG_aug + version +Versuch, data = subdata, FUN = median)
# View the result
result

# Lineares Modell 1 (Summen-Modell)

lm <- lm(mAP_95 ~ OG + OG_aug + SG + SG_aug,data =result)
summary(lm)
AIC(lm)

#Lineares Modell 2 (Interaktionsmodell)

lmi <- lm(mAP_95 ~ OG * OG_aug * SG * SG_aug,data =result)
summary(lmi)
AIC(lmi)

# Extract coefficients from the model
coefs <- summary(lmi)$coefficients

# Create a data frame with the coefficient names, values, and standard errors
coef_data <- data.frame(Trainingsdatensatzkombinationen = rownames(coefs), Koeffizienten = coefs[, 1], StdErr = coefs[, 2])

# Save the results of the second linear model and the AIC values of both models to an xlsx file
library(writexl)
write_xlsx(list("linear_model_results" = cbind(coef_data, AIC_lm1 = AIC(lm), AIC_lmi = AIC(lmi))), path = "linear_model_results_mAP_95_angepasst.xlsx")


# Create a PDF file

pdf("Abb_LM_Koeffizienten_mAP_95.pdf_angepasst",height=5, width=5)

# Create a bar plot of the coefficients with error bars
library(ggplot2)
ggplot(coef_data, aes(x = Trainingsdatensatzkombinationen, y = Koeffizienten)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Koeffizienten - StdErr, ymax = Koeffizienten + StdErr), width = 0.2) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # Add a title to the plot
  ggtitle("Koeffizienten des linearen Interaktionsmodells mit Standardfehlern: mAP_95") +
  theme(plot.title = element_text(size = 9, color = "black"))

dev.off()

write_xlsx(list("interaction_model_results" = cbind(summary(lmi), AIC_lmi = AIC(lmi))), path = "interaction_model_results_mAP_95_angepasst.xlsx")

####################################################################################################################

#####################################################################################################################

# Datenaugmentation (15): mAP_50, 10 Epochen, 5x001, Median-Version, Datensatzumbenennung, finale Version,1

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "014", "005",
                                                     "015","007","016","009","008",
                                                     "017", "018", "013", "019", "010",
                                                     "020","011"))

subdata$visible_symbols <- ifelse(subdata$versuch == "010", c("OG","SG", "SG_aug"),
                                  ifelse(subdata$versuch == "011", c("OG", "OG_aug", "SG", "SG_aug"),
                                         ifelse(subdata$versuch == "006", c("OG"),
                                                ifelse(subdata$versuch == "005", c("SG"),
                                                       ifelse(subdata$versuch == "007", c("OG", "OG_aug"),
                                                              ifelse(subdata$versuch == "008", c("OG","SG"),
                                                                     ifelse(subdata$versuch == "009", c("SG", "SG_aug"), 
                                                                            ifelse(subdata$versuch == "013", c("OG", "OG_aug", "SG"),
                                                                                   ifelse(subdata$versuch == "014", c("OG_aug"),
                                                                                          ifelse(subdata$versuch == "015", c("SG_aug"),
                                                                                                 ifelse(subdata$versuch == "016", c("OG_aug","SG"),
                                                                                                        ifelse(subdata$versuch == "017", c("OG_aug","SG_aug"),
                                                                                                               ifelse(subdata$versuch == "018", c("OG","SG_aug"),
                                                                                                                      ifelse(subdata$versuch == "019", c("OG_aug","SG","SG_aug"),
                                                                                                                             ifelse(subdata$versuch == "020", c("OG","OG_aug","SG_aug"), NA)))))))))))))))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_50 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_50 ~ versuch, data = subdata_median, median)

pdf("Abb_Datenaugmentation_15_mAP_50_10E_5x001_Median_Datensatzumbenennung_final_1.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_50)) + 
  geom_boxplot(outlier.colour = "black", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG_aug"),aes(shape=factor("OG_aug"),y=1.15),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG_aug"),aes(shape=factor("SG_aug"),y=1.05),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG"),aes(shape=factor("SG"),y=1.10),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,7,6,5),labels=c("OG","OG_aug","SG","SG_aug"))+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Datenaugmentation (Übersicht): mAP_50 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

###############################################################################################################################

# Effekt der konventionellen Datenaugmentation: mAP_95, 10 Epochen (schön), 5x001, Median-Version, Datensatzumbenennung

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "014", "007"))

subdata$visible_symbols <- ifelse(subdata$versuch == "006", c("OG"),
                                  ifelse(subdata$versuch == "007", c("OG", "OG_aug"),
                                         ifelse(subdata$versuch == "014", c("OG_aug"), NA)))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_95 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_95 ~ versuch, data = subdata_median, median)

pdf("Abb_Konventionelle_Datenaugmentation_mAP_95_10E_5x001_Median_Datensatzumbenennung.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_95)) + 
  geom_boxplot(outlier.colour = "darkgrey", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG_aug"),aes(shape=factor("OG_aug"),y=1.15),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,7),labels=c("OG","OG_aug"))+
  stat_summary(fun=median,geom='point',color='blue',size=2.5)+
  stat_summary(fun=median,geom='text',
               aes(label=round(..y..,digits=3)),
               vjust=-10.5,color='blue',size=3)+
  # Füge die Mediane als kleine rote Punkte hinzu
  geom_point(data=subdata_median,aes(x=versuch,y=mAP_95),color="red",size=0.5)+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=c(0.5,0.75,1))+
  ggtitle("Effekt der konventionellen Datenaugmentation: mAP_95 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))

dev.off()

##################################################################################################################


# Effekt der konventionellen Datenaugmentation: mAP_95, 10 Epochen (schön), 5x001, Median-Version, Datensatzumbenennung, finale Version

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "014", "007"))

subdata$visible_symbols <- ifelse(subdata$versuch == "006", c("OG"),
                                  ifelse(subdata$versuch == "007", c("OG", "OG_aug"),
                                         ifelse(subdata$versuch == "014", c("OG_aug"), NA)))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_95 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_95 ~ versuch, data = subdata_median, median)

pdf("Abb_Konventionelle_Datenaugmentation_mAP_95_10E_5x001_Median_Datensatzumbenennung_final.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_95)) + 
  geom_boxplot(outlier.colour = "darkgrey", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG_aug"),aes(shape=factor("OG_aug"),y=1.15),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,7),labels=c("OG","OG_aug"))+
  # Füge die Mediane als kleine rote Punkte hinzu
  geom_point(data=subdata_median,aes(x=versuch,y=mAP_95),color="red",size=0.5)+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Einfluss der konventionellen Datenaugmentation: mAP_95 der 
letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

#################################################################################################################

# Effekt der konventionellen Datenaugmentation (mAP_50), 10 Epochen (schön), Median-Version, Datensatzumbenennung

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "014", "007"))

subdata$visible_symbols <- ifelse(subdata$versuch == "006", c("OG"),
                                  ifelse(subdata$versuch == "007", c("OG", "OG_aug"),
                                         ifelse(subdata$versuch == "014", c("OG_aug"), NA)))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_50 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_50 ~ versuch, data = subdata_median, median)

pdf("Abb_Konventionelle_Datenaugmentation_mAP_50_10E_5x001_Median_Datensatzumbenennung.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_50)) + 
  geom_boxplot(outlier.colour = "darkgrey", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG_aug"),aes(shape=factor("OG_aug"),y=1.15),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,7),labels=c("OG","OG_aug"))+
  stat_summary(fun=median,geom='point',color='blue',size=2.5)+
  stat_summary(fun=median,geom='text',
               aes(label=round(..y..,digits=3)),
               vjust=-10.5,color='blue',size=3)+
  # Füge die Mediane als kleine rote Punkte hinzu
  geom_point(data=subdata_median,aes(x=versuch,y=mAP_50),color="red",size=0.5)+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=c(0.5,0.75,1))+
  ggtitle("Effekt der konventionellen Datenaugmentation: mAP_50 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))

dev.off()

#############################################################################################################

# Effekt der konventionellen Datenaugmentation (mAP_50), 10 Epochen (schön), Median-Version, Datensatzumbenennung, finale Version

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "014", "007"))

subdata$visible_symbols <- ifelse(subdata$versuch == "006", c("OG"),
                                  ifelse(subdata$versuch == "007", c("OG", "OG_aug"),
                                         ifelse(subdata$versuch == "014", c("OG_aug"), NA)))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_50 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_50 ~ versuch, data = subdata_median, median)

pdf("Abb_Konventionelle_Datenaugmentation_mAP_50_10E_5x001_Median_Datensatzumbenennung_final.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_50)) + 
  geom_boxplot(outlier.colour = "darkgrey", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG_aug"),aes(shape=factor("OG_aug"),y=1.15),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,7),labels=c("OG","OG_aug"))+
  # Füge die Mediane als kleine rote Punkte hinzu
  geom_point(data=subdata_median,aes(x=versuch,y=mAP_50),color="red",size=0.5)+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Einfluss der konventionellen Datenaugmentation: mAP_50 der 
letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

#############################################################################################################

# Effekt der synthetischen Datenaugmentation (mAP_95), 10 Epochen (schön),Median-Version, Datensatzumbenennung

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "005", "008"))

subdata$visible_symbols <- ifelse(subdata$versuch == "006", c("OG"),
                                  ifelse(subdata$versuch == "005", c("SG"),
                                         ifelse(subdata$versuch == "008", c("OG","SG"), NA)))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_95 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_95 ~ versuch, data = subdata_median, median)

pdf("Abb_Synthetische_Datenaugmentation__mAP_95_10E_5x001_Median_Datensatzumbenennung.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_95)) + 
  geom_boxplot(outlier.colour = "darkgrey", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG"),aes(shape=factor("SG"),y=1.10),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,6),labels=c("OG","SG"))+
  stat_summary(fun=median,geom='point',color='blue',size=2.5)+
  stat_summary(fun=median,geom='text',
               aes(label=round(..y..,digits=3)),
               vjust=-10.5,color='blue',size=3)+
  # Füge die Mediane als kleine rote Punkte hinzu
  geom_point(data=subdata_median,aes(x=versuch,y=mAP_95),color="red",size=0.5)+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=c(0.5,0.75,1))+
  ggtitle("Einfluss der synthetischen Datenaugmentation: mAP_95 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))

dev.off()

###############################################################################################################

# Effekt der synthetischen Datenaugmentation (mAP_95), 10 Epochen (schön),Median-Version, Datensatzumbenennung, finale Version

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "005", "008"))

subdata$visible_symbols <- ifelse(subdata$versuch == "006", c("OG"),
                                  ifelse(subdata$versuch == "005", c("SG"),
                                         ifelse(subdata$versuch == "008", c("OG","SG"), NA)))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_95 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_95 ~ versuch, data = subdata_median, median)

pdf("Abb_Synthetische_Datenaugmentation__mAP_95_10E_5x001_Median_Datensatzumbenennung_final.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_95)) + 
  geom_boxplot(outlier.colour = "darkgrey", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG"),aes(shape=factor("SG"),y=1.10),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,6),labels=c("OG","SG"))+
  # Füge die Mediane als kleine rote Punkte hinzu
  geom_point(data=subdata_median,aes(x=versuch,y=mAP_95),color="red",size=0.5)+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Einfluss der synthetischen Datenaugmentation: mAP_95 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

#############################################################################################################

# Effekt der synthetischen Datenaugmentation (mAP_50), 10 Epochen (schön),Median-Version, Datensatzumbenennung

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "005", "008"))

subdata$visible_symbols <- ifelse(subdata$versuch == "006", c("OG"),
                                  ifelse(subdata$versuch == "005", c("SG"),
                                         ifelse(subdata$versuch == "008", c("OG","SG"), NA)))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_50 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_50 ~ versuch, data = subdata_median, median)

pdf("Abb_Synthetische_Datenaugmentation__mAP_50_10E_5x001_Median_Datensatzumbenennung.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_50)) + 
  geom_boxplot(outlier.colour = "darkgrey", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG"),aes(shape=factor("SG"),y=1.10),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,6),labels=c("OG","SG"))+
  stat_summary(fun=median,geom='point',color='blue',size=2.5)+
  stat_summary(fun=median,geom='text',
               aes(label=round(..y..,digits=3)),
               vjust=-10.5,color='blue',size=3)+
  # Füge die Mediane als kleine rote Punkte hinzu
  geom_point(data=subdata_median,aes(x=versuch,y=mAP_50),color="red",size=0.5)+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=c(0.5,0.75,1))+
  ggtitle("Einfluss der synthetischen Datenaugmentation: mAP_50 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))

dev.off()

#####################################################################################################################

# Effekt der synthetischen Datenaugmentation (mAP_50), 10 Epochen (schön),Median-Version, Datensatzumbenennung, finale Version

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "005", "008"))

subdata$visible_symbols <- ifelse(subdata$versuch == "006", c("OG"),
                                  ifelse(subdata$versuch == "005", c("SG"),
                                         ifelse(subdata$versuch == "008", c("OG","SG"), NA)))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_50 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_50 ~ versuch, data = subdata_median, median)

pdf("Abb_Synthetische_Datenaugmentation__mAP_50_10E_5x001_Median_Datensatzumbenennung_final.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_50)) + 
  geom_boxplot(outlier.colour = "darkgrey", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG"),aes(shape=factor("SG"),y=1.10),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,6),labels=c("OG","SG"))+
  # Füge die Mediane als kleine rote Punkte hinzu
  geom_point(data=subdata_median,aes(x=versuch,y=mAP_50),color="red",size=0.5)+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Einfluss der synthetischen Datenaugmentation: mAP_50 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

#############################################################################################################

# maximaler Effekt der Datenaugmentation/kombinierte Datenaugmentation; mAP_95, 10 Epochen (5 Plots), Median, Datensatzumbenennung

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "007", "008", "013", "011"))

subdata$visible_symbols <- ifelse(subdata$versuch == "006", c("OG"),
                                  ifelse(subdata$versuch == "007", c("OG", "OG_aug"),
                                         ifelse(subdata$versuch == "008", c("OG","SG"),
                                                ifelse(subdata$versuch == "013", c("OG", "OG_aug", "SG"),
                                                       ifelse(subdata$versuch == "011", c("OG", "OG_aug", "SG", "SG_aug"), NA)))))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_95 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_95 ~ versuch, data = subdata_median, median)

pdf("Abb_Datenaugmentation_kombiniert_mAP_95_10E_5x001_Median_Datensatzumbenennung.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_95)) + 
  geom_boxplot(outlier.colour = "darkgrey", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG_aug"),aes(shape=factor("OG_aug"),y=1.15),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG"),aes(shape=factor("SG"),y=1.10),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG_aug"),aes(shape=factor("SG_aug"),y=1.05),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,7,6,5),labels=c("OG","OG_aug","SG","SG_aug"))+
  stat_summary(fun=median,geom='point',color='blue',size=2.5)+
  stat_summary(fun=median,geom='text',
               aes(label=round(..y..,digits=3)),
               vjust=-10.5,color='blue',size=3)+
  # Füge die Mediane als kleine rote Punkte hinzu
  geom_point(data=subdata_median,aes(x=versuch,y=mAP_95),color="red",size=0.5)+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=c(0.5,0.75,1))+
  ggtitle("Kombinierte Datenaugmentation: mAP_95 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))

dev.off()

##############################################################################################################

# maximaler Effekt der Datenaugmentation/kombinierte Datenaugmentation; mAP_95, 10 Epochen (5 Plots), Median, Datensatzumbenennung, finale Version

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "007", "008", "013", "011"))

subdata$visible_symbols <- ifelse(subdata$versuch == "006", c("OG"),
                                  ifelse(subdata$versuch == "007", c("OG", "OG_aug"),
                                         ifelse(subdata$versuch == "008", c("OG","SG"),
                                                ifelse(subdata$versuch == "013", c("OG", "OG_aug", "SG"),
                                                       ifelse(subdata$versuch == "011", c("OG", "OG_aug", "SG", "SG_aug"), NA)))))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_95 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_95 ~ versuch, data = subdata_median, median)

pdf("Abb_Datenaugmentation_kombiniert_mAP_95_10E_5x001_Median_Datensatzumbenennung_final.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_95)) + 
  geom_boxplot(outlier.colour = "darkgrey", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG_aug"),aes(shape=factor("OG_aug"),y=1.15),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG"),aes(shape=factor("SG"),y=1.10),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG_aug"),aes(shape=factor("SG_aug"),y=1.05),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,7,6,5),labels=c("OG","OG_aug","SG","SG_aug"))+
  # Füge die Mediane als kleine rote Punkte hinzu
  geom_point(data=subdata_median,aes(x=versuch,y=mAP_95),color="red",size=0.5)+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Kombinierte Datenaugmentation: mAP_95 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

#############################################################################################################

# maximaler Effekt der Datenaugmentation/kombinierte Datenaugmentation; mAP_50, 10 Epochen (5 Plots), Median, Datensatzumbenennung

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "007", "008", "013", "011"))

subdata$visible_symbols <- ifelse(subdata$versuch == "006", c("OG"),
                                  ifelse(subdata$versuch == "007", c("OG", "OG_aug"),
                                         ifelse(subdata$versuch == "008", c("OG","SG"),
                                                ifelse(subdata$versuch == "013", c("OG", "OG_aug", "SG"),
                                                       ifelse(subdata$versuch == "011", c("OG", "OG_aug", "SG", "SG_aug"), NA)))))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_50 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_50 ~ versuch, data = subdata_median, median)

pdf("Abb_Datenaugmentation_kombiniert_mAP_50_10E_5x001_Median_Datensatzumbenennung.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_50)) + 
  geom_boxplot(outlier.colour = "darkgrey", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG_aug"),aes(shape=factor("OG_aug"),y=1.15),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG"),aes(shape=factor("SG"),y=1.10),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG_aug"),aes(shape=factor("SG_aug"),y=1.05),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,7,6,5),labels=c("OG","OG_aug","SG","SG_aug"))+
  stat_summary(fun=median,geom='point',color='blue',size=2.5)+
  stat_summary(fun=median,geom='text',
               aes(label=round(..y..,digits=3)),
               vjust=8.5,color='blue',size=3)+
  # Füge die Mediane als kleine rote Punkte hinzu
  geom_point(data=subdata_median,aes(x=versuch,y=mAP_50),color="red",size=0.5)+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=c(0.5,0.75,1))+
  ggtitle("Kombinierte Datenaugmentation: mAP_50 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))

dev.off()

########################################################################################################################

# maximaler Effekt der Datenaugmentation/kombinierte Datenaugmentation; mAP_50, 10 Epochen (5 Plots), Median, Datensatzumbenennung, finale Version

subdata <- subset(data1df, (Epoche > 289) & (Epoche < 300) & !(versuch == "001" & SuperRank == 6))

subdata$versuch <- factor(subdata$versuch , levels=c("006", "007", "008", "013", "011"))

subdata$visible_symbols <- ifelse(subdata$versuch == "006", c("OG"),
                                  ifelse(subdata$versuch == "007", c("OG", "OG_aug"),
                                         ifelse(subdata$versuch == "008", c("OG","SG"),
                                                ifelse(subdata$versuch == "013", c("OG", "OG_aug", "SG"),
                                                       ifelse(subdata$versuch == "011", c("OG", "OG_aug", "SG", "SG_aug"), NA)))))

# Berechne den Median für jeden SuperRank
subdata_median <- aggregate(mAP_50 ~ versuch + SuperRank, data = subdata, median)

# Berechne den Median der Mediane für jeden Versuch
subdata_median_median <- aggregate(mAP_50 ~ versuch, data = subdata_median, median)

pdf("Abb_Datenaugmentation_kombiniert_mAP_50_10E_5x001_Median_Datensatzumbenennung_final.pdf",height=5, width=5)

ggplot(subdata_median, aes(x = versuch, y = mAP_50)) + 
  geom_boxplot(outlier.colour = "darkgrey", outlier.size = 0.25) +
  geom_point(data=subset(subdata,visible_symbols=="OG"),aes(shape=factor("OG"),y=1.2),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="OG_aug"),aes(shape=factor("OG_aug"),y=1.15),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG"),aes(shape=factor("SG"),y=1.10),size=2,position=position_dodge(width=1))+
  geom_point(data=subset(subdata,visible_symbols=="SG_aug"),aes(shape=factor("SG_aug"),y=1.05),size=2,position=position_dodge(width=1))+
  scale_shape_manual(values=c(8,7,6,5),labels=c("OG","OG_aug","SG","SG_aug"))+
  # Füge die Mediane als kleine rote Punkte hinzu
  geom_point(data=subdata_median,aes(x=versuch,y=mAP_50),color="red",size=0.5)+
  labs(shape="") + geom_hline(yintercept=1)+   
  scale_y_continuous(breaks=seq(0,1,0.1), labels=seq(0,1,0.1)) +
  ggtitle("Kombinierte Datenaugmentation: mAP_50 der letzten 10 Epochen") +
  theme(plot.title=element_text(color="black",size=9))+
  theme(axis.text.x=element_text(size=6))+
  theme(panel.grid.major.y = element_line(colour = "grey", size = 0.25),
        panel.grid.minor.y = element_line(colour = "grey", size = 0.125)) +
  scale_y_continuous(breaks = seq(0,1,0.1), minor_breaks = seq(0,1,0.01))

dev.off()

#######################################################################################################################

#Zusammenhang zwischen Bildanzahl und Trainingszeit

# Erstelle Vektoren für Trainingszeit und Bildanzahl
trainingszeit <- c(9.544, 113.401, 21.871, 4.854, 3.099, 2.856, 4.936, 11.607, 11.489, 12.316, 100.733, 5.162, 2.808, 9.625, 5.067, 9.924, 9.77, 11.583, 9.911)
bildanzahl <- c(3410, 53908, 8855, 800, 15, 75, 815, 4000, 4015, 4075, 44275, 875, 60, 3200, 860, 3260, 3215, 4060, 3275)

pdf("Abb_Bildanzahl_Trainingszeit.pdf",height=5, width=5)

# Ändere die Grafikparameter
par(cex.axis = 0.8, cex.lab =0.8, cex.main = 0.8, cex.sub = 0.5)

# Plotte Trainingszeit gegen Bildanzahl
plot(bildanzahl,
     trainingszeit,
     ylab = "Trainingszeit in h",
     xlab = "Anzahl der Trainingsbilder",
     main = "Abhängigkeit der Trainingszeit von der Trainingsdatensatzgröße",
     pch = 1,
     col = 2,
     cex = 1)


# Füge eine Regressionslinie hinzu
abline(lm(trainingszeit ~ bildanzahl))

# Zeige die Formel der Regressionsgerade an
formula <- paste("y =", round(coef(fit)[2], digits = 3), "x +", round(coef(fit)[1], digits = 3))
mtext(formula,
      side = 3,
      line = -12,
      cex = 0.8)

dev.off()
                                                                 
#######################################################################################################################
# Versuch 1 (001), mAP_50

versuch_001 <-subset(data1df, versuch == "001"& SuperRank != 6)

pdf("Abb_Versuch_mAP_50_001_5x.pdf",height=5, width=5)


my_plot <- ggplot(versuch_001)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50), size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 1: mAP_50") + theme(plot.title = element_text(color="black", size=9))
print(my_plot)


dev.off()


# Versuch 1 (001), mAP_95

# Filtern Sie die Daten, um nur Versuch 001 zu behalten und entfernen Sie Versuch 6
versuch_001 <- subset(data1df, versuch == "001" & SuperRank != 6)

pdf("Abb_Versuch_mAP_95_001_5x.pdf", height=5, width=5)

my_plot <- ggplot(versuch_001)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95), size=0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 1: mAP_95") + theme(plot.title = element_text(color="black", size=9))
print(my_plot)

dev.off()


# Versuch 1 (001), precision 

versuch_001 <-subset(data1df, versuch == "001" & SuperRank != 6)

pdf("Abb_Versuch_p_001_5x.pdf",height=5, width=5)


my_plot <- ggplot(versuch_001)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 1: Precision (Postiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))
print(my_plot)
print(my_plot)


dev.off()

# Versuch 1 (001), recall

versuch_001 <-subset(data1df, versuch == "001" & SuperRank != 6)

pdf("Abb_Versuch_r_001_5x.pdf",height=5, width=5)


my_plot <- ggplot(versuch_001)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 1: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

###################################################################################################

# Versuch 3 (003), mAP_50

versuch_003 <-subset(data1df, versuch == "003")

pdf("Abb_Versuch_mAP_50_003.pdf",height=5, width=5)


my_plot <- ggplot(versuch_003)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50), size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 3: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 3 (003), mAP_95

versuch_003 <-subset(data1df, versuch == "003")

pdf("Abb_Versuch_mAP_95_003.pdf",height=5, width=5)


my_plot <- ggplot(versuch_003)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 3: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 3 (003), precision 

versuch_003 <-subset(data1df, versuch == "003")

pdf("Abb_Versuch_p_003.pdf",height=5, width=5)


my_plot <- ggplot(versuch_003)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 3: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 3 (003), recall

versuch_003 <-subset(data1df, versuch == "003")

pdf("Abb_Versuch_r_003.pdf",height=5, width=5)


my_plot <- ggplot(versuch_003)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 3: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

#################################################################################################

# Versuch 4 (004), mAP_50

versuch_004 <-subset(data1df, versuch == "004")

pdf("Abb_Versuch_mAP_50_004.pdf",height=5, width=5)


my_plot <- ggplot(versuch_004)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 4: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 4 (004), mAP_95

versuch_004 <-subset(data1df, versuch == "004")

pdf("Abb_Versuch_mAP_95_004.pdf",height=5, width=5)


my_plot <- ggplot(versuch_004)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 4: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 4 (004), precision 

versuch_004 <-subset(data1df, versuch == "004")

pdf("Abb_Versuch_p_004.pdf",height=5, width=5)


my_plot <- ggplot(versuch_004)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 4: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 4 (004), recall

versuch_004 <-subset(data1df, versuch == "004")

pdf("Abb_Versuch_r_004.pdf",height=5, width=5)


my_plot <- ggplot(versuch_004)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 4: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

###############################################################################################

# Versuch 5 (005), mAP_50

versuch_005 <-subset(data1df, versuch == "005")

pdf("Abb_Versuch_mAP_50_005.pdf",height=5, width=5)


my_plot <- ggplot(versuch_005)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 5: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 5 (005), mAP_95

versuch_005 <-subset(data1df, versuch == "005")

pdf("Abb_Versuch_mAP_95_005.pdf",height=5, width=5)


my_plot <- ggplot(versuch_005)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 5: mAP:95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 5 (005), precision 

versuch_005 <-subset(data1df, versuch == "005")

pdf("Abb_Versuch_p_005.pdf",height=5, width=5)


my_plot <- ggplot(versuch_005)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 5: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 5 (005), recall

versuch_005 <-subset(data1df, versuch == "005")

pdf("Abb_Versuch_r_005.pdf",height=5, width=5)


my_plot <- ggplot(versuch_005)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 5: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

#################################################################################################

dev.off()

# Versuch 6 (006), mAP_50

versuch_006 <-subset(data1df, versuch == "006")

pdf("Abb_Versuch_mAP_50_006.pdf",height=5, width=5)


my_plot <- ggplot(versuch_006)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 6: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 6 (006), mAP_95

versuch_006 <-subset(data1df, versuch == "006")

pdf("Abb_Versuch_mAP_95_006.pdf",height=5, width=5)


my_plot <- ggplot(versuch_006)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 6: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 6 (006), precision 

versuch_006 <-subset(data1df, versuch == "006")

pdf("Abb_Versuch_p_006.pdf",height=5, width=5)


my_plot <- ggplot(versuch_006)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 6: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 6 (006), recall

versuch_006 <-subset(data1df, versuch == "006")

pdf("Abb_Versuch_r_006.pdf",height=5, width=5)


my_plot <- ggplot(versuch_006)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 6: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

################################################################################################



# Versuch 7 (007), mAP_50

versuch_007 <-subset(data1df, versuch == "007")

pdf("Abb_Versuch_mAP_50_007.pdf",height=5, width=5)


my_plot <- ggplot(versuch_007)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 7: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 7 (007), mAP_95

versuch_007 <-subset(data1df, versuch == "007")

pdf("Abb_Versuch_mAP_95_007.pdf",height=5, width=5)


my_plot <- ggplot(versuch_007)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 7: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 7 (007), precision 

versuch_007 <-subset(data1df, versuch == "007")

pdf("Abb_Versuch_p_007.pdf",height=5, width=5)


my_plot <- ggplot(versuch_007)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 7: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 7 (007), recall

versuch_007 <-subset(data1df, versuch == "007")

pdf("Abb_Versuch_r_007.pdf",height=5, width=5)


my_plot <- ggplot(versuch_007)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 7: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

################################################################################################

# Versuch 8 (008), mAP_50

versuch_008 <-subset(data1df, versuch == "008")

pdf("Abb_Versuch_mAP_50_008.pdf",height=5, width=5)


my_plot <- ggplot(versuch_008)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 8: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 8 (008), mAP_95

versuch_008 <-subset(data1df, versuch == "008")

pdf("Abb_Versuch_mAP_95_008.pdf",height=5, width=5)


my_plot <- ggplot(versuch_008)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 8: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 8 (008), precision 

versuch_008 <-subset(data1df, versuch == "008")

pdf("Abb_Versuch_p_008.pdf",height=5, width=5)


my_plot <- ggplot(versuch_008)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 8: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 8 (008), recall

versuch_008 <-subset(data1df, versuch == "008")

pdf("Abb_Versuch_r_008.pdf",height=5, width=5)


my_plot <- ggplot(versuch_008)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 8: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

################################################################################################

# Versuch 9 (009), mAP_50

versuch_009 <-subset(data1df, versuch == "009")

pdf("Abb_Versuch_mAP_50_009.pdf",height=5, width=5)


my_plot <- ggplot(versuch_009)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 9: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 9 (009), mAP_95

versuch_009 <-subset(data1df, versuch == "009")

pdf("Abb_Versuch_mAP_95_009.pdf",height=5, width=5)


my_plot <- ggplot(versuch_009)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 9: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 9 (009), precision 

versuch_009 <-subset(data1df, versuch == "009")

pdf("Abb_Versuch_p_009.pdf",height=5, width=5)


my_plot <- ggplot(versuch_009)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 9: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 9 (009), recall

versuch_009 <-subset(data1df, versuch == "009")

pdf("Abb_Versuch_r_009.pdf",height=5, width=5)


my_plot <- ggplot(versuch_009)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 9: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()
#################################################################################################

# Versuch 10 (010), mAP_50

versuch_010 <-subset(data1df, versuch == "010")

pdf("Abb_Versuch_mAP_50_010.pdf",height=5, width=5)


my_plot <- ggplot(versuch_010)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 10: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 10 (010), mAP_95

versuch_010 <-subset(data1df, versuch == "010")

pdf("Abb_Versuch_mAP_95_010.pdf",height=5, width=5)


my_plot <- ggplot(versuch_010)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 10: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 10 (010), precision 

versuch_010 <-subset(data1df, versuch == "010")

pdf("Abb_Versuch_p_010.pdf",height=5, width=5)


my_plot <- ggplot(versuch_010)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 10: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 10 (010), recall

versuch_010 <-subset(data1df, versuch == "010")

pdf("Abb_Versuch_r_010.pdf",height=5, width=5)


my_plot <- ggplot(versuch_010)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 10: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

################################################################################################

# Versuch 11 (011), mAP_50

versuch_011 <-subset(data1df, versuch == "011")

pdf("Abb_Versuch_mAP_50_011.pdf",height=5, width=5)


my_plot <- ggplot(versuch_011)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 11: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 11 (011), mAP_95

versuch_011 <-subset(data1df, versuch == "011")

pdf("Abb_Versuch_mAP_95_011.pdf",height=5, width=5)


my_plot <- ggplot(versuch_011)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 11: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 11 (011), precision 

versuch_011 <-subset(data1df, versuch == "011")

pdf("Abb_Versuch_p_011.pdf",height=5, width=5)


my_plot <- ggplot(versuch_011)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 11: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 11 (011), recall

versuch_011 <-subset(data1df, versuch == "011")

pdf("Abb_Versuch_r_011.pdf",height=5, width=5)


my_plot <- ggplot(versuch_011)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 11: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

###################################################################################################

# Versuch 12 (012), mAP_50

versuch_012 <-subset(data1df, versuch == "012")

pdf("Abb_Versuch_mAP_50_012.pdf",height=5, width=5)


my_plot <- ggplot(versuch_012)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 12: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 12 (012), mAP_95

versuch_012 <-subset(data1df, versuch == "012")

pdf("Abb_Versuch_mAP_95_012.pdf",height=5, width=5)


my_plot <- ggplot(versuch_012)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 12: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 12 (012), precision 

versuch_012 <-subset(data1df, versuch == "012")

pdf("Abb_Versuch_p_012.pdf",height=5, width=5)


my_plot <- ggplot(versuch_012)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 12: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

# Versuch 12 (012), recall

versuch_012 <-subset(data1df, versuch == "012")

pdf("Abb_Versuch_r_012.pdf",height=5, width=5)


my_plot <- ggplot(versuch_012)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 12: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)


dev.off()

###############################################################################################

# Versuch 13 (013), mAP_50

versuch_013 <-subset(data1df, versuch == "013")

pdf("Abb_Versuch_mAP_50_013.pdf",height=5, width=5)


my_plot <- ggplot(versuch_013)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 13: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 13 (013), mAP_95

versuch_013 <-subset(data1df, versuch == "013")

pdf("Abb_Versuch_mAP_95_013.pdf",height=5, width=5)


my_plot <- ggplot(versuch_013)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 13: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 13 (013), precision

versuch_013 <-subset(data1df, versuch == "013")

pdf("Abb_Versuch_p_013.pdf",height=5, width=5)


my_plot <- ggplot(versuch_013)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 13: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 13 (013), recall

versuch_013 <-subset(data1df, versuch == "013")

pdf("Abb_Versuch_r_013.pdf",height=5, width=5)


my_plot <- ggplot(versuch_013)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 13: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

##################################################################################################

# Versuch 14 (014), mAP_50

versuch_014 <-subset(data1df, versuch == "014")

pdf("Abb_Versuch_mAP_50_014.pdf",height=5, width=5)


my_plot <- ggplot(versuch_014)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 14: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 14 (014), mAP_95

versuch_014 <-subset(data1df, versuch == "014")

pdf("Abb_Versuch_mAP_95_014.pdf",height=5, width=5)


my_plot <- ggplot(versuch_014)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 14: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 14 (014), precision

versuch_014 <-subset(data1df, versuch == "014")

pdf("Abb_Versuch_p_014.pdf",height=5, width=5)


my_plot <- ggplot(versuch_014)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 14: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 14 (014), recall

versuch_014 <-subset(data1df, versuch == "014")

pdf("Abb_Versuch_r_014.pdf",height=5, width=5)


my_plot <- ggplot(versuch_014)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 14: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

##################################################################################################

# Versuch 15 (015), mAP_50

versuch_015 <-subset(data1df, versuch == "015")

pdf("Abb_Versuch_mAP_50_015.pdf",height=5, width=5)


my_plot <- ggplot(versuch_015)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 15: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 15 (015), mAP_95

versuch_015 <-subset(data1df, versuch == "015")

pdf("Abb_Versuch_mAP_95_015.pdf",height=5, width=5)


my_plot <- ggplot(versuch_015)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 15: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 15 (015), precision

versuch_015 <-subset(data1df, versuch == "015")

pdf("Abb_Versuch_p_015.pdf",height=5, width=5)


my_plot <- ggplot(versuch_015)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 15: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 15 (013), recall

versuch_015 <-subset(data1df, versuch == "015")

pdf("Abb_Versuch_r_015.pdf",height=5, width=5)


my_plot <- ggplot(versuch_015)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 15: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

###################################################################################################

# Versuch 16 (016), mAP_50

versuch_016 <-subset(data1df, versuch == "016")

pdf("Abb_Versuch_mAP_50_016.pdf",height=5, width=5)


my_plot <- ggplot(versuch_016)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 16: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 16 (016), mAP_95

versuch_016 <-subset(data1df, versuch == "016")

pdf("Abb_Versuch_mAP_95_016.pdf",height=5, width=5)


my_plot <- ggplot(versuch_016)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 16: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 16 (016), precision

versuch_016 <-subset(data1df, versuch == "016")

pdf("Abb_Versuch_p_016.pdf",height=5, width=5)


my_plot <- ggplot(versuch_016)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 16: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 16 (016), recall

versuch_016 <-subset(data1df, versuch == "016")

pdf("Abb_Versuch_r_016.pdf",height=5, width=5)


my_plot <- ggplot(versuch_016)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 16: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

#################################################################################################

# Versuch 17 (013), mAP_50

versuch_017 <-subset(data1df, versuch == "017")

pdf("Abb_Versuch_mAP_50_017.pdf",height=5, width=5)


my_plot <- ggplot(versuch_017)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 17: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 17 (017), mAP_95

versuch_017 <-subset(data1df, versuch == "017")

pdf("Abb_Versuch_mAP_95_017.pdf",height=5, width=5)


my_plot <- ggplot(versuch_017)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 17: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 17 (017), precision

versuch_017 <-subset(data1df, versuch == "017")

pdf("Abb_Versuch_p_017.pdf",height=5, width=5)


my_plot <- ggplot(versuch_017)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 17: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 17 (017), recall

versuch_017 <-subset(data1df, versuch == "017")

pdf("Abb_Versuch_r_017.pdf",height=5, width=5)


my_plot <- ggplot(versuch_017)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 17: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

###################################################################################################

# Versuch 18 (018), mAP_50

versuch_018 <-subset(data1df, versuch == "018")

pdf("Abb_Versuch_mAP_50_018.pdf",height=5, width=5)


my_plot <- ggplot(versuch_018)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 18: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 18 (018), mAP_95

versuch_018 <-subset(data1df, versuch == "018")

pdf("Abb_Versuch_mAP_95_018.pdf",height=5, width=5)


my_plot <- ggplot(versuch_018)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 18: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 18 (018), precision

versuch_018 <-subset(data1df, versuch == "018")

pdf("Abb_Versuch_p_018.pdf",height=5, width=5)


my_plot <- ggplot(versuch_018)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 18: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 18 (018), recall

versuch_018 <-subset(data1df, versuch == "018")

pdf("Abb_Versuch_r_018.pdf",height=5, width=5)


my_plot <- ggplot(versuch_018)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 18: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

################################################################################################

# Versuch 19 (019), mAP_50

versuch_019 <-subset(data1df, versuch == "019")

pdf("Abb_Versuch_mAP_50_019.pdf",height=5, width=5)


my_plot <- ggplot(versuch_019)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 19: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 19 (019), mAP_95

versuch_019 <-subset(data1df, versuch == "019")

pdf("Abb_Versuch_mAP_95_019.pdf",height=5, width=5)


my_plot <- ggplot(versuch_019)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 19: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 19 (019), precision

versuch_019 <-subset(data1df, versuch == "019")

pdf("Abb_Versuch_p_019.pdf",height=5, width=5)


my_plot <- ggplot(versuch_019)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 19: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 19 (019), recall

versuch_019 <-subset(data1df, versuch == "019")

pdf("Abb_Versuch_r_019.pdf",height=5, width=5)


my_plot <- ggplot(versuch_019)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 19: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

##################################################################################################

# Versuch 20 (020), mAP_50

versuch_020 <-subset(data1df, versuch == "020")

pdf("Abb_Versuch_mAP_50_020.pdf",height=5, width=5)


my_plot <- ggplot(versuch_020)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_50),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 20: mAP_50") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 20 (020), mAP_95

versuch_020 <-subset(data1df, versuch == "020")

pdf("Abb_Versuch_mAP_95_020.pdf",height=5, width=5)


my_plot <- ggplot(versuch_020)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=mAP_95),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 20: mAP_95") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 20 (020), precision

versuch_020 <-subset(data1df, versuch == "020")

pdf("Abb_Versuch_p_020.pdf",height=5, width=5)


my_plot <- ggplot(versuch_020)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=precision),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 20: Precision (Positiver Prädiktiver Wert)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

# Versuch 20 (020), recall

versuch_020 <-subset(data1df, versuch == "020")

pdf("Abb_Versuch_r_020.pdf",height=5, width=5)


my_plot <- ggplot(versuch_020)
my_plot <- my_plot + geom_point(aes(x=Epoche,y=recall),size = 0.1)
my_plot <- my_plot + facet_wrap(~ SuperRank , nrow=3)
my_plot <- my_plot + ggtitle("Experiment 20: Recall (Sensitivität)") + theme(plot.title = element_text(color="black", size=9))

print(my_plot)

dev.off()

#################################################################################################
#################################################################################################
#################################################################################################
###################################################################################################

#Statistik Teil:


# Grafische Untersuchung auf Normalverteilung (mAP_95, 10 Epochen, 5x 001)
subdata_all <- subset(data1df, (Epoche > 289) & (Epoche < 300) & versuch %in% c("001", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020") & !(versuch == "001" & SuperRank == 6))
pdf("Histogramm_alleVersuche_mAP_95_10E_5x001.pdf", height=5, width=5)

ggplot(subdata_all, aes(x = mAP_95)) +
  geom_histogram(binwidth = 0.01) +
  facet_wrap(~versuch) +
  ggtitle("Histogramm der mAP_95-Werte für alle Versuche in den letzten 10 Epochen") +
  theme(plot.title = element_text(color = "black", size = 9),
        axis.text.x = element_text(size = 8))

dev.off()

################################################################################################
# Grafische Untersuchung auf Normalverteilung (mAP_50, 10 Epochen, 5x 001)

subdata_all <- subset(data1df, (Epoche > 289) & (Epoche < 300) & versuch %in% c("001", "003", "004", "005", "006", "007", "008", "009", "010", "011", "012", "013", "014", "015", "016", "017", "018", "019", "020") & !(versuch == "001" & SuperRank == 6))
pdf("Histogramm_alleVersuche_mAP_50_10E_5x001.pdf", height=5, width=5)

ggplot(subdata_all, aes(x = mAP_50)) +
  geom_histogram(binwidth = 0.01) +
  facet_wrap(~versuch) +
  ggtitle("Histogramm der mAP_95-Werte für alle Versuche in den letzten 10 Epochen") +
  theme(plot.title = element_text(color = "black", size = 9),
        axis.text.x = element_text(size = 5))

dev.off()
###################################################################################################


#weitere Tabellen

install.packages("tidyr")
library(tidyr)
####################################################################################################




























































########################################################################################################################

result <- aggregate(mAP_95 ~ X15_OG + X15_OG_aug + X800_SG + X800_SG_au + version +Versuch, data = subdata, FUN = median)
# View the result
result

kruskal_result <- kruskal.test(mAP_95 ~ Versuch, data = result)
kruskal_result

lm <- lm(mAP_95 ~ X15_OG + X15_OG_aug + X800_SG + X800_SG_au,data =result)
summary(lm)
AIC(lm)

lmi <- lm(mAP_95 ~ X15_OG * X15_OG_aug * X800_SG * X800_SG_au,data =result)
summary(lmi)
AIC(lmi)



###################################################################################################################################

result <- aggregate(mAP_95 ~ X15_OG + X15_OG_aug + X800_SG + X800_SG_au + version +Versuch, data = subdata, FUN = median)
# View the result
result

kruskal_result <- kruskal.test(mAP_95 ~ Versuch, data = result)
kruskal_result


lm <- lm(mAP_95 ~ X15_OG + X15_OG_aug + X800_SG + X800_SG_au,data =result)
summary(lm)
AIC(lm)

lmi <- lm(mAP_95 ~ X15_OG * X15_OG_aug * X800_SG * X800_SG_au,data =result)
summary(lmi)
AIC(lmi)

########################################################################################################################

result <- aggregate(mAP_95 ~ X15_OG + X15_OG_aug + X800_SG + X800_SG_au + version +Versuch, data = subdata, FUN = median)
# View the result
result

kruskal_result <- kruskal.test(mAP_95 ~ Versuch, data = result)
kruskal_result

lm <- lm(mAP_95 ~ X15_OG + X15_OG_aug + X800_SG + X800_SG_au,data =result)
summary(lm)
AIC(lm)

lmi <- lm(mAP_95 ~ X15_OG * X15_OG_aug * X800_SG * X800_SG_au,data =result)
summary(lmi)
AIC(lmi)