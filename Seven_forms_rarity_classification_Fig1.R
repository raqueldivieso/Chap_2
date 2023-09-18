## Identification and classification of rare species

library(dplyr)
library(tidyverse)

# First, we will utilize imputed data for body size and habitat breadth, predicted densities from Santini et al.'s 2022 study, and calculated range sizes (based on IUCN polygons) to classify all mammal species into various forms and levels of rarity.

data<-read.csv("data/trait_data_imputed.csv")
imp<-dplyr::select(data, iucn2020_binomial, order, habitat_breadth_n, adult_mass_g, freshwater, marine)
colnames(imp)<-c("binomial", "order", "habitat_breadth_n", "adult_mass_g", "freshwater", "marine")

range<- read.csv("data/Range_size.csv")
range<- dplyr::select(range, binomial, range_size)

dens<-read.csv("data/santini_pred_dens.csv")
colnames(dens)<- c("binomial", "predDens")
dens_obs<-read.csv("data/density_ind-km2.csv") 
colnames(dens_obs)<- c("x", "binomial", "obsDens")
dens_obs<-dplyr::select(dens_obs, binomial, obsDens)

# dim(range)
# dim(imp)
# dim(dens)

table(duplicated(range$binomial))
table(duplicated(imp$binomial))
table(duplicated(dens$binomial))
table(duplicated(dens_obs$binomial))

imp<- imp %>% distinct(binomial, .keep_all = TRUE)
dens<- dens %>% distinct(binomial, .keep_all = TRUE)
tail(dens)

data1<-merge(imp,range, by="binomial", all = T)
dim(data1)
data2<-merge(data1, dens, by="binomial", all = T)
table(duplicated(data2$binomial))
data2.2<-merge(data2, dens_obs, by="binomial", all.x = T)
table(duplicated(data2$binomial))


# check if there is any freshwater or marine species in complementary data of densities:
# write.csv(data2.2, "Removing_exclusive_freshwater.csv")

# The following species were removed from analysis: 
rem<-c("Callorhinus ursinus",
       "Cystophora cristata",
       "Enhydra lutris",
       "Erignathus barbatus",
       "Eschrichtius robustus",
       "Hydrurga leptonyx",
       "Leptonychotes weddellii",
       "Lobodon carcinophaga",
       "Odobenus rosmarus",
       "Ommatophoca rossii",
       "Orcaella brevirostris",
       "Orcaella heinsohni",
       "Pagophilus groenlandicus",
       "Phoca vitulina",
       "Phocoena phocoena",
       "Pusa caspica",
       "Pusa hispida",
       "Pusa sibirica",
       "Stenella coeruleoalba",
       "Tursiops truncatus",
       "Zalophus californianus")

length(rem)
data2.3 <- data2.2[!(data2.2$binomial %in% rem), ]
dim(data2.3) # 6243 spp
dim(data2.2)[1]-dim(data2.3)[1]

data2.3$predDens<-as.numeric(data2.3$predDens)
head(data2.3)

data2.3$density <- ifelse(is.na(data2.3$predDens), data2.3$obsDens, data2.3$predDens)

table(is.na(data2.3$density))


## Adult mass
## Range size
## Predicted density
## habitat breadth (1-9)

data<-dplyr::select(data2.3, binomial, adult_mass_g, range_size, density, habitat_breadth_n)
data<-rename(data, pred_density=density)
data<-na.omit(data)
dim(data)


### We can identify a significant correlation for both, predicted and observed density and body size:
data$pred_density<-as.numeric(data$pred_density)
reg2<- lm(na.omit(data[,c("adult_mass_g", "pred_density")], method = "spearman"))
summary(reg2) # p-value: 0.01039

### With observed densities
data$density_n_km2_l<-log(data$density_n_km2+1)
data2.2$obsDens_l<-log(data2.2$obsDens)
reg3<- lm(na.omit(data2.2[,c("adult_mass_g", "obsDens_l")]))
summary(reg3) # p-value: 0.009474

table(is.na(data))

# Set the percentiles for body size categories:
data$log_adult_mass_g<-log(data$adult_mass_g)
percentiles <- quantile(data$log_adult_mass_g, probs = c(0.25, 0.50, 0.75))

# 25%      50%      75% 
# 3.027473 4.257030 6.438547  

data_ext_small <- subset(data, log_adult_mass_g <= 3.027473)
data_small <- subset(data, log_adult_mass_g > 3.027473 & log_adult_mass_g <=4.257030)
data_medium <- subset(data, log_adult_mass_g > 4.257030 & log_adult_mass_g <=6.438547)
data_large <- subset(data, log_adult_mass_g > 6.438547)


data_ext_small$dp_l<-log(data_ext_small$pred_density) # predicted density log
data_ext_small$dp<- 1-(data_ext_small$dp_l/max(data_ext_small$dp_l, na.rm = T)) # predicted
data_small$dp_l<-log(data_small$pred_density) # predicted density log
data_small$dp<- 1-(data_small$dp_l/max(data_small$dp_l, na.rm = T)) # predicted density index***
data_medium$dp_l<-log(data_medium$pred_density) # predicted density log
data_medium$dp<- 1-(data_medium$dp_l/max(data_medium$dp_l, na.rm = T))
##
data_large$dp_l<-log(data_large$pred_density) # predicted density log
data_large$dp<- 1-(data_large$dp_l/max(data_large$dp_l, na.rm = T))

#### The medians of each group for density index
median(data_ext_small$dp, na.rm = T) #extra_small = 0.6030632
median(data_small$dp, na.rm = T) #small =  0.4020501
median(data_medium$dp, na.rm = T) #medium =  0.5280464
median(data_large$dp, na.rm = T) #large =  0.7665536

data_ext_small$dp_rare<-ifelse(data_ext_small$dp>=0.6030632, TRUE, FALSE)
data_small$dp_rare<-ifelse(data_small$dp>=0.4017021, TRUE, FALSE)
data_medium$dp_rare<-ifelse(data_medium$dp>=0.5280464, TRUE, FALSE)
data_large$dp_rare<-ifelse(data_large$dp>=0.7665536, TRUE, FALSE)

data_dp<-rbind(data_ext_small, data_small, data_medium, data_large)
head(data_dp)
data_dp<-dplyr::select(data_dp,binomial, dp, dp_rare )
table(data_dp$dp_rare)

#The median for range size
data$rs_l<-log(data$range) # range size log
data$rs<- 1-(data$rs_l/max(data$rs_l, na.rm = T)) # range size index***
hist(data$rs)

median(data$rs, na.rm = T)  #0.4020506
data$rs_rare<- ifelse(data$rs>=0.4020506, TRUE, FALSE)


#All species with habitat breadth = 1 are considering rare:
data$hb_rare<- ifelse(data$habitat_breadth_n==1, TRUE, FALSE)
table(data$hb_rare)

## Rank rarity = score to each of the eight cells of the model by adding 1 to the number of the three characteristics of species that exhibited values above the median.

dat<-merge(data, data_dp, by="binomial", all = F, no.dups = TRUE)

# This next code will attribute a specific classification according to Rabnowitz rarity types:
dat$rank<- ifelse(dat$rs_rare==TRUE & dat$hb_rare==TRUE & dat$dp_rare==TRUE, "H", ifelse(dat$rs_rare==TRUE & dat$hb_rare==FALSE & dat$dp_rare==TRUE, "G",ifelse(dat$rs_rare==TRUE & dat$hb_rare==TRUE & dat$dp_rare==FALSE, "F", ifelse(dat$rs_rare==TRUE & dat$hb_rare==FALSE & dat$dp_rare==FALSE, "E", ifelse(dat$rs_rare==FALSE & dat$hb_rare==TRUE & dat$dp_rare==TRUE, "D",ifelse(dat$rs_rare==FALSE & dat$hb_rare==FALSE & dat$dp_rare==TRUE, "C", ifelse(dat$rs_rare==FALSE & dat$hb_rare==TRUE & dat$dp_rare==FALSE, "B", "A")))))))

table(dat$rank)
#     RS  Pop  Hab
# A = F   F    F (commonness)         
# B = F   F    T (rarity level 1) 
# C = F   T    F (rarity level 1) 
# D = F   T    T (rarity level 2) 
# E = T   F    F (rarity level 1) 
# F = T   F    T (rarity level 2) 
# G = T   T    F (rarity level 2) 
# H = T   T    T (rarity level 3)

dat$rar_levels<-dat$rank
dat$rar_levels<-sub('A','0', dat$rar_levels)
dat$rar_levels<-sub('B','1', dat$rar_levels)
dat$rar_levels<-sub('C','1', dat$rar_levels)
dat$rar_levels<-sub('D','2', dat$rar_levels)
dat$rar_levels<-sub('E','1', dat$rar_levels)
dat$rar_levels<-sub('F','2', dat$rar_levels)
dat$rar_levels<-sub('G','2', dat$rar_levels)
dat$rar_levels<-sub('H','3', dat$rar_levels)

# ### Combining IUCN category information
category<-read.csv("data/category_IUCN.csv")

data_rank<-merge(dat, category, by="binomial", all.x = T)

y <- list()
y[[1]]<-subset(data_rank, rank== "A")
y[[2]]<-subset(data_rank, rank== "B")
y[[3]]<-subset(data_rank, rank== "C")
y[[4]]<-subset(data_rank, rank== "D")
y[[5]]<-subset(data_rank, rank== "E")
y[[6]]<-subset(data_rank, rank== "F")
y[[7]]<-subset(data_rank, rank== "G")
y[[8]]<-subset(data_rank, rank== "H")
y[[9]]<-subset(data_rank, rar_levels == "1")
y[[10]]<-subset(data_rank, rar_levels == "2")

cat<-data.frame(table(data_rank$category))

df<-  matrix(ncol = 8, nrow = 10)
colnames(df) <- cat$Var1
df<- data.frame(df)
rownames(df)<- c(LETTERS[seq( from = 1, to = 8 )], "L1", "L2")
df$Nspp<-"spp"

for(i in 1:10){
x<-data.frame(t(data.frame(table(y[[i]]$category))))
colnames(x)<-x[1,]
try({df$CR[i]<-x$CR[2]})
try({df$DD[i]<-x$DD[2]})
try({df$EN[i]<-x$EN[2]})
try({df$EW[i]<-x$EW[2]})
try({df$EX[i]<-x$EX[2]})
try({df$LC[i]<-x$LC[2]})
try({df$NT[i]<-x$NT[2]})
try({df$VU[i]<-x$VU[2]})
try({df$Nspp[i]<-sum(data.frame(table(y[[i]]$category))$Freq)})
}

df<-dplyr::select(df, LC, NT,VU, EN, CR, EW, EX, DD, Nspp)
write.csv(df, "results/Table_1_freq.csv")

df_p<-  matrix(ncol = 8, nrow = 10)
colnames(df_p) <- cat$Var1
df_p<- data.frame(df_p)
rownames(df_p)<- c(LETTERS[seq( from = 1, to = 8 )], "L1", "L2")
df$Nspp<-"spp"

for(i in 1:10){
  x<-data.frame(t(data.frame(table(y[[i]]$category))))
  colnames(x)<-x[1,]
  n<-try({sum(data.frame(table(y[[i]]$category))$Freq)})
  df_p$Nspp[i]<-n
  try({df_p$CR[i]<-as.numeric(x$CR[2])*100/n})
  try({df_p$DD[i]<-as.numeric(x$DD[2])*100/n})
  try({df_p$EN[i]<-as.numeric(x$EN[2])*100/n})
  try({df_p$EW[i]<-as.numeric(x$EW[2])*100/n})
  try({df_p$EX[i]<-as.numeric(x$EX[2])*100/n})
  try({df_p$LC[i]<-as.numeric(x$LC[2])*100/n})
  try({df_p$NT[i]<-as.numeric(x$NT[2])*100/n})
  try({df_p$VU[i]<-as.numeric(x$VU[2])*100/n})
}

df_p<-dplyr::select(df_p, LC, NT,VU, EN, CR, EW, EX, DD, Nspp)
write.csv(df_p, "results/Table_1_percent_ok.csv")


## Rarity by order

head(data_rank)
head(imp)
imp<-read.csv("data/trait_data_imputed.csv")
ord<-dplyr::select(imp, iucn2020_binomial, order)
ord<-rename(ord, binomial=iucn2020_binomial)
data_rank<-merge(data_rank, ord, by="binomial", all.x = T, all.y=F)
dim(data_rank)

ord<-data.frame(table(data_rank$order))$Var1

  for(j in 1:length(ord)){
o<-as.character(ord[1])
d<-subset(data_rank, order==o)

y <- list()
y[[1]]<-subset(d, rank== "A")
y[[2]]<-subset(d, rank== "B")
y[[3]]<-subset(d, rank== "C")
y[[4]]<-subset(d, rank== "D")
y[[5]]<-subset(d, rank== "E")
y[[6]]<-subset(d, rank== "F")
y[[7]]<-subset(d, rank== "G")
y[[8]]<-subset(d, rank== "H")
y[[9]]<-subset(d, rar_levels == "1")
y[[10]]<-subset(d, rar_levels == "2")

cat<-data.frame(table(data_rank$category))

df_o<-  matrix(ncol = 8, nrow = 10)
colnames(df_o) <- cat$Var1
df_o<- data.frame(df_o)
rownames(df_o)<- c(LETTERS[seq( from = 1, to = 8 )], "L1", "L2")
df_o$Nspp<-"spp"

for(i in 1:10){
  x<-data.frame(t(data.frame(table(y[[i]]$category))))
  colnames(x)<-x[1,]
  try({df_o$CR[i]<-x$CR[2]})
  try({df_o$DD[i]<-x$DD[2]})
  try({df_o$EN[i]<-x$EN[2]})
  try({df_o$EW[i]<-x$EW[2]})
  try({df_o$EX[i]<-x$EX[2]})
  try({df_o$LC[i]<-x$LC[2]})
  try({df_o$NT[i]<-x$NT[2]})
  try({df_o$VU[i]<-x$VU[2]})
  try({df_o$Nspp[i]<-sum(data.frame(table(y[[i]]$category))$Freq)})
  write.csv(df_o,  paste("results/tab_result_ord", paste(o, "_T1.csv", sep=""), sep="/"))
}}


head(data_rank)

df_op<- data.frame(matrix(ncol = 11, nrow = 25))
colnames(df_op)<- c(LETTERS[seq( from = 1, to = 8 )], "L1", "L2", "Nspp")
rownames(df_op)<-ord


for(p in 1:length(ord)){
  o<-as.character(ord[p])
  d<-subset(data_rank, order==o)
  df_op$A[p]<-length(which(d$rank=="A"))
  df_op$B[p]<-length(which(d$rank=="B"))
  df_op$C[p]<-length(which(d$rank=="C"))
  df_op$D[p]<-length(which(d$rank=="D"))
  df_op$E[p]<-length(which(d$rank=="E"))
  df_op$F[p]<-length(which(d$rank=="F"))
  df_op$G[p]<-length(which(d$rank=="G"))
  df_op$H[p]<-length(which(d$rank=="H"))
  df_op$L1[p]<-length(which(d$rar_levels=="1"))
  df_op$L2[p]<-length(which(d$rar_levels=="2"))
  df_op$Nspp[1]<-length(d$binomial)
  write.csv(df_op, "results/Rarity_forms_levels_by_order.csv")}
  
# Percentage for each specie

ord<-data.frame(table(data_rank$order))$Var1

for(j in 1:length(ord)){
  o<-as.character(ord[j])
  d<-subset(data_rank, order==o)
  
  y <- list()
  y[[1]]<-subset(d, rank== "A")
  y[[2]]<-subset(d, rank== "B")
  y[[3]]<-subset(d, rank== "C")
  y[[4]]<-subset(d, rank== "D")
  y[[5]]<-subset(d, rank== "E")
  y[[6]]<-subset(d, rank== "F")
  y[[7]]<-subset(d, rank== "G")
  y[[8]]<-subset(d, rank== "H")
  y[[9]]<-subset(d, rar_levels == "1")
  y[[10]]<-subset(d, rar_levels == "2")
  
  
  cat<-data.frame(table(data_rank$category))
  
  df_op<-  matrix(ncol = 8, nrow = 10)
  colnames(df_op) <- cat$Var1
  df_op<- data.frame(df_op)
  rownames(df_op)<- c(LETTERS[seq( from = 1, to = 8 )], "L1", "L2")
  df$Nspp<-"spp"
  
  for(i in 1:10){
    x<-data.frame(t(data.frame(table(y[[i]]$category))))
    colnames(x)<-x[1,]
    n<-try({sum(data.frame(table(y[[i]]$category))$Freq)})
    df_op$Nspp[i]<-n
    try({df_op$CR[i]<-round(as.numeric(x$CR[2])*100/n, 1)})
    try({df_op$DD[i]<-round(as.numeric(x$DD[2])*100/n, 1)})
    try({df_op$EN[i]<-round(as.numeric(x$EN[2])*100/n, 1)})
    try({df_op$EW[i]<-round(as.numeric(x$EW[2])*100/n, 1)})
    try({df_op$EX[i]<-round(as.numeric(x$EX[2])*100/n, 1)})
    try({df_op$LC[i]<-round(as.numeric(x$LC[2])*100/n, 1)})
    try({df_op$NT[i]<-round(as.numeric(x$NT[2])*100/n, 1)})
    try({df_op$VU[i]<-round(as.numeric(x$VU[2])*100/n, 1)})
   
    write.csv(df_op,  paste("results/tab_result_ord", paste("perc_", o, "_MS.csv", sep=""), sep="/"))
  }}


# Using anova to test if the highest levels of rarity are disproportional represented in IUCN most vulnerable categories.
dat<-read.csv("data/rank_seven_forms_rarity_final.csv")

dat<-dplyr::select(dat, rank, category)

dat$rar_levels1<-dat$rank
dat$rar_levels1<-sub('A','1', dat$rar_levels)
dat$rar_levels1<-sub('B','2', dat$rar_levels)
dat$rar_levels1<-sub('C','2', dat$rar_levels)
dat$rar_levels1<-sub('D','3', dat$rar_levels)
dat$rar_levels1<-sub('E','2', dat$rar_levels)
dat$rar_levels1<-sub('F','3', dat$rar_levels)
dat$rar_levels1<-sub('G','3', dat$rar_levels)
dat$rar_levels1<-sub('H','4', dat$rar_levels)

dat$cat<-dat$category
dat$cat<-sub('LC','1', dat$cat)
dat$cat<-sub('NT','2', dat$cat)
dat$cat<-sub('VU','3', dat$cat)
dat$cat<-sub('EN','4', dat$cat)
dat$cat<-sub('CR','5', dat$cat)
dat$cat<-sub('DD','6', dat$cat)
dat$cat<-sub('EW', NA, dat$cat)
dat$cat<-sub('EX', NA, dat$cat)

# Create a contingency table
contingency_table <- table(dat$category, dat$rar_levels1)

# Perform the chi-square test of independence
chi_square <- chisq.test(contingency_table)
# Print the test result
print(chi_square)



library(ggplot2)
head(dat)

rem<-c("EW", "EX")
dat <- dat[!(dat$category %in% rem), ]
table(dat$category)


cat_order <- c("LC", "NT", "VU", "EN", "CR", "DD")
dat$category <- factor(dat$category, levels = cat_order)

# Convert the Conservation column to a factor with the desired order
# data$Conservation <- factor(data$Conservation, levels = desired_order)

# Create a bar plot
plot <- ggplot(dat, aes(x = category, fill = factor(rar_levels1))) +
  geom_bar(color = "grey60", size = 0.1) +
  labs(x = "", y = "", fill = "") +
  scale_fill_manual(values = c("#ffeda0", "#f6b338", "#e9763f", "#d7002b"),
                    labels = c("Common", "Low rarity", "Moderate rarity", "High rarity")) +
  theme_minimal()
plot

# The break in the LC category bar was set in image editor.
ggsave("Fig3_IUCN-by-cat_ParityLevels_new_colors.tiff", plot, width = 8, height = 9, dpi = 500)

