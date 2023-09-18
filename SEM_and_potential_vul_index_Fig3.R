## SEM - this framework allow us to test and refine theoretical models by examining the underlying structure of the relationships between multiple variables simultaneously.
# Here we will examine the correlation between some life history traits in addition to the three main ones that define rarity in previous works. We start with the conceptual model and refine the structure of the model step by step until we reach the best model.We also includes in the model three types of anthropogenic habitat effect to test if the changes in habitat has the same contribution to the vulnerability patterns that we see today.

library(dplyr)
library(tidyverse)

# Organizing data

# den<-read.csv("data/density_ind-km2.csv") #2548 
den<-read.csv("data/santini_pred_dens.csv") #5087
cat<-read.csv("data/category_IUCN.csv") #145630 
vl<-read.csv("data/veg_loss_atualizada.csv") #5745
# table(duplicated(vl$binomial)) 
# vl <- vl %>% distinct(binomial, .keep_all = TRUE)
# write.csv(vl, "data/vegetation_loss.csv")
rs<-read.csv("data/Range_size.csv") #5855
imp<-read.csv("data/trait_data_imputed.csv") #6263
footp<-read.csv("data/footprint2018.csv")
obs_den<-read.csv("data/density_ind-km2.csv")
colnames(obs_den)<-c("x", "binomial", "obs_dens")
obs_den<-select(obs_den, binomial, obs_dens)
cel_ex<-read.csv("data/losted_area.csv")

head(imp)

imp<-select(imp, order, iucn2020_binomial, phylacine_binomial, phylacine_binomial,  adult_mass_g, litter_size_n, litters_per_year_n, interbirth_interval_d, det_diet_breadth_n, habitat_breadth_n, density_n_km2, marine)

# dfc<- data.frame(rs %>% group_by(binomial) %>% filter(n() > 1))
# write.csv(rs, "data/RangeSize.csv")
# table(duplicated(rs$binomial))
# rs <- rs %>% distinct(binomial, .keep_all = TRUE)

sum(!is.na(imp$adult_mass_g)) #6033
sum(!is.na(imp$litter_size_n)) #6110
sum(!is.na(imp$litters_per_year_n)) #6108
sum(!is.na(imp$interbirth_interval_d)) #6105
sum(!is.na(imp$det_diet_breadth_n)) #6087
sum(!is.na(imp$habitat_breadth_n)) #5656
sum(!is.na(imp$marine)) #5930
sum(!is.na(imp$marine)) #5930

imp<-rename(imp, binomial=iucn2020_binomial, binomial_p=phylacine_binomial)

footp<-select(footp, binomial, footprint2018)
footp<-rename(footp, footp=footprint2018)
rs<-select(rs, binomial, range_size)
vl<-select(vl, binomial, change_perc)
vl<-rename(vl, veg_loss=change_perc)
den<-rename(den, binomial=Species, mean_density=PredMd)

dat1<-merge(imp, cat, by="binomial", all.x = T)
dat2<-merge(dat1, vl, by="binomial", all.x = T)
dat3<-merge(dat2, rs, by="binomial", all.x = T)
dat4<-merge(dat3, den, by="binomial", all.x = T)
dat5<-merge(dat4, footp, by="binomial", all.x = T)
dat6<-merge(dat5, cel_ex, by="binomial_p", all.x = T)
dat7<-merge(dat6, obs_den, by="binomial", all.x = T )

dat7$mean_density<-as.numeric(dat7$mean_density)
table(is.na(dat7$mean_density))

dat7$mean_density_ok <- ifelse(is.na(dat7$mean_density), dat7$obs_dens, dat7$mean_density)

dat8<-select(dat7, binomial, order, adult_mass_g, litter_size_n,litters_per_year_n, interbirth_interval_d, det_diet_breadth_n, habitat_breadth_n, marine, category, range_size, mean_density_ok, veg_loss, footp, area.loss)

dat9<-na.omit(dat8)
dim(dat9)

# dat10<-(na.omit(dat9)) #4717: Add 285 spp by using the observed density.


write.csv(dat9, "data/traits_for_SEM_imputed.csv") #the exclusively aquatic freshwater species were removed manually

# pant<-read.csv("data/search_dens.csv")
# data5<-merge(dat4, pant, by="binomial", all=T)
# write.csv(data5, "traits_for_SEM_com_search.csv")


############

data<-read.csv("data/traits_for_SEM_imputed.csv")

# IUCN categories of treat
table(data$category)
# Binary levels
# LC   3437  ->> 0 
# NT   371   ->> 0
# VU   596   ->> 1
# CR   220   ->> 1
# EN   539   ->> 1
# EW   2     ->> 1
# EX   83    ->> 1
# DD   776   ->> NA (level 2 for trinary levels)

data$cat_bi<-data$category
data$cat_bi<-sub('LC','0', data$cat_bi)
data$cat_bi<-sub('NT','0', data$cat_bi)
data$cat_bi<-sub('VU','1', data$cat_bi)
data$cat_bi<-sub('EN','1', data$cat_bi)
data$cat_bi<-sub('CR','1', data$cat_bi)
data$cat_bi<-sub('EW','1', data$cat_bi)
data$cat_bi<-sub('EX','1', data$cat_bi)
data$cat_bi<-sub('DD','NA', data$cat_bi)

data$cat_tr<-data$category
data$cat_tr<-sub('LC','0', data$cat_tr)
data$cat_tr<-sub('NT','0', data$cat_tr)
data$cat_tr<-sub('VU','1', data$cat_tr)
data$cat_tr<-sub('EN','1', data$cat_tr)
data$cat_tr<-sub('CR','1', data$cat_tr)
data$cat_tr<-sub('EW','1', data$cat_tr)
data$cat_tr<-sub('EX','1', data$cat_tr)
data$cat_tr<-sub('DD','2', data$cat_tr)

# Now we create the followings levels of threat:
# 1= Least Concern
# 2= Near to threat
# 3= Vulnerable
# 4= Endanger
# 5= Criticaly endengered
# 6= Extinct and Extinct in the wild

# data$cat_n<-data$category
# data$cat_n<-gsub("LC", "1", data$cat_n)
# data$cat_n<-gsub("NT", "2", data$cat_n)
# data$cat_n<-gsub("VU", "3", data$cat_n)
# data$cat_n<-gsub("EN", "4", data$cat_n)
# data$cat_n<-gsub("CR", "5", data$cat_n)
# data$cat_n<-gsub("EX", "6", data$cat_n)
# data$cat_n<-gsub("EW", "6", data$cat_n)
# data$cat_n[data$cat_n == "DD"] <- NA

# cat_bi == just treated(1) or not (0) and DD as NAs.
# cat_tr == treated (1), not treated (0) and data deficient (2)
# cat_n == levels of treat and DD as NA.

# Keeping just the important variables for build the SEM:
df<-dplyr::select(data, binomial, order, adult_mass_g, mean_density_ok , litter_size_n, litters_per_year_n, interbirth_interval_d, habitat_breadth_n, det_diet_breadth_n, range_size, veg_loss, area.loss, footp, cat_bi, cat_tr, category)

hist(df$adult_mass_g)
min(df$adult_mass_g)
max(df$adult_mass_g)
df$am_l<-log(df$adult_mass_g) # adult mass log
df$am<- 1-df$am_l/(max(df$am_l, na.rm = T)) # body size index***
hist(df$am_l)
hist(df$am)

df$ls_l<-log(df$litter_size_n+1) # litter size log
df$ls<- 1-df$ls_l/(max(df$ls_l, na.rm = T)) # litter size index***
hist(df$ls_l)
hist(df$ls)

df$ly_l<-log(df$litters_per_year_n+1) # litter per year log
df$ly<- 1-df$ly_l/(max(df$ly_l, na.rm = T)) # litter per year index***
hist(df$ly_l)
hist(df$ly)

df$ii_l<-log(df$interbirth_interval_d) # interval inter birth (days) log
df$ii<- 1-df$ii_l/(max(df$ii_l, na.rm = T))  # interval inter birth index***
hist(df$ii_l)
hist(df$ii)

df$dp_l<-log(df$mean_density_ok+1) # predicted density log
df$dp<- 1-df$dp_l/(max(df$dp_l, na.rm = T)) # predicted density index***

df$rs_l<-log(df$range_size+1) # range size log
df$rs<- 1-df$rs_l/(max(df$rs_l, na.rm = T)) # range size index***

df$veg_loss<-abs(df$veg_loss)
df$vl_l<- log(df$veg_loss+1)
df$vl<- 1-df$vl_l/(max(df$vl_l, na.rm = T))

df$al_l<-log(df$area.loss+1) 
df$al<- 1-df$al_l/(max(df$al_l, na.rm = T))

df$fp_l<-log(df$footp+1)
df$fp<- 1-df$fp_l/(max(df$fp_l, na.rm = T))

df$hb<- 1-df$habitat_breadth_n/(max(df$habitat_breadth_n, na.rm = T))

df$db<- 1-df$det_diet_breadth_n/(max(df$det_diet_breadth_n , na.rm = T))

df<-rename(df, vb=cat_bi, vt=cat_tr)
df<-select(df, binomial, order, am, ls, ly, ii, db, hb, dp, rs, vl, fp, al, vb, vt)

head(df)

### independent variables:
# [,3] am == adult mass g log
# [,4] ls == litter size log
# [,5] ly == litters per year log
# [,6] ii == inter birth interval-days log
# [,7] db == diet breadth
# [,8] hb == habitat breadth
# [,9] dp == Predicted density index
# [,10] rs == range size index
# [,11] vl == vegetation loss index
# [,12] fp == foot print index
# [,13] al == historical losted area index


### dependent variables:
# [,14] vb == IUCN category - ONLY endanger or not
# [,19] vt == IUCN category - endanger, not end. and data deficient

## SEM

libs = c("piecewiseSEM","lme4","corrplot")
lapply(libs, require, character.only = TRUE)
rm(libs)
library(dplyr)
library(naniar)
library(tidyverse)
library(multcompView)
library(piecewiseSEM)
library(mvabund)
library(rsq)

### Histograms perda de habitat por 
# cm<-subset(df, df$vb=="0")
# vul<-subset(df, df$vb=="1")
# 
# hist(cm$al)
# hist(vul$al)

df[,3:13] = scale(df[,3:13], center = T, scale = T)
df$vb<-as.numeric(df$vb)

correl = cor(na.omit(df[,c("am","ls","ly","ii","rs","dp", "db", "hb", "vl", "fp", "al")], method = "spearman"))
corrplot(correl, method = 'number')

### the interval interbirth and litters per year are high correlated, we need to pick one. Lets see what have more data 
sum(!is.na(df$ii))
# sum(!is.na(df$ly))
# We have litter size data for 2143 spp, while we have data of interval interbirth for 1211 species. So, we will use the litter per year data.

# semz<-subset(df,!al==1 )
# boxplot(semz$al ~ as.numeric(semz$vt))
# vioplot(as.numeric(semz$vt), semz$al)
# 
# semz<-subset(df,!vl==1 )
# boxplot(semz$vl ~ as.numeric(semz$vt))
# 
# semz<-subset(df,!fp==1 )
# boxplot(semz$fp ~ as.numeric(semz$vt))

# LT<-subset(df, vt=="0")
# MT<-subset(df, vt=="1")
# DD<-subset(df, vt=="2")

# 
# hist(LT$vl, col="blue", breaks = 15)
# hist(MT$vl, add=T, col="red", breaks = 15)
# hist(DD$vl, add=T, col="yellow", breaks = 15)
# 
# hist(LT$al, col="blue")
# hist(MT$al, add=T, col="red")
# hist(DD$al, add=T, col="yellow")
# 
# hist(LT$fp, col="blue", breaks = 15)
# hist(MT$fp, add=T, col="red", breaks = 15)
# hist(DD$fp, add=T, col="yellow", breaks = 15)

table(is.na(df))

#model 1 - Conceptual model
mod1 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df)),
  glm(rs ~ hb + db, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df), family = binomial(link = "logit")))
summary(mod1) #AIC 1318.697   

# model 2 rs ~ al
mod2 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df)),
  glm(rs ~ hb + db + al, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df), family = binomial(link = "logit")))
summary(mod2)

# model 3 rs ~ vl
mod3 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df), family = binomial(link = "logit")))
summary(mod3)


# model 4 rs ~ dp
mod4 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df), family = binomial(link = "logit")))
summary(mod4)

# model 5 vb ~ am
mod5 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp + am, data = na.omit(df), family = binomial(link = "logit")))
summary(mod5)

# model 6 dp ~ hb
mod6 = psem(
  glm(dp ~ ls + ii + am + hb, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp + am, data = na.omit(df), family = binomial(link = "logit")))
summary(mod6)

# model 7 vb ~ ii
mod7 = psem(
  glm(dp ~ ls + ii + am + hb, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp + am + ii, data = na.omit(df), family = binomial(link = "logit")))
summary(mod7)


# model 8 dp ~ db
mod8 = psem(
  glm(dp ~ ls + ii + am + hb + db, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp + am + ii, data = na.omit(df), family = binomial(link = "logit")))
summary(mod8)

# model 9 rs ~ ls
mod9 = psem(
  glm(dp ~ ls + ii + am + hb + db, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp + ls, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp + am + ii, data = na.omit(df), family = binomial(link = "logit")))
summary(mod9)


# model 10 rs ~ fp
mod10 = psem(
  glm(dp ~ ls + ii + am + hb + db, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp + am + ii, data = na.omit(df), family = binomial(link = "logit")))
summary(mod10)

# model 11 vb ~ hb
mod11 = psem(
  glm(dp ~ ls + ii + am + hb + db, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp + am + ii + hb, data = na.omit(df), family = binomial(link = "logit")))
summary(mod11)

#model 12 vb ~ ls
mod12 = psem(
  glm(dp ~ ls + ii + am + hb + db, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp + am + ii + hb + ls, data = na.omit(df), family = binomial(link = "logit")))
summary(mod12)

# model 13 dp ~ fp
mod13 = psem(
  glm(dp ~ ls + ii + am + hb + db + fp, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp + am + ii + hb + ls, data = na.omit(df), family = binomial(link = "logit")))
summary(mod13)

# model 14 dp ~ vl
mod14 = psem(
  glm(dp ~ ls + ii + am + hb + db + fp + vl, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp + am + ii + hb + ls, data = na.omit(df), family = binomial(link = "logit")))
summary(mod14)

# model 15 rs ~ am
mod15 = psem(
  glm(dp ~ ls + ii + am + hb + db + fp + vl, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp + am, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp + am + ii + hb + ls, data = na.omit(df), family = binomial(link = "logit")))
summary(mod15)

# model 16 rs ~ ii
mod16 = psem(
  glm(dp ~ ls + ii + am + hb + db + fp + vl, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp + am + ii, data = na.omit(df)),
  glm(vb ~ dp + rs + vl + al + fp + am + ii + hb + ls, data = na.omit(df), family = binomial(link = "logit")))
summary(mod16)

# model 17 removing vb ~vl
mod17 = psem(
  glm(dp ~ ls + ii + am + hb + db + fp + vl, data = na.omit(df)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp + am + ii, data = na.omit(df)),
  glm(vb ~ dp + rs + al + fp + am + ii + hb + ls, data = na.omit(df), family = binomial(link = "logit")))
summary(mod17)

# model 18 removing rs ~ db
mod18 = psem(
  glm(dp ~ ls + ii + am + hb + db + fp + vl, data = na.omit(df)),
  glm(rs ~ hb + al + vl + dp + ls + fp + am + ii, data = na.omit(df)),
  glm(vb ~ dp + rs + al + fp + am + ii + hb + ls, data = na.omit(df), family = binomial))
summary(mod18)

# model 19 removing  vb ~ dp
mod19 = psem(
  glm(dp ~ ls + ii + am + hb + db + fp + vl, data = na.omit(df)),
  glm(rs ~ hb + al + vl + dp + ls + fp + am + ii, data = na.omit(df)),
  glm(vb ~ rs + al + fp + am + ii + hb + ls, data = na.omit(df)))
summary(mod19) # AIC=  67.274


# Residual inspections
eq1 = glm(dp ~ ls + ii + am + hb + db + fp + vl, data = na.omit(df))
plot(eq1)
eq2 =  glm(rs ~ hb + al + vl + dp + ls + fp + am + ii, data = na.omit(df))
plot(eq2)
eq3 = glm(vb ~ rs + al + fp + am + ii + hb + ls, data = na.omit(df), family = binomial(link = "logit"))
plot(eq3)

res<-data.frame(residuals(mod19))$vb_residuals

plot(res, mod19$data$rs)
rlm<-glm(mod19$data$rs ~ res)
summary(rlm)
abline(rlm)

plot(res, eq3$data$al)
plot(res, eq3$data$fp)
plot(res, eq3$data$am)
plot(res, eq3$data$ii)
plot(res, eq3$data$hb)
plot(res, eq3$data$ls)

ma<-manyglm(df$vb ~ df$rs + df$al + df$fp + df$am + df$ii + df$hb + df$ls +df$dp, family = "binomial")
resid<-data.frame(residuals(ma))
d<-lm(ma$data$`df$rs` ~ resid$residuals.ma.)
summary(d)

# boxplot(df$dp ~ df$vb)
# boxplot(df$rs ~ df$vb)
# boxplot(df$am ~ df$vb)
# boxplot(df$ii ~ df$vb)
# boxplot(df$hb ~ df$vb)
# boxplot(df$ls ~ df$vb)
# boxplot(df$db ~ df$vb)
# boxplot(df$fp ~ df$vb)
# boxplot(df$al ~ df$vb)


#R² for vulnerability equation
model1 <- glm(vb ~ rs + al + fp + am + ii + hb + ls, data = na.omit(df), family = binomial(link = "logit"))
rsq::rsq(model1)

## Models by orders:

libs = c("piecewiseSEM","lme4","corrplot")
lapply(libs, require, character.only = TRUE)
rm(libs)
library(dplyr)
library(naniar)
library(tidyverse)
library(multcompView)

f<-data.frame(table(df$order))
orders <- f$Var1[f$Freq > 100]


#### Carnivora
orders[1]
df_car<-subset(df, order=="Carnivora")

df_car[,3:13] = scale(df_car[,3:13], center = T, scale = T)
df_car$vb<-as.numeric(df_car$vb)
length(df_car$binomial)-
sum(table(df_car$vb)[1], table(df_car$vb)[2])
#4 species as DD

correl = cor(na.omit(df_car[,c("am","ls","ly","ii","rs","dp", "db", "hb", "vl", "fp", "al")], method = "spearman"))
corrplot(correl, method = 'number')

#(A) Conceptual model and (B) structural equation modeling (SEM)

#model 1 - Conceptual model
mod1 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_car)),
  glm(rs ~ hb + db, data = na.omit(df_car)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_car), family = binomial(link = "logit")))
summary(mod1) #AIC 155.642   / Fisher's C = 125.642

#Model 2 including rs ~ al
mod2 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_car)),
  glm(rs ~ hb + db + al, data = na.omit(df_car)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_car), family = binomial(link = "logit")))
summary(mod2)

#Model 3 including dp ~ db
mod3 = psem(
  glm(dp ~ ls + ii + am + db, data = na.omit(df_car)),
  glm(rs ~ hb + db + al, data = na.omit(df_car)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_car), family = binomial(link = "logit")))
summary(mod3)

#Model 4 including  rs ~ fp
mod4 = psem(
  glm(dp ~ ls + ii + am + db, data = na.omit(df_car)),
  glm(rs ~ hb + db + al + fp, data = na.omit(df_car)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_car), family = binomial(link = "logit")))
summary(mod4)

#Model 5 including  rs ~ vl
mod5 = psem(
  glm(dp ~ ls + ii + am + db, data = na.omit(df_car)),
  glm(rs ~ hb + db + al + fp + vl, data = na.omit(df_car)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_car), family = binomial(link = "logit")))
summary(mod5)

#Model 6 including  vb ~ hb
mod6 = psem(
  glm(dp ~ ls + ii + am + db, data = na.omit(df_car)),
  glm(rs ~ hb + db + al + fp + vl, data = na.omit(df_car)),
  glm(vb ~ dp + rs + vl + al + fp + hb, data = na.omit(df_car), family = binomial(link = "logit")))
summary(mod6)

#Model 7 including  vb ~ am
mod7 = psem(
  glm(dp ~ ls + ii + am + db, data = na.omit(df_car)),
  glm(rs ~ hb + db + al + fp + vl, data = na.omit(df_car)),
  glm(vb ~ dp + rs + vl + al + fp + hb + am, data = na.omit(df_car), family = binomial(link = "logit")))
summary(mod7)


#Model 8 removing  dp~ls
mod8 = psem(
  glm(dp ~ ii + am + db, data = na.omit(df_car)),
  glm(rs ~ hb + db + al + fp + vl, data = na.omit(df_car)),
  glm(vb ~ dp + rs + vl + al + fp + hb + am, data = na.omit(df_car), family = binomial(link = "logit")))
summary(mod8)

#Model 9 removing  dp~ii
mod9 = psem(
  glm(dp ~ am + db, data = na.omit(df_car)),
  glm(rs ~ hb + db + al + fp + vl, data = na.omit(df_car)),
  glm(vb ~ dp + rs + vl + al + fp + hb + am, data = na.omit(df_car), family = binomial(link = "logit")))
summary(mod9)


#Model 10 removing  vb~dp 
mod10 = psem(
  glm(dp ~ am + db, data = na.omit(df_car)),
  glm(rs ~ hb + db + al + fp + vl, data = na.omit(df_car)),
  glm(vb ~ rs + vl + al + hb + am, data = na.omit(df_car), family = binomial(link = "logit")))
summary(mod10)


#Model 11 removing vb~vl
mod11 = psem(
  glm(dp ~ am + db, data = na.omit(df_car)),
  glm(rs ~ hb + db + al + fp + vl, data = na.omit(df_car)),
  glm(vb ~ rs + al + hb + am, data = na.omit(df_car), family = binomial(link = "logit")))
summary(mod11) # AIC= 48.267


#R² for vulnerability equation
model1 <- glm(vb ~ rs + al + hb + am, data = na.omit(df_car), family = binomial(link = "logit"))
rsq(model1)

summary(model)
with(summary(model), 1 - deviance/null.deviance)
install.packages('rsq')
library(rsq)
#######
rm(list=ls())
#### Cetartiodactyla
orders[2]
df_cet<-subset(df, order=="Cetartiodactyla")

df_cet[,3:13] = scale(df_cet[,3:13], center = T, scale = T)
df_cet$vb<-as.numeric(df_cet$vb)
table(is.na(df_cet$vb))#11spp DD #217spp for analysis

correl = cor(na.omit(df_cet[,c("am","ls","ly","ii","rs","dp", "db", "hb", "vl", "fp", "al")], method = "spearman"))
corrplot(correl, method = 'number')

#model 1 - Conceptual model
mod1 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_cet)),
  glm(rs ~ hb + db, data = na.omit(df_cet)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_cet), family = binomial(link = "logit")))
summary(mod1) #AIC= 108.409

#model 2 including rs ~ al
mod2 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_cet)),
  glm(rs ~ hb + db + al, data = na.omit(df_cet)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_cet), family = binomial(link = "logit")))
summary(mod2)

#model 3 including rs ~ vl
mod3 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_cet)),
  glm(rs ~ hb + db + al + vl, data = na.omit(df_cet)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_cet), family = binomial(link = "logit")))
summary(mod3)

#model 4 including dp ~ al
mod4 = psem(
  glm(dp ~ ls + ii + am + al, data = na.omit(df_cet)),
  glm(rs ~ hb + db + al + vl, data = na.omit(df_cet)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_cet), family = binomial(link = "logit")))
summary(mod4)

#model 5 including dp ~ hb
mod5 = psem(
  glm(dp ~ ls + ii + am + al + hb, data = na.omit(df_cet)),
  glm(rs ~ hb + db + al + vl, data = na.omit(df_cet)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_cet), family = binomial(link = "logit")))
summary(mod5)


#model 6 including vb ~ hb
mod6 = psem(
  glm(dp ~ ls + ii + am + al + hb, data = na.omit(df_cet)),
  glm(rs ~ hb + db + al + vl, data = na.omit(df_cet)),
  glm(vb ~ dp + rs + vl + al + fp + hb, data = na.omit(df_cet), family = binomial(link = "logit")))
summary(mod6)

#model 7 removing dp ~ ls
mod7 = psem(
  glm(dp ~ ii + am + al + hb, data = na.omit(df_cet)),
  glm(rs ~ hb + db + al + vl, data = na.omit(df_cet)),
  glm(vb ~ dp + rs + vl + al + fp + hb, data = na.omit(df_cet), family = binomial(link = "logit")))
summary(mod7)


#model 8 removing vb ~ dp
mod8 = psem(
  glm(dp ~ ii + am + al + hb, data = na.omit(df_cet)),
  glm(rs ~ hb + db + al + vl, data = na.omit(df_cet)),
  glm(vb ~ rs + vl + al + fp + hb, data = na.omit(df_cet), family = binomial(link = "logit")))
summary(mod8)

#model 9 removing rs ~ db 
mod9 = psem(
  glm(dp ~ ii + am + al + hb, data = na.omit(df_cet)),
  glm(rs ~ hb + al + vl, data = na.omit(df_cet)),
  glm(vb ~ rs + vl + al + fp + hb, data = na.omit(df_cet), family = binomial(link = "logit")))
summary(mod9)


#model 10 removing vb ~ fp 
mod10 = psem(
  glm(dp ~ ii + am + al + hb, data = na.omit(df_cet)),
  glm(rs ~ hb + al + vl, data = na.omit(df_cet)),
  glm(vb ~ rs + vl + al + hb, data = na.omit(df_cet), family = binomial(link = "logit")))
summary(mod10) #AIC=42.716

#R² for vulnerability equation
model2 <- glm(vb ~ rs + vl + al + hb, data = na.omit(df_cet), family = binomial(link = "logit"))
rsq(model2)


#####
#### Chiroptera
orders[3]
df_chi<-subset(df, order=="Chiroptera")

df_chi[,3:13] = scale(df_chi[,3:13], center = T, scale = T)
df_chi$vb<-as.numeric(df_chi$vb)
table(is.na(df_chi$vb)) #94spp DD #844spp for analysis

correl = cor(na.omit(df_chi[,c("am","ls","ly","ii","rs","dp", "db", "hb", "vl", "fp", "al")], method = "spearman"))
corrplot(correl, method = 'number')

#model 1 - Conceptual model
mod1 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_chi)),
  glm(rs ~ hb + db, data = na.omit(df_chi)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_chi), family = binomial(link = "logit")))
summary(mod1) #AIC= 446.141

#model 2 - including rs ~ vl
mod2 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_chi)),
  glm(rs ~ hb + db + vl, data = na.omit(df_chi)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_chi), family = binomial(link = "logit")))
summary(mod2) 

#model 3 - including rs ~ am 
mod3 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_chi)),
  glm(rs ~ hb + db + vl + al + am, data = na.omit(df_chi)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_chi), family = binomial(link = "logit")))
summary(mod3) 

#model 4 - including dp ~ vl
mod4 = psem(
  glm(dp ~ ls + ii + am + vl, data = na.omit(df_chi)),
  glm(rs ~ hb + db + vl + al + am, data = na.omit(df_chi)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_chi), family = binomial(link = "logit")))
summary(mod4) 

#model 5 - including  rs ~ dp
mod5 = psem(
  glm(dp ~ ls + ii + am + vl, data = na.omit(df_chi)),
  glm(rs ~ hb + db + vl + al + am + dp, data = na.omit(df_chi)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_chi), family = binomial(link = "logit")))
summary(mod5) 

#model 6 - including  dp ~ fp
mod6 = psem(
  glm(dp ~ ls + ii + am + vl + fp, data = na.omit(df_chi)),
  glm(rs ~ hb + db + vl + al + am + dp, data = na.omit(df_chi)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_chi), family = binomial(link = "logit")))
summary(mod6) 

#model 7 - including  rs ~ ii
mod7 = psem(
  glm(dp ~ ls + ii + am + vl + fp, data = na.omit(df_chi)),
  glm(rs ~ hb + db + vl + al + am + dp + ii, data = na.omit(df_chi)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_chi), family = binomial(link = "logit")))
summary(mod7) 

#model 8 - including  vb ~ hb
mod8 = psem(
  glm(dp ~ ls + ii + am + vl + fp, data = na.omit(df_chi)),
  glm(rs ~ hb + db + vl + al + am + dp + ii, data = na.omit(df_chi)),
  glm(vb ~ dp + rs + vl + al + fp + hb, data = na.omit(df_chi), family = binomial(link = "logit")))
summary(mod8) 

#model 9 - including  rs ~ ls
mod9 = psem(
  glm(dp ~ ls + ii + am + vl + fp, data = na.omit(df_chi)),
  glm(rs ~ hb + db + vl + al + am + dp + ii + ls, data = na.omit(df_chi)),
  glm(vb ~ dp + rs + vl + al + fp + hb, data = na.omit(df_chi), family = binomial(link = "logit")))
summary(mod9) 

#model 10 - removing  rs  ~  db 
mod10 = psem(
  glm(dp ~ ls + ii + am + vl + fp, data = na.omit(df_chi)),
  glm(rs ~ hb + vl + al + am + dp + ii + ls, data = na.omit(df_chi)),
  glm(vb ~ dp + rs + vl + al + fp + hb, data = na.omit(df_chi), family = binomial(link = "logit")))
summary(mod10) 


#model 11 - removing  vb ~  vl
mod11 = psem(
  glm(dp ~ ls + ii + am + vl + fp, data = na.omit(df_chi)),
  glm(rs ~ hb + vl + al + am + dp + ii + ls, data = na.omit(df_chi)),
  glm(vb ~ dp + rs + al + fp + hb, data = na.omit(df_chi), family = binomial(link = "logit")))
summary(mod11) 


#model 12 - removing  vb ~ fp 
mod12 = psem(
  glm(dp ~ ls + ii + am + vl + fp, data = na.omit(df_chi)),
  glm(rs ~ hb + vl + al + am + dp + ii + ls, data = na.omit(df_chi)),
  glm(vb ~ dp + rs + al + hb, data = na.omit(df_chi), family = binomial(link = "logit")))
summary(mod12) #AIC=62.755


#R² for vulnerability equation
model3 <- glm(vb ~ dp + rs + al + hb, data = na.omit(df_chi), family = binomial(link = "logit"))
rsq(model3)


#####
rm(list=ls())
#### Diprotodontia   
orders[4]
df_dip<-subset(df, order=="Diprotodontia")

df_dip[,3:13] = scale(df_dip[,3:13], center = T, scale = T)
df_dip$vb<-as.numeric(df_dip$vb)
table(is.na(df_dip$vb)) #0 spp DD #134spp for analysis

correl = cor(na.omit(df_dip[,c("am","ls","ly","ii","rs","dp", "db", "hb", "vl", "fp", "al")], method = "spearman"))
corrplot(correl, method = 'number')

#model 1 - Conceptual model
mod1 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_dip)),
  glm(rs ~ hb + db, data = na.omit(df_dip)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_dip), family = binomial(link = "logit")))
summary(mod1) #AIC=  89.969

#model 2 - including dp ~ al
mod2 = psem(
  glm(dp ~ ls + ii + am + al, data = na.omit(df_dip)),
  glm(rs ~ hb + db, data = na.omit(df_dip)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_dip), family = binomial(link = "logit")))
summary(mod2)

#model 3 - including rs ~ al
mod3 = psem(
  glm(dp ~ ls + ii + am + al, data = na.omit(df_dip)),
  glm(rs ~ hb + db + al, data = na.omit(df_dip)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_dip), family = binomial(link = "logit")))
summary(mod3)

#model 4 - including dp ~ vl
mod4 = psem(
  glm(dp ~ ls + ii + am + al + vl, data = na.omit(df_dip)),
  glm(rs ~ hb + db + al, data = na.omit(df_dip)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_dip), family = binomial(link = "logit")))
summary(mod4)


#model 5 - removing rs ~ db
mod5 = psem(
  glm(dp ~ ls + ii + am + al + vl, data = na.omit(df_dip)),
  glm(rs ~ hb + al, data = na.omit(df_dip)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_dip), family = binomial(link = "logit")))
summary(mod5)


#model 6 - removing vb ~ fp 
mod6 = psem(
  glm(dp ~ ls + ii + am + al + vl, data = na.omit(df_dip)),
  glm(rs ~ hb + al, data = na.omit(df_dip)),
  glm(vb ~ dp + rs + vl + al, data = na.omit(df_dip), family = binomial(link = "logit")))
summary(mod6)

#model 7 - removing vb ~ vl 
mod7 = psem(
  glm(dp ~ ls + ii + am + al + vl, data = na.omit(df_dip)),
  glm(rs ~ hb + al, data = na.omit(df_dip)),
  glm(vb ~ dp + rs + al, data = na.omit(df_dip), family = binomial(link = "logit")))
summary(mod7)


#model 8 - removing dp ~ ii
mod8 = psem(
  glm(dp ~ ls + am + al + vl, data = na.omit(df_dip)),
  glm(rs ~ hb + al, data = na.omit(df_dip)),
  glm(vb ~ dp + rs + al, data = na.omit(df_dip), family = binomial(link = "logit")))
summary(mod8)


#R² for vulnerability equation
model4 <- glm(vb ~ dp + rs + al, data = na.omit(df_dip), family = binomial(link = "logit"))
rsq(model4)

#####
# rm(list=ls())
#### Eulipotyphla   
orders[5]
df_eul<-subset(df, order=="Eulipotyphla")

df_eul[,3:13] = scale(df_eul[,3:13], center = T, scale = T)
df_eul$vb<-as.numeric(df_eul$vb)
table(is.na(df_eul$vb)) #54 spp DD #353 spp for analysis

correl = cor(na.omit(df_eul[,c("am","ls","ly","ii","rs","dp", "db", "hb", "vl", "fp", "al")], method = "spearman"))
corrplot(correl, method = 'number')

#model 1 - Conceptual model
mod1 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_eul)),
  glm(rs ~ hb + db, data = na.omit(df_eul)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_eul), family = binomial(link = "logit")))
summary(mod1) #AIC=  252.573

#model 2 - including dp ~ db
mod2 = psem(
  glm(dp ~ ls + ii + am + db, data = na.omit(df_eul)),
  glm(rs ~ hb + db + al, data = na.omit(df_eul)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_eul), family = binomial(link = "logit")))
summary(mod2)

#model 3 - including  dp ~ fp
mod3 = psem(
  glm(dp ~ ls + ii + am + db + fp, data = na.omit(df_eul)),
  glm(rs ~ hb + db + al, data = na.omit(df_eul)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_eul), family = binomial(link = "logit")))
summary(mod3)

#model 4 - including  rs ~ fp
mod4 = psem(
  glm(dp ~ ls + ii + am + db + fp, data = na.omit(df_eul)),
  glm(rs ~ hb + db + al + fp, data = na.omit(df_eul)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_eul), family = binomial(link = "logit")))
summary(mod4)

#model 5 - including  vb ~ db
mod5 = psem(
  glm(dp ~ ls + ii + am + db + fp, data = na.omit(df_eul)),
  glm(rs ~ hb + db + al + fp, data = na.omit(df_eul)),
  glm(vb ~ dp + rs + vl + al + fp + db, data = na.omit(df_eul), family = binomial(link = "logit")))
summary(mod5)

#model 6 - including  vb ~ ii
mod6 = psem(
  glm(dp ~ ls + ii + am + db + fp, data = na.omit(df_eul)),
  glm(rs ~ hb + db + al + fp, data = na.omit(df_eul)),
  glm(vb ~ dp + rs + vl + al + fp + db + ii, data = na.omit(df_eul), family = binomial(link = "logit")))
summary(mod6)


#model 7 - including  rs ~ am
mod7 = psem(
  glm(dp ~ ls + ii + am + db + fp, data = na.omit(df_eul)),
  glm(rs ~ hb + db + al + fp +am, data = na.omit(df_eul)),
  glm(vb ~ dp + rs + vl + al + fp + db + ii, data = na.omit(df_eul), family = binomial(link = "logit")))
summary(mod7)


#model 8 - including  rs ~ ls
mod8 = psem(
  glm(dp ~ ls + ii + am + db + fp, data = na.omit(df_eul)),
  glm(rs ~ hb + db + al + fp + am +ls, data = na.omit(df_eul)),
  glm(vb ~ dp + rs + vl + al + fp + db + ii, data = na.omit(df_eul), family = binomial(link = "logit")))
summary(mod8)

#model 9 - including  rs ~ vl
mod9 = psem(
  glm(dp ~ ls + ii + am + db + fp, data = na.omit(df_eul)),
  glm(rs ~ hb + db + al + fp + am + ls +vl, data = na.omit(df_eul)),
  glm(vb ~ dp + rs + vl + al + fp + db + ii, data = na.omit(df_eul), family = binomial(link = "logit")))
summary(mod9)


#model 10 - removing  vb  ~  vl 
mod10 = psem(
  glm(dp ~ ls + ii + am + db + fp, data = na.omit(df_eul)),
  glm(rs ~ hb + db + al + fp + am + ls +vl, data = na.omit(df_eul)),
  glm(vb ~ dp + rs + al + fp + db + ii, data = na.omit(df_eul), family = binomial(link = "logit")))
summary(mod10)

#model 11 - removing  vb  ~  fp
mod11 = psem(
  glm(dp ~ ls + ii + am + db + fp, data = na.omit(df_eul)),
  glm(rs ~ hb + db + al + fp + am + ls +vl, data = na.omit(df_eul)),
  glm(vb ~ dp + rs + al + db + ii, data = na.omit(df_eul), family = binomial(link = "logit")))
summary(mod11)

#model 12 - removing  vb  ~  al
mod12 = psem(
  glm(dp ~ ls + ii + am + db + fp, data = na.omit(df_eul)),
  glm(rs ~ hb + db + al + fp + am + ls +vl, data = na.omit(df_eul)),
  glm(vb ~ dp + rs + db + ii, data = na.omit(df_eul), family = binomial(link = "logit")))
summary(mod12)

#R² for vulnerability equation
model5 <- glm(vb ~ dp + rs + db + ii, data = na.omit(df_eul), family = binomial(link = "logit"))
rsq(model5)

#####
# rm(list=ls())
#### Primates   
orders[6]
df_pri<-subset(df, order=="Primates")

df_pri[,3:13] = scale(df_pri[,3:13], center = T, scale = T)
df_pri$vb<-as.numeric(df_pri$vb)
table(is.na(df_pri$vb)) #8 spp DD #392 spp for analysis

correl = cor(na.omit(df_pri[,c("am","ls","ly","ii","rs","dp", "db", "hb", "vl", "fp", "al")], method = "spearman"))
corrplot(correl, method = 'number')

#model 1 - Conceptual model
mod1 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_pri)),
  glm(rs ~ hb + db, data = na.omit(df_pri)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod1) #AIC=  345.116 

#model 2 - including  rs ~ al
mod2 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_pri)),
  glm(rs ~ hb + db + al, data = na.omit(df_pri)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod2)

#model 3 - including vb ~ ii
mod3 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_pri)),
  glm(rs ~ hb + db + al, data = na.omit(df_pri)),
  glm(vb ~ dp + rs + vl + al + fp + ii, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod3)

#model 4 - including   rs ~ fp
mod4 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_pri)),
  glm(rs ~ hb + db + al + fp, data = na.omit(df_pri)),
  glm(vb ~ dp + rs + vl + al + fp + ii, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod4)

#model 5 - including  rs ~ ls
mod5 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_pri)),
  glm(rs ~ hb + db + al + fp + ls, data = na.omit(df_pri)),
  glm(vb ~ dp + rs + vl + al + fp + ii, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod5)

#model 6 - including  dp ~ fp
mod6 = psem(
  glm(dp ~ ls + ii + am + fp, data = na.omit(df_pri)),
  glm(rs ~ hb + db + al + fp + ls, data = na.omit(df_pri)),
  glm(vb ~ dp + rs + vl + al + fp + ii, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod6)

#model 7 - including  vb ~ db
mod7 = psem(
  glm(dp ~ ls + ii + am + fp, data = na.omit(df_pri)),
  glm(rs ~ hb + db + al + fp + ls, data = na.omit(df_pri)),
  glm(vb ~ dp + rs + vl + al + fp + ii + db, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod7)

#model 8 - including  rs ~ dp
mod8 = psem(
  glm(dp ~ ls + ii + am + fp, data = na.omit(df_pri)),
  glm(rs ~ hb + db + al + fp + ls + dp, data = na.omit(df_pri)),
  glm(vb ~ dp + rs + vl + al + fp + ii + db, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod8)

#model 9 - including  vb ~ hb
mod9 = psem(
  glm(dp ~ ls + ii + am + fp, data = na.omit(df_pri)),
  glm(rs ~ hb + db + al + fp + ls + dp, data = na.omit(df_pri)),
  glm(vb ~ dp + rs + vl + al + fp + ii + db + hb, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod9)

#model 10 - removing  dp ~ ls
mod10 = psem(
  glm(dp ~ ii + am + fp, data = na.omit(df_pri)),
  glm(rs ~ hb + db + al + fp + ls + dp, data = na.omit(df_pri)),
  glm(vb ~ dp + rs + vl + al + fp + ii + db + hb, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod10)

#model 11 - removing vb ~ dp 
mod11 = psem(
  glm(dp ~ ii + am + fp, data = na.omit(df_pri)),
  glm(rs ~ hb + db + al + fp + ls + dp, data = na.omit(df_pri)),
  glm(vb ~ rs + vl + al + fp + ii + db + hb, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod11)

#model 12 - removing dp ~ ii
mod12 = psem(
  glm(dp ~ am + fp, data = na.omit(df_pri)),
  glm(rs ~ hb + db + al + fp + ls + dp, data = na.omit(df_pri)),
  glm(vb ~ rs + vl + al + fp + ii + db + hb, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod12)

#model 13 - removing vb ~ al
mod13 = psem(
  glm(dp ~ am + fp, data = na.omit(df_pri)),
  glm(rs ~ hb + db + al + fp + ls + dp, data = na.omit(df_pri)),
  glm(vb ~ rs + vl + fp + ii + db + hb, data = na.omit(df_pri), family = binomial(link = "logit")))
summary(mod13)

#R² for vulnerability equation
model6 <- glm(vb ~ rs + vl + fp + ii + db + hb, data = na.omit(df_pri), family = binomial(link = "logit"))
rsq(model6)

#####
# rm(list=ls())
#### Rodentia   
orders[7]
df_rod<-subset(df, order=="Rodentia")

df_rod[,3:13] = scale(df_rod[,3:13], center = T, scale = T)
df_rod$vb<-as.numeric(df_rod$vb)
table(is.na(df_rod$vb)) #245  spp DD #1737 spp for analysis

correl = cor(na.omit(df_rod[,c("am","ls","ly","ii","rs","dp", "db", "hb", "vl", "fp", "al")], method = "spearman"))
corrplot(correl, method = 'number')

#model 1 - Conceptual model
mod1 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_rod)),
  glm(rs ~ hb + db, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod1) #AIC= 410.164  

#model 2 - including rs ~ al
mod2 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod2)

#model 3 - including  rs ~ vl
mod3 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod3)


#model 4 - including  rs ~ dp
mod4 = psem(
  glm(dp ~ ls + ii + am, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl + dp, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod4)


#model 5 - including  dp ~ fp 
mod5 = psem(
  glm(dp ~ ls + ii + am + fp, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl + dp, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod5)


#model 6 - including rs ~ ls
mod6 = psem(
  glm(dp ~ ls + ii + am + fp, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl + dp + ls, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod6)

#model 7 - including rs ~ fp
mod7 = psem(
  glm(dp ~ ls + ii + am + fp, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod7)

#model 8 - including dp ~ db
mod8 = psem(
  glm(dp ~ ls + ii + am + fp + db, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod8)

#model 9 - including dp ~ vl
mod9 = psem(
  glm(dp ~ ls + ii + am + fp + db + vl, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod9)

#model 10 - including vb ~ ls
mod10 = psem(
  glm(dp ~ ls + ii + am + fp + db + vl, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp + ls, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod10)

#model 11 - including vb ~ hb
mod11 = psem(
  glm(dp ~ ls + ii + am + fp + db + vl, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp + ls + hb, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod11)

#model 12 - including dp ~ hb
mod12 = psem(
  glm(dp ~ ls + ii + am + fp + db + vl + hb, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp + ls + hb, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod12)

#model 13 - including dp ~ hb
mod13 = psem(
  glm(dp ~ ls + ii + am + fp + db + vl + hb, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp + ls + hb, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod13)


#model 14 - removing dp ~ ii
mod14 = psem(
  glm(dp ~ ls + am + fp + db + vl + hb, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df_rod)),
  glm(vb ~ dp + rs + vl + al + fp + ls + hb, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod14)


#model 15 - removing vb ~  dp
mod15 = psem(
  glm(dp ~ ls + am + fp + db + vl + hb, data = na.omit(df_rod)),
  glm(rs ~ hb + db + al + vl + dp + ls + fp, data = na.omit(df_rod)),
  glm(vb ~ rs + vl + al + fp + ls + hb, data = na.omit(df_rod), family = binomial(link = "logit")))
summary(mod15)

## We use the results of each model to calculate a index of potential vulnerability for each specie.

library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggridges)

data<-read.csv("data/traits_for_SEM_imputed.csv")

colnames(data)
df<-dplyr::select(data, binomial, order, adult_mass_g, mean_density_ok , litter_size_n, litters_per_year_n, interbirth_interval_d, habitat_breadth_n, det_diet_breadth_n, range_size, veg_loss, area.loss, footp, category)

df$am_l<-log(df$adult_mass_g) # adult mass log
df$am<- 1-df$am_l/(max(df$am_l, na.rm = T)) # body size index***
# hist(df$am_l)
# hist(df$am)
df$ls_l<-log(df$litter_size_n+1) # litter size log
df$ls<- 1-df$ls_l/(max(df$ls_l, na.rm = T)) # litter size index***
# hist(df$ls_l)
# hist(df$ls)
df$ly_l<-log(df$litters_per_year_n+1) # litter per year log
df$ly<- 1-df$ly_l/(max(df$ly_l, na.rm = T)) # litter per year index***
# hist(df$ly_l)
# hist(df$ly)
df$ii_l<-log(df$interbirth_interval_d) # interval inter birth (days) log
df$ii<- 1-df$ii_l/(max(df$ii_l, na.rm = T))  # interval inter birth index***
# hist(df$ii_l)
# hist(df$ii)
df$dp_l<-log(df$mean_density_ok+1) # predicted density log
df$dp<- 1-df$dp_l/(max(df$dp_l, na.rm = T)) # predicted density index***
df$rs_l<-log(df$range_size+1) # range size log
df$rs<- 1-df$rs_l/(max(df$rs_l, na.rm = T)) # range size index***
df$veg_loss<-abs(df$veg_loss)
df$vl_l<- log(df$veg_loss+1)
df$vl<- 1-df$vl_l/(max(df$vl_l, na.rm = T))
df$al_l<-log(df$area.loss+1) 
df$al<- 1-df$al_l/(max(df$al_l, na.rm = T))
df$fp_l<-log(df$footp+1)
df$fp<- 1-df$fp_l/(max(df$fp_l, na.rm = T))
df$hb<- 1-df$habitat_breadth_n/(max(df$habitat_breadth_n, na.rm = T))
df$db<- 1-df$det_diet_breadth_n/(max(df$det_diet_breadth_n , na.rm = T))
df<-dplyr::select(df, binomial, order, am, ls, ly, ii, db, hb, dp, rs, vl, fp, al, category)
head(df)


## Index of Potencial Vulnerability
est<-read.csv("estimates_for_index.csv") #table with estimates for each predictor variable. Estimates are the resulted of the SEM model for each order - Carnivora, Cetartiodactyla, Chiroptera, Diprotodontia, Eulipotyphla, Primates, Rodentia - and for all species.

#All species index
all_est<-subset(est, order=="all_species")
df$index_all<-(df$rs*all_est[1,2]+  #rs
                 df$al*all_est[2,2]+  #al
                 df$fp*all_est[3,2]+  #fp
                 df$am*all_est[4,2]+  #am
                 df$ii*all_est[5,2]+  #ii
                 df$hb*all_est[6,2]+  #hb
                 df$ls*all_est[7,2])/ #ls
  (all_est[1,2]+ 
     all_est[2,2]+
     all_est[3,2]+ 
     all_est[4,2]+ 
     all_est[5,2]+ 
     all_est[6,2]+ 
     all_est[7,2])
# hist(df$index_all)
# write.csv(df, "index_vulnerab-all_species.csv")


### Calculating an particular index for each order with more than 100 species in our data:
## Carnivora index
car_est<-subset(est, order=="Carnivora")
car_df<-subset(df, order=="Carnivora")
dim(car_df)
car_df$index<-(car_df$rs*car_est[1,2]+  #rs
                 car_df$al*car_est[2,2]+  #al
                 car_df$hb*car_est[3,2]+  #hb
                 car_df$am*car_est[4,2])/ #am
  (car_est[1,2]+
     car_est[2,2]+
     car_est[3,2]+
     car_est[4,2])
car_df<-dplyr::select(car_df, binomial, index)
# write.csv(car_df, "index_vulnerab_Carnivora.csv")


## Cetartiodactyla index
cet_est<-subset(est, order=="Cetartiodactyla")
cet_df<-subset(df, order=="Cetartiodactyla")
dim(cet_df)
cet_df$index<-(cet_df$rs*cet_est[1,2]+  #rs
                 cet_df$vl*cet_est[2,2]+  #vl
                 cet_df$al*cet_est[3,2]+  #al
                 cet_df$hb*cet_est[4,2])/ #hb
  (cet_est[1,2]+
     cet_est[2,2]+
     cet_est[3,2]+
     cet_est[4,2])
cet_df<-dplyr::select(cet_df, binomial, index)
# write.csv(cet_df, "index_vulnerab_Cetartiodactyla.csv")


## Chiroptera index
chi_est<-subset(est, order=="Chiroptera")
chi_df<-subset(df, order=="Chiroptera")
dim(chi_df)
chi_df$index<-(chi_df$dp*chi_est[1,2]+  #dp
                 chi_df$rs*chi_est[2,2]+  #rs
                 chi_df$al*chi_est[3,2]+  #al
                 chi_df$hb*chi_est[4,2])/ #hb
  (chi_est[1,2]+ 
     chi_est[2,2]+
     chi_est[3,2]+
     chi_est[4,2])
chi_df<-dplyr::select(chi_df, binomial, index)
# write.csv(chi_df, "index_vulnerab_Chiroptera.csv")

## Diprotodontia index
dip_est<-subset(est, order=="Diprotodontia")
dip_df<-subset(df, order=="Diprotodontia")
dim(dip_df)
dip_df$index<-(dip_df$dp*dip_est[1,2]+  #dp
                 dip_df$rs*dip_est[2,2]+  #rs
                 dip_df$al*dip_est[3,2])/ #al
  (dip_est[1,2]+ 
     dip_est[2,2]+
     dip_est[3,2])
dip_df<-dplyr::select(dip_df, binomial, index)
# write.csv(dip_df, "index_vulnerab_Diprotodontia.csv")


## Eulipotyphla index
eul_est<-subset(est, order=="Eulipotyphla")
eul_df<-subset(df, order=="Eulipotyphla")
dim(eul_df)
eul_df$index<-(eul_df$dp*eul_est[1,2]+  #dp
                 eul_df$rs*eul_est[2,2]+  #rs
                 eul_df$db*eul_est[3,2]+  #db
                 eul_df$ii*eul_est[4,2])/ #ii 
  (eul_est[1,2]+ 
     eul_est[2,2]+ 
     eul_est[3,2]+ 
     eul_est[4,2])
eul_df<-dplyr::select(eul_df, binomial, index)
# write.csv(eul_df, "index_vulnerab_Eulipotyphla.csv")


## Primates index
pri_est<-subset(est, order=="Primates")
pri_df<-subset(df, order=="Primates")
dim(pri_df)
pri_df$index<-(pri_df$rs*pri_est[1,2]+  #rs
                 pri_df$vl*pri_est[2,2]+  #vl
                 pri_df$fp*pri_est[3,2]+  #fp
                 pri_df$ii*pri_est[4,2]+  #ii
                 pri_df$db*pri_est[5,2]+  #db
                 pri_df$hb*pri_est[6,2])/ #hb
  (pri_est[1,2]+ 
     pri_est[2,2]+ 
     pri_est[3,2]+
     pri_est[4,2]+ 
     pri_est[5,2]+ 
     pri_est[6,2])
pri_df<-dplyr::select(pri_df, binomial, index)
# write.csv(pri_df, "index_vulnerab_Primates.csv")


## Rodentia index
rod_est<-subset(est, order=="Rodentia")
rod_df<-subset(df, order=="Rodentia")
dim(rod_df)
rod_df$index<-(rod_df$rs*rod_est[1,2]+  #rs
                 rod_df$vl*rod_est[2,2]+  #vl
                 rod_df$al*rod_est[3,2]+  #al
                 rod_df$fp*rod_est[4,2]+  #fp
                 rod_df$ls*rod_est[5,2]+  #ls
                 rod_df$hb*rod_est[6,2])/ #hb
  (rod_est[1,2]+ 
     rod_est[2,2]+ 
     rod_est[3,2]+
     rod_est[4,2]+ 
     rod_est[5,2]+ 
     rod_est[6,2])
rod_df<-dplyr::select(rod_df, binomial, index)
# write.csv(rod_df, "index_vulnerab_Rodentia.csv")


### Combining all index
folder<- "C:/Users/raque/OneDrive - ufpr.br/Área de Trabalho/2_cap_final"
filenames <- list.files(".", pattern = "index_vulnerab_")
orders_index <- Reduce(rbind, lapply(filenames, read.csv))
table(duplicated(orders_index$binomial))
dfc<- data.frame(orders_index %>% group_by(binomial) %>% filter(n() > 1))
orders_index <- orders_index %>% distinct(binomial, .keep_all = TRUE)
table(duplicated(orders_index$binomial))
orders_index<-select(orders_index, binomial, index)

all_spp_index<-read.csv("index_vulnerab-all_species.csv")
### Combining all index for each order in to one: potvuln
all_spp_index<-select(all_spp_index, binomial, index_all)
table(duplicated(all_spp_index$binomial))
dfc<- data.frame(all_spp_index %>% group_by(binomial) %>% filter(n() > 1))
all_spp_index <- all_spp_index %>% distinct(binomial, .keep_all = TRUE)
dim(all_spp_index )
potvuln<-merge(all_spp_index, orders_index, by="binomial", all = T)

#Combining the index into the same column
potvuln<- potvuln %>% 
  mutate(potvuln_index = coalesce(index, index_all)) %>% select(1:2) %>% `colnames<-`(c("binomial", "potvuln_index"))
  
### Frequency distribution plot of the index for each IUCN category of threat - Fig. 3
# install.packages("showtext")
# install.packages("BSDA")
# install.packages("ggridges")

library(BSDA)
library(showtext)
library(ggplot2)
library(ggridges)

df<-read.csv("data/traits_for_SEM_imputed.csv")
dim(df)
df<-select(df, binomial, category)

df<-merge(potvuln, df, by="binomial")
# dim(df)

LC<-subset(df, category=="LC")
NT<-subset(df, category=="NT")
VU<-subset(df, category=="VU")
EN<-subset(df, category=="EN")
CR<-subset(df, category=="CR")
DD<-subset(df, category=="DD")

#the index of potential vulnerability for order
column1 = data.frame(LC$potvuln_index)
column1$cat="LC"
column2 = data.frame(NT$potvuln_index)
column2$cat="NT"
column3 = data.frame(VU$potvuln_index)
column3$cat="VU"
column4 = data.frame(EN$potvuln_index)
column4$cat="EN"
column5 = data.frame(CR$potvuln_index)
column5$cat="CR"
column6 = data.frame(DD$potvuln_index)
column6$cat="DD"
colnames(column1)<-colnames(column2)<-colnames(column3)<-colnames(column4)<-colnames(column5)<-colnames(column6)<-c("index", "cat")


showtext_opts(dpi = 900)
showtext_auto(enable = TRUE)

dfp<-rbind(column1, column2, column3, column4, column5, column6)
colnames(dfp)<-c("index", "category")
category_colors <- c("#377EB8", "#4aaf7a", "#ffdd33","#FF7F00", "#b61516", "#8a4ea3")
category_order <- c("LC","NT","VU","EN","CR","DD")
dfp$category <- factor(dfp$category, levels = category_order)
fig4<-ggplot(dfp, aes(x = index, y = category)) +
  geom_density_ridges(aes(fill = category), rel_min_height = 0.01) +
  scale_fill_manual(values = category_colors) +
  scale_y_discrete(limits = category_order)+
  labs(x = "Potential vulnerability index", y="IUCN categories")+  theme_bw()+
  theme(axis.text.y=element_blank(),  #remove y axis labels
        axis.ticks.y=element_blank()  #remove y axis ticks
  )+
  guides(fill = FALSE) 

ggsave("figures/fig4_1.png", plot = fig4, width = 10, height = 10, units = "in", dpi = 500)
#### Z-test between each category distribution

#LC x NT 
a<-z.test(LC$potvuln_index, NT$potvuln_index, sigma.x = sd(LC$potvuln_index), sigma.y = sd(NT$potvuln_index))
#LC x VU
b<-z.test(LC$potvuln_index, VU$potvuln_index, sigma.x = sd(LC$potvuln_index), sigma.y = sd(VU$potvuln_index))
#LC x EN
c<-z.test(LC$potvuln_index, EN$potvuln_index, sigma.x = sd(LC$potvuln_index), sigma.y = sd(EN$potvuln_index))
#LC x CR
d<-z.test(LC$potvuln_index, CR$potvuln_index, sigma.x = sd(LC$potvuln_index), sigma.y = sd(CR$potvuln_index))
#LC x DD
e<-z.test(LC$potvuln_index, DD$potvuln_index, sigma.x = sd(LC$potvuln_index), sigma.y = sd(DD$potvuln_index))
#NT x VU
f<-z.test(NT$potvuln_index, VU$potvuln_index, sigma.x = sd(NT$potvuln_index), sigma.y = sd(VU$potvuln_index))
#NT x EN
g<-z.test(NT$potvuln_index, EN$potvuln_index, sigma.x = sd(NT$potvuln_index), sigma.y = sd(EN$potvuln_index))
#NT x CR
h<-z.test(NT$potvuln_index, CR$potvuln_index, sigma.x = sd(NT$potvuln_index), sigma.y = sd(CR$potvuln_index))
#NT x DD
i<-z.test(NT$potvuln_index, DD$potvuln_index, sigma.x = sd(NT$potvuln_index), sigma.y = sd(DD$potvuln_index))
#VU x EN
j<-z.test(VU$potvuln_index, EN$potvuln_index, sigma.x = sd(VU$potvuln_index), sigma.y = sd(EN$potvuln_index))
#VU x CR
k<-z.test(VU$potvuln_index, CR$potvuln_index, sigma.x = sd(VU$potvuln_index), sigma.y = sd(CR$potvuln_index))
#VU x DD
l<-z.test(VU$potvuln_index, DD$potvuln_index, sigma.x = sd(VU$potvuln_index), sigma.y = sd(DD$potvuln_index))
#EN x CR
m<-z.test(EN$potvuln_index, CR$potvuln_index, sigma.x = sd(EN$potvuln_index), sigma.y = sd(CR$potvuln_index))
#EN x DD
n<-z.test(EN$potvuln_index, DD$potvuln_index, sigma.x = sd(EN$potvuln_index), sigma.y = sd(DD$potvuln_index))
#CR x DD
o<-z.test(CR$potvuln_index, DD$potvuln_index, sigma.x = sd(CR$potvuln_index), sigma.y = sd(DD$potvuln_index))


res<-data.frame(matrix(ncol = 6, nrow = 6))
colnames(res)<-c("LC","NT","VU","EN","CR","DD")
rownames(res)<-c("LC","NT","VU","EN","CR","DD")
res[1,2]<-a$p.value #LC x NT a
res[1,3]<-b$p.value #LC x VU b
res[1,4]<-c$p.value #LC x EN c
res[1,5]<-d$p.value #LC x CR d
res[1,6]<-e$p.value #LC x DD e
res[2,3]<-f$p.value #NT x VU f
res[2,4]<-g$p.value #NT x EN g
res[2,5]<-h$p.value #NT x CR h
res[2,6]<-i$p.value #NT x DD i
res[3,4]<-j$p.value #VU x EN j
res[3,5]<-k$p.value #VU x CR k
res[3,6]<-l$p.value #VU x DD l
res[4,5]<-m$p.value #EN x CR m
res[4,6]<-n$p.value #EN x DD n
res[5,6]<-o$p.value #CR x DD o


res2<-data.frame(matrix(ncol = 6, nrow = 6))
colnames(res2)<-c("LC","NT","VU","EN","CR","DD")
rownames(res2)<-c("LC","NT","VU","EN","CR","DD")
res2[1,2]<-a$statistic #LC x NT a
res2[1,3]<-b$statistic #LC x VU b
res2[1,4]<-c$statistic #LC x EN c
res2[1,5]<-d$statistic #LC x CR d
res2[1,6]<-e$statistic #LC x DD e
res2[2,3]<-f$statistic #NT x VU f
res2[2,4]<-g$statistic #NT x EN g
res2[2,5]<-h$statistic #NT x CR h
res2[2,6]<-i$statistic #NT x DD i
res2[3,4]<-j$statistic #VU x EN j
res2[3,5]<-k$statistic #VU x CR k
res2[3,6]<-l$statistic #VU x DD l
res2[4,5]<-m$statistic #EN x CR m
res2[4,6]<-n$statistic #EN x DD n
res2[5,6]<-o$statistic #CR x DD o

a$estimate
res3<-data.frame(matrix(ncol = 6, nrow = 6))
colnames(res3)<-c("LC","NT","VU","EN","CR","DD")
rownames(res3)<-c("LC","NT","VU","EN","CR","DD")
res3[1,2]<-a$statistic #LC x NT a
res3[1,3]<-b$statistic #LC x VU b
res3[1,4]<-c$statistic #LC x EN c
res3[1,5]<-d$statistic #LC x CR d
res3[1,6]<-e$statistic #LC x DD e
res3[2,3]<-f$statistic #NT x VU f
res3[2,4]<-g$statistic #NT x EN g
res3[2,5]<-h$statistic #NT x CR h
res3[2,6]<-i$statistic #NT x DD i
res3[3,4]<-j$statistic #VU x EN j
res3[3,5]<-k$statistic #VU x CR k
res3[3,6]<-l$statistic #VU x DD l
res3[4,5]<-m$statistic #EN x CR m
res3[4,6]<-n$statistic #EN x DD n
res3[5,6]<-o$statistic #CR x DD o

# p-values between the index in all categories
# LC           NT           VU            EN            CR           DD
# LC NA 5.085922e-09 4.331690e-88 7.461904e-142 3.588280e-114 1.073375e-93
# NT NA           NA 9.954228e-17  2.434075e-36  1.053489e-50 2.358572e-10
# VU NA           NA           NA  2.447301e-07  6.639209e-21 8.627179e-04
# EN NA           NA           NA            NA  4.188598e-08 2.268408e-19
# CR NA           NA           NA            NA            NA 4.016541e-34
# DD NA           NA           NA            NA            NA           NA
# 

list_cat<-list(LC, NT, VU, EN, CR, DD)
tab_sup<-data.frame(matrix(ncol = 5, nrow = 6))
colnames(tab_sup)<-c("mean","median","sd","var", "Nspp")
rownames(tab_sup)<-c("LC","NT","VU","EN","CR","DD")

for(i in 1:length(list_cat)){
  d<-data.frame(list_cat[i])
  tab_sup[i,1]<-mean(d$potvuln_index)
  tab_sup[i,2]<-median(d$potvuln_index)
  tab_sup[i,3]<-sd(d$potvuln_index)
  tab_sup[i,4]<-var(d$potvuln_index)
  tab_sup[i,5]<-length(d$potvuln_index)
}
# write.csv(tab_sup, "Stat_index_by_categor2.csv")


