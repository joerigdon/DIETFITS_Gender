##Load packages and code
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Tables.R")
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Figures.R")
source("/Users/jrigdon/Box sync/Rigdon/Useful Functions/Functions.R")
library(tidyr)

##Load data
g1 = read.csv("/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/gender_data1.csv" , header=TRUE)[, -1] #demographics

g2 = read.csv("/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/gender_data2.csv" , header=TRUE)[, -1]
g3 = g2[g2$redcap_event_name!="screening_arm_1", ]

s1 = read.csv("/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/DATA_2018-02-09.csv", header=TRUE)
s2 = s1[s1$redcap_event_name!="screening_arm_1", names(s1) %in% c("study_id", "redcap_event_name", "waist_gcrc", "height_gcrc_corrected")]

##Merge to get waist and height
g3$unique = paste(g3$record_id, g3$redcap_event_name, sep="_")
s2$unique = paste(s2$study_id, s2$redcap_event_name, sep="_")
g4 = merge(g3, s2[, -c(1:2)], by="unique", all.x=TRUE)

g4$fatmass = g4$weight_gcrc*(g4$dxa_percentfat/100)
g4$leanmass = g4$weight_gcrc-g4$fatmass

##Merge in additional variables
z1 = read.csv("/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/DATA_2018-03-22.csv", header=TRUE)
z2 = z1[z1$redcap_event_name=="screening_arm_1", names(z1) %in% c("record_id", "scr_gender",	"age_consent", "scr_racial", "scr_racial_other", "scr_ethnic")]
z3 = z1[z1$redcap_event_name=="baseline_arm_1", names(z1) %in% c("record_id", "bmi",	"bp_sys", "bp_diast", "met_syn")]

z4 = merge(z2, z3, by="record_id")

g5 = merge(g4, z4, by="record_id", all.x=TRUE)

##Define changes data set
wt = g4[, names(g4) %in% c("record_id", "redcap_event_name", "weight_gcrc")]
wt2 = spread(data=wt, redcap_event_name, weight_gcrc)
names(wt2)[2:5] = c("w12", "w3", "w6", "w0")
wt2$wC3 = wt2$w3-wt2$w0
wt2$wC6 = wt2$w6-wt2$w0
wt2$wC12 = wt2$w12-wt2$w0

#Fat
f = g4[, names(g4) %in% c("record_id", "redcap_event_name", "fatmass")]
f2 = spread(data=f, redcap_event_name, fatmass)
names(f2)[2:5] = c("f12", "f3", "f6", "f0")
f2 = f2[, -3]
f2$fC6 = f2$f6-f2$f0
f2$fC12 = f2$f12-f2$f0

#Lean mass
l = g4[, names(g4) %in% c("record_id", "redcap_event_name", "leanmass")]
l2 = spread(data=l, redcap_event_name, leanmass)
names(l2)[2:5] = c("l12", "l3", "l6", "l0")
l2 = l2[, -3]
l2$lC6 = l2$l6-l2$l0
l2$lC12 = l2$l12-l2$l0

#Merge
all1 = merge(wt2, f2, by="record_id", all.x=TRUE)
all2 = merge(all1, l2, by="record_id", all.x=TRUE)
all3 = all2[, names(all2) %in% c("record_id", "wC3", "wC6", "wC12", "fC6", "fC12", "lC6", "lC12")]

h1 = merge(g1, all2, by="record_id", all.x=TRUE)
table(h1$scr_gender, exclude=NULL)
table(h1$diet_type, exclude=NULL)

##Final merge
g6 = merge(g5, h1[, names(h1) %in% c("record_id", "diet_type")], by="record_id", all.x=TRUE)

##Table 1. Baseline Demographics and Anthropometric Variables by Sex/Gender and Diet (Age, Weight, BMI, % Body Fat, Body Fat, Lean Body Mass)
names(g6)

g6$genDiet = NA
g6$genDiet[g6$scr_gender==2 & g6$diet_type=="lowfat"] = "a.LFW"
g6$genDiet[g6$scr_gender==2 & g6$diet_type=="lowcarb"] = "b.LCW"
g6$genDiet[g6$scr_gender==1 & g6$diet_type=="lowfat"] = "c.LFM"
g6$genDiet[g6$scr_gender==1 & g6$diet_type=="lowcarb"] = "d.LCM"

bl = g6[g6$redcap_event_name=="baseline_arm_1", ]
table(bl$genDiet, exclude=NULL)

tab1 = mktab(data=bl, var.names=c("age_consent", "weight_gcrc", "bmi", "dxa_percentfat", "fatmass", "leanmass"), ind.cat=rep(0, 6), group.name="genDiet", cfn=describeMean, miss="always", pval=FALSE, tot=FALSE, digit=1)


##Table 2: Macronutrient Intake by Sex/Gender, Diet and Time Point (3, 6, and 12 months) [Calories (Kcal), Carbs (g), Fat (g), and Protein (g)]
bl = g6[g6$redcap_event_name=="baseline_arm_1", ]
m3 = g6[g6$redcap_event_name=="3_months_arm_1", ]
m6 = g6[g6$redcap_event_name=="6_months_arm_1", ]
m12 = g6[g6$redcap_event_name=="12_months_arm_1", ]

j0 = mktab(data=bl, var.names=c("energy", "total_carb", "total_gfat", "total_protein"), ind.cat=rep(0, 4), group.name="genDiet", cfn=describeSEM, miss="no", pval=FALSE, tot=FALSE, digit=1)
j3 = mktab(data=m3, var.names=c("energy", "total_carb", "total_gfat", "total_protein"), ind.cat=rep(0, 4), group.name="genDiet", cfn=describeSEM, miss="no", pval=FALSE, tot=FALSE, digit=1)
j6 = mktab(data=m6, var.names=c("energy", "total_carb", "total_gfat", "total_protein"), ind.cat=rep(0, 4), group.name="genDiet", cfn=describeSEM, miss="no", pval=FALSE, tot=FALSE, digit=1)
j12 = mktab(data=m12, var.names=c("energy", "total_carb", "total_gfat", "total_protein"), ind.cat=rep(0, 4), group.name="genDiet", cfn=describeSEM, miss="no", pval=FALSE, tot=FALSE, digit=1)

tab2N = rbind(j0[1:2, ], j3[2, ], j6[2, ], j12[2, ])

for (i in 3:5) {
jj = rbind(j0[i, ], j3[i, ], j6[i, ], j12[i, ])
tab2N = rbind(tab2N, jj)
}

rownames(tab2N)[2] = "Total calories"
rownames(tab2N)[6] = "Total carb (g)"
rownames(tab2N)[10] = "Total fat (g)"
rownames(tab2N)[14] = "Total protein (g)"


##Save all tables
word.doc(obj.list=list(tab1, tab2), obj.title=c("Table 1: Baseline Demographics and Anthropometric Variables by Sex/Gender and Diet (Age, Weight, BMI, % Body Fat, Body Fat, Lean Body Mass)", "Table 2: Macronutrient Intake by Sex/Gender, Diet and Time Point (3, 6, and 12 months) [Calories (Kcal), Carbs (g), Fat (g), and Protein (g)]"), dest="/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/Tables_2018-03-22.docx", ftype="Arial", col.odd="white")


##Appendix Table 1: Sample sizes for weight and fat mass outcomes
j0 = mktab(data=bl, var.names=c("weight_gcrc", "dxa_percentfat"), ind.cat=rep(0, 2), group.name="genDiet", cfn=describeN, miss="no", pval=FALSE, tot=FALSE, digit=1)
j3 = mktab(data=m3, var.names=c("weight_gcrc", "dxa_percentfat"), ind.cat=rep(0, 2), group.name="genDiet", cfn=describeN, miss="no", pval=FALSE, tot=FALSE, digit=1)
j6 = mktab(data=m6, var.names=c("weight_gcrc", "dxa_percentfat"), ind.cat=rep(0, 2), group.name="genDiet", cfn=describeN, miss="no", pval=FALSE, tot=FALSE, digit=1)
j12 = mktab(data=m12, var.names=c("weight_gcrc", "dxa_percentfat"), ind.cat=rep(0, 2), group.name="genDiet", cfn=describeN, miss="no", pval=FALSE, tot=FALSE, digit=1)

tab3N = rbind(j0[1:2, ], j3[2, ], j6[2, ], j12[2, ])

for (i in 3:3) {
jj = rbind(j0[i, ], j3[i, ], j6[i, ], j12[i, ])
tab3N = rbind(tab3N, jj)
}

rownames(tab3N)[2] = "Weight (kg)"
rownames(tab3N)[6] = "Body fat (%)"

##Appendix Table 2: Sapmle sizes for macronutrient data
bl = g6[g6$redcap_event_name=="baseline_arm_1", ]
m3 = g6[g6$redcap_event_name=="3_months_arm_1", ]
m6 = g6[g6$redcap_event_name=="6_months_arm_1", ]
m12 = g6[g6$redcap_event_name=="12_months_arm_1", ]

j0 = mktab(data=bl, var.names=c("energy", "total_carb", "total_gfat", "total_protein"), ind.cat=rep(0, 4), group.name="genDiet", cfn=describeN, miss="no", pval=FALSE, tot=FALSE, digit=1)
j3 = mktab(data=m3, var.names=c("energy", "total_carb", "total_gfat", "total_protein"), ind.cat=rep(0, 4), group.name="genDiet", cfn=describeN, miss="no", pval=FALSE, tot=FALSE, digit=1)
j6 = mktab(data=m6, var.names=c("energy", "total_carb", "total_gfat", "total_protein"), ind.cat=rep(0, 4), group.name="genDiet", cfn=describeN, miss="no", pval=FALSE, tot=FALSE, digit=1)
j12 = mktab(data=m12, var.names=c("energy", "total_carb", "total_gfat", "total_protein"), ind.cat=rep(0, 4), group.name="genDiet", cfn=describeN, miss="no", pval=FALSE, tot=FALSE, digit=1)

tab2N = rbind(j0[1:2, ], j3[2, ], j6[2, ], j12[2, ])

for (i in 3:5) {
jj = rbind(j0[i, ], j3[i, ], j6[i, ], j12[i, ])
tab2N = rbind(tab2N, jj)
}

rownames(tab2N)[2] = "Total calories"
rownames(tab2N)[6] = "Total carb (g)"
rownames(tab2N)[10] = "Total fat (g)"
rownames(tab2N)[14] = "Total protein (g)"


word.doc(obj.list=list(tab3N), obj.title=c("Table 2: Sample sizes of weight and body fat"), dest="/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/Table3N_2018-04-10.docx", ftype="Arial", col.odd="white")


##Figure 1 A-C: Box plots of 12-months changes (Kg) in weight  (A), body fat (B), and lean mass (C) by sex/gender and diet
##Weight

##Table of 12 month fat/12 month lean changes by sex/diet
h1$FL = h1$fC12/h1$lC12
h1$sex = ifelse(h1$scr_gender==2, "a.Female", "b.Male")
table(h1$sex, h1$scr_gender, exclude=NULL)

h1$diet = ifelse(h1$diet_type=="lowfat", "a.LowFat", "b.LowCarb")
table(h1$diet, h1$diet_type, exclude=NULL)

h1$sexdiet = paste(h1$sex, h1$diet, sep="_")
table(h1$sexdiet, exclude=NULL)

tabSUPP = mktab(data=h1, var.names=c("wC12", "fC12", "lC12", "FL"), ind.cat=rep(0, 4), group.name="sexdiet", cfn=describeMedian, miss="always", pval=FALSE, tot=FALSE, digit=1)

word.doc(obj.list=list(tabSUPP), obj.title=c("Table 4: 12 month changes by sex and diet"), dest="/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/Table_2018-04-10.docx", ftype="Arial", col.odd="white")


##Figure 1: Box-plots of diet adherence* by sex/gender and diet
##* Averages of 3/6/12 month of Z scores of DS/ baseline weight (Kg) by diet.
ca = g3[, names(g3) %in% c("record_id", "redcap_event_name", "total_carb")]
ca2 = spread(data=ca, redcap_event_name, total_carb)
names(ca2)[2:5] = c("ca12", "ca3", "ca6", "ca0")

fa = g3[, names(g3) %in% c("record_id", "redcap_event_name", "total_gfat")]
fa2 = spread(data=fa, redcap_event_name, total_gfat)
names(fa2)[2:5] = c("fa12", "fa3", "fa6", "fa0")

i1 = merge(h1, ca2, by="record_id", all.x=TRUE)
i2 = merge(i1, fa2, by="record_id", all.x=TRUE)

##Calculate Z-score at 3, 6, and 12; then average
##Z-score at 3 months
i2$DS_ca3 = (i2$ca3-20)/i2$w0
i2$DS_fa3 = (i2$fa3-20)/i2$w0

i2$Z3 = NA
i2$Z3[i2$diet_type=="lowcarb"] = (i2$DS_ca3[i2$diet_type=="lowcarb"]-mean(i2$DS_ca3[i2$diet_type=="lowcarb"], na.rm=TRUE)) / sd(i2$DS_ca3[i2$diet_type=="lowcarb"], na.rm=TRUE)

i2$Z3[i2$diet_type=="lowfat"] = (i2$DS_fa3[i2$diet_type=="lowfat"]-mean(i2$DS_fa3[i2$diet_type=="lowfat"], na.rm=TRUE)) / sd(i2$DS_fa3[i2$diet_type=="lowfat"], na.rm=TRUE)

summary(i2$Z3[i2$diet_type=="lowcarb"])
summary(i2$Z3[i2$diet_type=="lowfat"])

##Z-score at 6 months
i2$DS_ca6 = (i2$ca6-20)/i2$w0
i2$DS_fa6 = (i2$fa6-20)/i2$w0

i2$Z6 = NA
i2$Z6[i2$diet_type=="lowcarb"] = (i2$DS_ca6[i2$diet_type=="lowcarb"]-mean(i2$DS_ca6[i2$diet_type=="lowcarb"], na.rm=TRUE)) / sd(i2$DS_ca6[i2$diet_type=="lowcarb"], na.rm=TRUE)

i2$Z6[i2$diet_type=="lowfat"] = (i2$DS_fa6[i2$diet_type=="lowfat"]-mean(i2$DS_fa6[i2$diet_type=="lowfat"], na.rm=TRUE)) / sd(i2$DS_fa6[i2$diet_type=="lowfat"], na.rm=TRUE)

summary(i2$Z6[i2$diet_type=="lowcarb"])
summary(i2$Z6[i2$diet_type=="lowfat"])

##Z-score at 12 months
i2$DS_ca12 = (i2$ca12-20)/i2$w0
i2$DS_fa12 = (i2$fa12-20)/i2$w0

i2$Z12 = NA
i2$Z12[i2$diet_type=="lowcarb"] = (i2$DS_ca12[i2$diet_type=="lowcarb"]-mean(i2$DS_ca12[i2$diet_type=="lowcarb"], na.rm=TRUE)) / sd(i2$DS_ca12[i2$diet_type=="lowcarb"], na.rm=TRUE)

i2$Z12[i2$diet_type=="lowfat"] = (i2$DS_fa12[i2$diet_type=="lowfat"]-mean(i2$DS_fa12[i2$diet_type=="lowfat"], na.rm=TRUE)) / sd(i2$DS_fa12[i2$diet_type=="lowfat"], na.rm=TRUE)

summary(i2$Z12[i2$diet_type=="lowcarb"])
summary(i2$Z12[i2$diet_type=="lowfat"])

##Deviation scores
tabD1 = mktab(data=i2, var.names=c("DS_fa3", "DS_ca3", "DS_fa6", "DS_ca6", "DS_fa12", "DS_ca12"), ind.cat=rep(0, 6), group.name="diet", cfn=describeMean, miss="always", pval=FALSE, tot=FALSE, digit=1)

tabD2 = mktab(data=i2, var.names=c("DS_fa3", "DS_ca3", "DS_fa6", "DS_ca6", "DS_fa12", "DS_ca12"), ind.cat=rep(0, 6), group.name="sexdiet", cfn=describeMean, miss="always", pval=FALSE, tot=FALSE, digit=1)

word.doc(obj.list=list(tabD1, tabD2), obj.title=c("Table S5: Deviation scores by diet", "Table S6: Deviation scores by sex and diet"), dest="/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/TableS5_2018-04-18.docx", ftype="Arial", col.odd="white")

##Defining adherence
##Z-score average
i2$Z = NA
i2$Z[i2$diet_type=="lowcarb"] = apply(i2[i2$diet_type=="lowcarb", names(i2) %in% c("Z3", "Z6", "Z12")], 1, function(x) mean(x, na.rm=TRUE))
i2$Z[i2$diet_type=="lowfat"] = apply(i2[i2$diet_type=="lowfat", names(i2) %in% c("Z3", "Z6", "Z12")], 1, function(x) mean(x, na.rm=TRUE))

summary(i2$Z[i2$diet_type=="lowcarb"])
summary(i2$Z[i2$diet_type=="lowfat"])

##lean mass and changes in body fat is called Kf
summary(i2$lC12)
summary(i2$fC12)
i2$Kf = i2$lC12/i2$fC12
summary(i2$Kf)

zLFW = i2$Z[i2$diet_type=="lowfat" & i2$scr_gender==2]
zLFM = i2$Z[i2$diet_type=="lowfat" & i2$scr_gender==1]
zLCW = i2$Z[i2$diet_type=="lowcarb" & i2$scr_gender==2]
zLCM = i2$Z[i2$diet_type=="lowcarb" & i2$scr_gender==1]

pdf("/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/Fig2_2018-03-29.pdf")
plot(x = c(0, 2.5), y=c(-2, 6), type="n", axes=F, xlab="", ylab="")
abline(h=4, col="grey", lwd=0.5)
abline(h=2, col="grey", lwd=0.5)
abline(h=0, lwd=1.5)
#abline(h=-2, col="grey", lwd=0.5)
#abline(h=-20, col="grey", lwd=0.5)
#abline(h=-30, col="grey", lwd=0.5)
boxp(obj.list=list(zLFW, zLCW, zLFM, zLCM), pos=c(0.5, 1, 1.5, 2), cols=c('violetred4', 'violetred1', 'steelblue4', 'steelblue1'), atx=c(0.5, 1, 1.5, 2), labs=c('Women, LF', 'Women, LC', 'Men, LF', 'Men, LC'), xtitle='Gender and Diet', ytitle='Z-score for deviation (lower is better)', mtitle='', ylim=c(-2.2, 5), add.n=FALSE, rotx=NA, xcex=1, ad=TRUE)
legend('topleft', fill=c('violetred4', 'violetred1', 'steelblue4', 'steelblue1'), legend=c('Women, LF', 'Women, LC', 'Men, LF', 'Men, LC'), bty='n', cex=1, inset=0.02)
dev.off()


##Figure 2: Box plots of 12-months changes in weight, body fat, and lean mass (Kg) sex/gender and diet (Kg)
w12_LFw = h1$wC12[h1$scr_gender==2 & h1$diet_type=="lowfat"]
w12_LCw = h1$wC12[h1$scr_gender==2 & h1$diet_type=="lowcarb"]
m12_LFw = h1$wC12[h1$scr_gender==1 & h1$diet_type=="lowfat"]
m12_LCw = h1$wC12[h1$scr_gender==1 & h1$diet_type=="lowcarb"]

w12_LFf = h1$fC12[h1$scr_gender==2 & h1$diet_type=="lowfat"]
w12_LCf = h1$fC12[h1$scr_gender==2 & h1$diet_type=="lowcarb"]
m12_LFf = h1$fC12[h1$scr_gender==1 & h1$diet_type=="lowfat"]
m12_LCf = h1$fC12[h1$scr_gender==1 & h1$diet_type=="lowcarb"]

w12_LFl = h1$lC12[h1$scr_gender==2 & h1$diet_type=="lowfat"]
w12_LCl = h1$lC12[h1$scr_gender==2 & h1$diet_type=="lowcarb"]
m12_LFl = h1$lC12[h1$scr_gender==1 & h1$diet_type=="lowfat"]
m12_LCl = h1$lC12[h1$scr_gender==1 & h1$diet_type=="lowcarb"]

pdf("/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/Fig1_2018-03-22.pdf")
par(mfrow=c(1,1))
plot(x = c(0, 7.5), y=c(-34, 20), type="n", axes=F, xlab="", ylab="")
abline(h=20, col="grey", lwd=0.5)
abline(h=10, col="grey", lwd=0.5)
abline(h=0, lwd=1.5)
abline(h=-10, col="grey", lwd=0.5)
abline(h=-20, col="grey", lwd=0.5)
abline(h=-30, col="grey", lwd=0.5)
boxp(obj.list=list(w12_LFw, w12_LCw, m12_LFw, m12_LCw, w12_LFf, w12_LCf, m12_LFf, m12_LCf, w12_LFl, w12_LCl, m12_LFl, m12_LCl), pos=c(0.5, 1, 1.5, 2, 3, 3.5, 4, 4.5, 5.5, 6, 6.5, 7), cols=rep(c('violetred4', 'violetred1', 'steelblue4', 'steelblue1'), 3), atx=c(1.25, 3.75, 6.25), labs=c('Weight', 'Fat mass', 'Lean mass'), xtitle='Measure', ytitle='12-month change (kg)', mtitle='', ylim=c(-34,20), add.n=FALSE, rotx=NA, xcex=1, ad=TRUE)
legend('topright', fill=c('violetred4', 'violetred1', 'steelblue4', 'steelblue1'), legend=c('Women, LF', 'Women, LC', 'Men, LF', 'Men, LC'), bty='n', inset=0.02)
dev.off()


##Appendix Figure 1: Diet adherence and 12 month weight change
i2$pW = 100*(i2$wC12/i2$w0)
i2$pF = 100*(i2$fC12/i2$f0)
i2$pL = 100*(i2$lC12/i2$l0)

LFW = i2[i2$diet_type=="lowfat" & i2$scr_gender==2, ]
LFM = i2[i2$diet_type=="lowfat" & i2$scr_gender==1, ]

LCW = i2[i2$diet_type=="lowcarb" & i2$scr_gender==2, ]
LCM = i2[i2$diet_type=="lowcarb" & i2$scr_gender==1, ]

pdf("/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/Fig3A_2018-03-29_supp.pdf")
par(mfrow=c(2,2))
scplot(dta=LFW, var.x="Z", var.y="wC12", clr="violetred4", xinf=1.1, yinf=1.1, xtitle="Z-score for deviation (lower is better)", ytitle="12-month weight change (kg)", mtitle="Women, LF", add.cor=TRUE, lpos="bottomleft", lcex=0.8)

scplot(dta=LCW, var.x="Z", var.y="wC12", clr="violetred1", xinf=1.1, yinf=1.1, xtitle="Z-score for deviation (lower is better)", ytitle="12-month weight change (kg)", mtitle="Women, LC", add.cor=TRUE, lpos="bottomright", lcex=0.8)

scplot(dta=LFM, var.x="Z", var.y="wC12", clr="steelblue4", xinf=1.1, yinf=1.1, xtitle="Z-score for deviation (lower is better)", ytitle="12-month weight change (kg)", mtitle="Men, LF", add.cor=TRUE, lpos="bottomright", lcex=0.8)

scplot(dta=LCM, var.x="Z", var.y="wC12", clr="steelblue1", xinf=1.1, yinf=1.1, xtitle="Z-score for deviation (lower is better)", ytitle="12-month weight change (kg)", mtitle="Men, LC", add.cor=TRUE, lpos="bottomright", lcex=0.8)
dev.off()

##Appendix Figure 2: Diet adherence and 12 month fat mass change
pdf("/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/Fig3B_2018-03-29_supp.pdf")
par(mfrow=c(2,2))
scplot(dta=LFW, var.x="Z", var.y="fC12", clr="violetred4", xinf=1.1, yinf=1.1, xtitle="Z-score for deviation (lower is better)", ytitle="12-month fat change (kg)", mtitle="Women, LF", add.cor=TRUE, lpos="bottomleft", lcex=0.8)

scplot(dta=LCW, var.x="Z", var.y="fC12", clr="violetred1", xinf=1.1, yinf=1.1, xtitle="Z-score for deviation (lower is better)", ytitle="12-month fat change (kg)", mtitle="Women, LC", add.cor=TRUE, lpos="bottomright", lcex=0.8)

scplot(dta=LFM, var.x="Z", var.y="fC12", clr="steelblue4", xinf=1.1, yinf=1.1, xtitle="Z-score for deviation (lower is better)", ytitle="12-month fat change (kg)", mtitle="Men, LF", add.cor=TRUE, lpos="bottomright", lcex=0.8)

scplot(dta=LCM, var.x="Z", var.y="fC12", clr="steelblue1", xinf=1.1, yinf=1.1, xtitle="Z-score for deviation (lower is better)", ytitle="12-month fat change (kg)", mtitle="Men, LC", add.cor=TRUE, lpos="bottomright", lcex=0.8)
dev.off()

##Appendix Figure 3: Diet adherence and 12 month lean mass change
pdf("/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/Fig3C_2018-03-29_supp.pdf")
par(mfrow=c(2,2))
scplot(dta=LFW, var.x="Z", var.y="lC12", clr="violetred4", xinf=1.1, yinf=1.1, xtitle="Z-score for deviation (lower is better)", ytitle="12-month lean change (kg)", mtitle="Women, LF", add.cor=TRUE, lpos="bottomleft", lcex=0.8)

scplot(dta=LCW, var.x="Z", var.y="lC12", clr="violetred1", xinf=1.1, yinf=1.1, xtitle="Z-score for deviation (lower is better)", ytitle="12-month lean change (kg)", mtitle="Women, LC", add.cor=TRUE, lpos="bottomright", lcex=0.8)

scplot(dta=LFM, var.x="Z", var.y="lC12", clr="steelblue4", xinf=1.1, yinf=1.1, xtitle="Z-score for deviation (lower is better)", ytitle="12-month lean change (kg)", mtitle="Men, LF", add.cor=TRUE, lpos="bottomright", lcex=0.8)

scplot(dta=LCM, var.x="Z", var.y="lC12", clr="steelblue1", xinf=1.1, yinf=1.1, xtitle="Z-score for deviation (lower is better)", ytitle="12-month lean change (kg)", mtitle="Men, LC", add.cor=TRUE, lpos="bottomright", lcex=0.8)
dev.off()


######################
##Statistical models##
######################
library(lme4)
library(lmerTest)
library(multcomp)

##Hypothesis 1: 12-month diet adherence is different by sex/gender, and diet
i2$sex = ifelse(i2$scr_gender==2, "a.Female", "b.Male")
table(i2$sex, i2$scr_gender, exclude=NULL)

i2$diet = ifelse(i2$diet_type=="lowfat", "a.LowFat", "b.LowCarb")
table(i2$diet, i2$diet_type, exclude=NULL)

mod2 = lm(Z ~ sex*diet, data=i2)
hist(resid(mod2))
summary(resid(mod2)) #check residuals

##Comparisons we want to make (b0 int, b1 male, b2 LC, b3 male+LC)
cm1 = rbind("W,LF" =  c(1, 0, 0, 0),
    "W,LC" = c(1, 0, 1, 0),
    "M,LF" = c(1, 1, 0, 0),
    "M,LC" = c(1, 1, 1, 1),
    "W,LCmLF" = c(0, 0, 1, 0),
    "M,LCmLF" = c(0, 0, 1, 1),
    "LF,MmW" = c(0, 1, 0, 0),
    "LC,MmW" = c(0, 1, 0, 1))

a = summary(glht(mod2, cm1), test = adjusted("none")) #unadjusted p-values
b = confint(glht(mod2, cm1), calpha = univariate_calpha())$confint

tab2 = cbind(apply(round(b, 2), 1, function(x) paste(paste(paste(paste(x[1], "(", sep=" "), x[2], sep=""), x[3], sep=", "), ")", sep="")), round(a$test$pvalues, 2))
colnames(tab2) = c("Est (95% CI)", "P-value")

##Estimates recorded in Figure 1
word.doc(obj.list=list(tab2), obj.title=c("Table 2: Adherence by sex and diet"), dest="/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/Table2_2018-03-29.docx", ftype="Arial", col.odd="white")


##Hypothesis 2: 12-month weight change, fat mass change, lean mass change (Kg) is different by sex and diet
##Which model to use?
g5 = merge(g4, h1[, names(h1) %in% c("record_id", "diet_type", "scr_gender")], by="record_id", all.x=TRUE)
g5$sex = ifelse(g5$scr_gender==2, "a.Female", "b.Male")
table(g5$sex, g5$scr_gender, exclude=NULL)

g5$diet = ifelse(g5$diet_type=="lowfat", "a.LowFat", "b.LowCarb")
table(g5$diet, g5$diet_type, exclude=NULL)

g5$time = NA
g5$time[g5$redcap_event_name=="baseline_arm_1"] = "a.BL"
g5$time[g5$redcap_event_name=="3_months_arm_1"] = "b.3m"
g5$time[g5$redcap_event_name=="6_months_arm_1"] = "c.6m"
g5$time[g5$redcap_event_name=="12_months_arm_1"] = "d.12m"
table(g5$time, g5$redcap_event_name, exclude=NULL)

##Comparisons of interest
cm0 = rbind("W,LF" =  c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    "W,LC" = c(1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    "M,LF" = c(1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    "M,LC" = c(1, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0))

cm12 = rbind("W,LF" =  c(1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    "W,LC" = c(1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
    "M,LF" = c(1, 0, 0, 1, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
    "M,LC" = c(1, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1))

cm12_0 = rbind("W,LF" =  c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    "W,LC" = c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
    "M,LF" = c(0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
    "M,LC" = c(0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1))


cm = rbind("W,LCmLF" =  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0),
    "M,LCmLF" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1),
    "LF,MmW" = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0),
    "LC,MmW" = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1))

##Weight
mW = lmer(weight_gcrc ~ time*sex*diet + (1|record_id), data=g5)
anova(mW)

wP = summary(glht(mW, rbind(cm12_0, cm)), test = adjusted("none")) #unadjusted p-values
wC = confint(glht(mW, rbind(cm12_0, cm)), calpha = univariate_calpha())$confint

tab1a = cbind(apply(round(wC, 2), 1, function(x) paste(paste(paste(paste(x[1], "(", sep=" "), x[2], sep=""), x[3], sep=", "), ")", sep="")), round(wP$test$pvalues, 2))
colnames(tab1a) = c("Est (95% CI)", "P-value")

##Fat
mF = lmer(fatmass ~ time*sex*diet + (1|record_id), data=g5)
anova(mF)

Cm12_0 = rbind("W,LF" =  c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    "W,LC" = c(0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0),
    "M,LF" = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0),
    "M,LC" = c(0, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 1))


Cm = rbind("W,LCmLF" =  c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
    "M,LCmLF" = c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1),
    "LF,MmW" = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0),
    "LC,MmW" = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1))

fP = summary(glht(mF, rbind(Cm12_0, Cm)), test = adjusted("none")) #unadjusted p-values
fC = confint(glht(mF, rbind(Cm12_0, Cm)), calpha = univariate_calpha())$confint

tab1b = cbind(apply(round(fC, 2), 1, function(x) paste(paste(paste(paste(x[1], "(", sep=" "), x[2], sep=""), x[3], sep=", "), ")", sep="")), round(fP$test$pvalues, 2))
colnames(tab1b) = c("Est (95% CI)", "P-value")

##Lean
mL = lmer(leanmass ~ time*sex*diet + (1|record_id), data=g5)
anova(mL)

lP = summary(glht(mL, rbind(Cm12_0, Cm)), test = adjusted("none")) #unadjusted p-values
lC = confint(glht(mL, rbind(Cm12_0, Cm)), calpha = univariate_calpha())$confint

tab1c = cbind(apply(round(lC, 2), 1, function(x) paste(paste(paste(paste(x[1], "(", sep=" "), x[2], sep=""), x[3], sep=", "), ")", sep="")), round(lP$test$pvalues, 2))
colnames(tab1c) = c("Est (95% CI)", "P-value")

##Estimates from tab1a, tab1b, and tab1c recorded in Figure 2

##Fat/Lean ratio
i2$ratio = i2$fC12 / i2$lC12
summary(i2$ratio)
i2$logratio = log(i2$ratio)
hist(i2$logratio)

i2$sex = ifelse(i2$scr_gender==2, "a.Female", "b.Male")
table(i2$sex, i2$scr_gender, exclude=NULL)

i2$diet = ifelse(i2$diet_type=="lowfat", "a.LowFat", "b.LowCarb")
table(i2$diet, i2$diet_type, exclude=NULL)

mod22 = lm(logratio ~ factor(sexdiet), data=i2)
summary(mod22)
anova(mod22)
hist(resid(mod22))
summary(resid(mod22)) #check residuals

#confint(summary(glht(mod2, cm1), test=adjusted("Westfall")))$confint #Westfall test


##Hypothesis 3: 12-month diet adherence is differentially associated with for 12-month changes in weight, body fat, and lean mass (kg) by sex/gender and diet.
m3f = lm(fC12 ~ Z*sex*diet, data=i2)
m3l = lm(lC12 ~ Z*sex*diet, data=i2)

cm3 = rbind("W,LF" =  -c(0, 1, 0, 0, 0, 0, 0, 0),
    "W,LC" = -c(0, 1, 0, 0, 0, 1, 0, 0),
    "M,LF" = -c(0, 1, 0, 0, 1, 0, 0, 0),
    "M,LC" = -c(0, 1, 0, 0, 1, 1, 0, 1),
    "W,LCmLF" = -c(0, 0, 0, 0, 0, 1, 0, 0),
    "M,LCmLF" = -c(0, 0, 0, 0, 0, 1, 0, 1),
    "LF,MmW" = -c(0, 0, 0, 0, 1, 0, 0, 0),
    "LC,MmW" = -c(0, 0, 0, 0, 1, 0, 0, 1))

##Weight
m3w = lm(wC12 ~ Z*sex*diet, data=i2)

w3P = summary(glht(m3w, cm3), test = adjusted("none")) #unadjusted p-values
w3C = confint(glht(m3w, cm3), calpha = univariate_calpha())$confint

tab3a = cbind(apply(round(w3C, 2), 1, function(x) paste(paste(paste(paste(x[1], "(", sep=" "), x[2], sep=""), x[3], sep=", "), ")", sep="")), round(w3P$test$pvalues, 2))
colnames(tab3a) = c("Est (95% CI)", "P-value")

##Fat
m3f = lm(fC12 ~ Z*sex*diet, data=i2)

f3P = summary(glht(m3f, cm3), test = adjusted("none")) #unadjusted p-values
f3C = confint(glht(m3f, cm3), calpha = univariate_calpha())$confint

tab3b = cbind(apply(round(f3C, 2), 1, function(x) paste(paste(paste(paste(x[1], "(", sep=" "), x[2], sep=""), x[3], sep=", "), ")", sep="")), round(f3P$test$pvalues, 2))
colnames(tab3b) = c("Est (95% CI)", "P-value")

##Lean
m3l = lm(lC12 ~ Z*sex*diet, data=i2)

l3P = summary(glht(m3l, cm3), test = adjusted("none")) #unadjusted p-values
l3C = confint(glht(m3l, cm3), calpha = univariate_calpha())$confint

tab3c = cbind(apply(round(l3C, 2), 1, function(x) paste(paste(paste(paste(x[1], "(", sep=" "), x[2], sep=""), x[3], sep=", "), ")", sep="")), round(l3P$test$pvalues, 2))
colnames(tab3c) = c("Est (95% CI)", "P-value")

##Appendix Table 3: Record of all comparisons of interest for adherence and 12-month change in weight, fat mass, and lean mass
word.doc(obj.list=list(tab3a, tab3b, tab3c), obj.title=c("Table 3a: 12-month weight change for one unit of improvement in adherence by sex and diet", "Table 3b: 12-month fat mass change for one unit of improvement in adherence by sex and diet", "Table 3c: 12-month lean mass change for one unit of improvement in adherence by sex and diet"), dest="/Users/jrigdon/Box sync/Rigdon/DIETFITS_Data/Aronica/Tables_2018-04-22.docx", ftype="Arial", col.odd="white")
