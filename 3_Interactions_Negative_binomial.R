library(data.table)
library(lme4)
library(lmerTest)
library(openxlsx)
library(haven)


#Load data
data <- fread("INSchool.txt")

df$SCHOOL <- as.factor(as.character(df$SCHOOL))
df$age <- as.numeric(as.character(df$age))
df$index_SES <- as.numeric(as.character(df$index_SES))
df$SEX <- as.factor(as.character(df$SEX))
df$SES <- scale(df$index_SES)
df$AGE <- scale(df$age)

#Create IQR variables
names(df)
expos <- names(df)[c(21:28)] 

for (i in expos) {
  Q1 <- quantile(df[[i]])[2]
  Q3 <- quantile(df[[i]])[4]
  df$new_col <- (df[[i]])/(Q3-Q1)
  df$new_col <- unlist(df$new_col)
  names(df)[names(df)=="new_col"] <- paste0(i,"_IQR")
}


df$AGE_GROUP[df$curs <= 6] <- "children"
df$AGE_GROUP[df$curs > 6] <- "adolescent"
df$AGE_GROUP <- as.factor(df$AGE_GROUP)



####################
### INTERACTIONS ###
####################
results <- list()

### AGE ###
for (vec in c("p_vals","b_stand","sample","formula","ci_lower","ci_upper")) {
  assign(vectores, numeric())
}
outcomes=names(df)[62:63] 
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ no2_IQR*AGE_GROUP
                  +SES+SEX+AGE
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[7,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[7,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[8,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[8,2]))
}
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ pm25_IQR*AGE_GROUP
                  +SES+SEX+AGE
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
 
  p_vals = c(p_vals, summary(model)$coef[7,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[7,1]))
  se_stand = c(se_stand, summary(model)$coef[7,2]) 
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[8,1])) 
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[8,2])) 
}
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ pm25abs_IQR*AGE_GROUP
                  +SES+SEX+AGE
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[7,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[7,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[8,1])) 
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[8,2])) 
}
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ pm10_IQR*AGE_GROUP
                  +SES+SEX+AGE
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[7,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[7,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[8,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[8,2]))
}
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ pmcoarse_IQR*AGE_GROUP
                  +SES+SEX+AGE
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[7,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[7,1]))
  se_stand = c(se_stand, summary(model)$coef[7,2]) 
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[8,1])) 
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[8,2])) 
}
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ ndvi_100_IQR*AGE_GROUP
                  +SES+SEX+AGE
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[7,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[7,1]))
  se_stand = c(se_stand, summary(model)$coef[7,2]) 
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[8,1])) 
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[8,2])) 
}
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ ndvi_300_IQR*AGE_GROUP
                  +SES+SEX+AGE
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[7,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[7,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[8,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[8,2]))
}
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ ndvi_500_IQR*AGE_GROUP
                  +SES+SEX+AGE
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[7,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[7,1]))
  se_stand = c(se_stand, summary(model)$coef[7,2]) 
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[8,1])) 
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[8,2])) 
}

expos=names(df)[c(69:76)]
interactions=paste0(expos,"*AGE_GROUP")
length(outcomes) * length(interactions)
interaction = c()
for (i in seq(1,length(interactions))) {interaction = c(interaction, rep(interactions[i], length(outcomes))) }
length(interaction)
outcomes_vec = c()
outcomes_vec = c(rep(outcomes, length(interactions)))
length(outcomes_vec)
formula1<-as.character(unlist(formula)) 
regression = data.frame(interaction=interaction, outcome=outcomes_vec, beta=b_stand, ci_lower=ci_lower, ci_upper=ci_upper, pval=p_vals, sample=sample,formula=formula1)
regression$sign = "" 
regression$sign[regression$pval <= 0.05] = "*" 

write.xlsx(regression,"Age_Interaction.xlsx")




### SEX ###
for (vec in c("p_vals","b_stand","sample","formula","ci_lower","ci_upper")) {
  assign(vectores, numeric())
}
outcomes=names(df)[62:63] 
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ no2_IQR*SEX
                   +SES+AGE
                   +(1|SCHOOL), data=df, verbose = T,
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[6,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[6,1])) 
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[7,1])) 
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[7,2])) 
} 
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ pm25_IQR*SEX
                   +SES+AGE
                   +(1|SCHOOL), data=df, verbose = T,
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[6,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[6,1])) 
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[7,1])) 
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[7,2])) 
} 
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ pm25abs_IQR*SEX
                   +SES+AGE
                   +(1|SCHOOL), data=df, verbose = T,
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[6,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[6,1])) 
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[7,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[7,2]))
} 
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ pm10_IQR*SEX
                   +SES+AGE
                   +(1|SCHOOL), data=df, verbose = T,
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[6,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[6,1])) 
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[7,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[7,2]))
} 
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ pmcoarse_IQR*SEX
                   +SES+AGE
                   +(1|SCHOOL), data=df, verbose = T,
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[6,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[6,1])) 
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[7,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[7,2]))
} 
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ ndvi_100_IQR*SEX
                   +SES+AGE
                   +(1|SCHOOL), data=df, verbose = T,
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[6,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[6,1])) 
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[7,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[7,2]))
} 
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ ndvi_300_IQR*SEX
                   +SES+AGE
                   +(1|SCHOOL), data=df, verbose = T,
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[6,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[6,1])) 
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[7,1])) 
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[7,2])) 
} 
for (outcome in outcomes) {
  model <- glmer.nb(get(outcome) ~ ndvi_500_IQR*SEX
                   +SES+AGE
                   +(1|SCHOOL), data=df, verbose = T,
                   control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))  
  p_vals = c(p_vals, summary(model)$coef[6,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[6,1])) 
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[7,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[7,2]))
} 

expos=names(df)[c(69:76)]
interactions=paste0(expos,"*SEX")
length(outcomes) * length(interactions) 
interaction = c() 
for (i in seq(1,length(interactions))) {interaction = c(interaction, rep(interactions[i], length(outcomes))) }
length(interaction)
outcomes_vec = c()
outcomes_vec = c(rep(outcomes, length(interactions)))
length(outcomes_vec)
formula1<-as.character(unlist(formula)) 
regression = data.frame(interaction=interaction, outcome=outcomes_vec, beta=b_stand, ci_lower=ci_lower, ci_upper=ci_upper, pval=p_vals, sample=sample,formula=formula1)
regression$sign = "" 
regression$sign[regression$pval <= 0.05] = "*" 

write.xlsx(regression,"Sex_Interaction.xlsx")






results <- list()
### PRS-DEPRE ###
for (vec in c("p_vals","b_stand","sample","formula","ci_lower","ci_upper")) {
  assign(vectores, numeric())
}
# outcomes <- names(df)[62] #BroadBand
outcomes <- names(df)[53:55] #Subscales
for (outcome in outcomes){
  model <- glmer.nb(get(outcome) ~ pm10_IQR*scale(SCORE_Meta_Depre)
                  +SES+AGE+SEX
                  +scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10)
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[17,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[17,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[18,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[18,2]))
}

for (outcome in outcomes){
  model <- glmer.nb(get(outcome) ~ pmcoarse_IQR*scale(SCORE_Meta_Depre)
                  +SES+AGE+SEX
                  +scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10)
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[17,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[17,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[18,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[18,2]))
}


### PRS-ANX ###
for (outcome in outcomes){
  model <- glmer.nb(get(outcome) ~ pm10_IQR*scale(SCORE_Meta_anxiety)
                  +SES+AGE+SEX
                  +scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10)
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[17,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[17,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[18,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[18,2]))
}

for (outcome in outcomes){
  model <- glmer.nb(get(outcome) ~ pmcoarse_IQR*scale(SCORE_Meta_anxiety)
                  +SES+AGE+SEX
                  +scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10)
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[17,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[17,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[18,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[18,2]))
}




### PRS ADHD ###
# outcomes <- names(df)[63] #BroadBand
outcomes <- names(df)[59:60] #Subscales
for (outcome in outcomes){
  model <- glmer.nb(get(outcome) ~ ndvi_100_IQR*scale(SCORE_ADHD)
                  +SES+AGE+SEX
                  +scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10)
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[17,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[17,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[18,1])) 
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[18,2])) 
}


### PRS Externalizing ###
for (outcome in outcomes){
  model <- glmer.nb(get(outcome) ~ ndvi_100_IQR*scale(SCORE_externalizing)
                  +SES+AGE+SEX
                  +scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10)
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[17,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[17,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[18,1])) 
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[18,2])) 
}




### SES ###
# outcomes <- names(df)[62] #BroadBand
outcomes <- names(df)[53:55] #Subscales
for (outcome in outcomes){
  model <- glmer.nb(get(outcome) ~ pm10_IQR*SES
                  +AGE+SEX
                  +(1|SCHOOL), data=df, verbose = T, 
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[6,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[6,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[7,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[7,2]))
}

for (outcome in outcomes){
  model <- glmer.nb(get(outcome) ~ pmcoarse_IQR*+SES
                  +AGE+SEX
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[6,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[6,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[7,1]))
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[7,2]))
}

# outcomes <- names(df)[63] #BroadBand
outcomes <- names(df)[59:60] #Subscales
for (outcome in outcomes){
  model <- glmer.nb(get(outcome) ~ ndvi_100_IQR*SES
                  +AGE+SEX
                  +(1|SCHOOL), data=df, verbose = T,
                  control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
  p_vals = c(p_vals, summary(model)$coef[6,4]) 
  b_stand = c(b_stand, exp(summary(model)$coef[6,1]))
  sample = c(sample, nobs(model))
  formula = c(formula, formula(model))
  ci_lower = c(ci_lower, exp(confint(model,method="Wald")[7,1])) #con random
  ci_upper = c(ci_upper, exp(confint(model,method="Wald")[7,2])) #con random
}


formula1<-as.character(unlist(formula))
regression = data.frame(
  interaction=c(rep("pm10_IQR*Meta_Depre",3),rep("pmcoarse_IQR*Meta_Depre",3),
                rep("pm10_IQR*Meta_anxiety",3),rep("pmcoarse_IQR*Meta_anxiety",3),
                rep("ndvi_100_IQR*ADHD",2),rep("ndvi_100_IQR*externalizing",2),
                rep("pm10_IQR*index_SES",3),rep("pmcoarse_IQR*index_SES",3),rep("ndvi_100_IQR*index_SES",2)),
  outcome=c(rep(names(df)[53:55],4),rep(names(df)[59:60],2),rep(names(df)[53:55],2),names(df)[59:60]),
  beta=b_stand, ci_lower=ci_lower, ci_upper=ci_upper, pval=p_vals, sample=sample, formula=formula1)
regression$sign = "" 
regression$sign[regression$pval <= 0.05] = "*" 

write.xlsx(regression,"PRS_SES_Interactions.xlsx")
