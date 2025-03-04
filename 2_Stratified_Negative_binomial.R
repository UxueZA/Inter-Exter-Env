library(data.table)
library(lme4)
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


#Stratify by age
children <- df[df$curs <= 6,]
dim(children)
adolescent <- df[df$curs >6,]
dim(adolescent)
#Stratify by sex:
boys <- df[df$SEX =="boys",]
dim(boys)
girls <- df[df$SEX == "girls",]
dim(girls)



#############################
### NEGATIVE BINOMIAL IQR ###
#############################

## children ##
names(children)
outcomes=names(children)[62:63]
expos=names(children)[c(69:76)] 
for (vec in c("p_vals","b_stand","sample","formula","ci_lower","ci_upper")) {
  assign(vectores, numeric())
}

for (expo in expos) {
  for (outcome in outcomes) {
    model <- glmer.nb(get(outcome) ~ get(expo)
                    +AGE+SES+SEX
                    +(1|SCHOOL), data=children, verbose = T,
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    p_vals = c(p_vals, summary(model)$coef[2,4]) 
    b_stand = c(b_stand, exp(summary(model)$coef[2,1]))
    sample = c(sample, nobs(model))
    formula = c(formula, formula(model))
    ci_lower = c(ci_lower, exp(confint(model,method="Wald")[3,1]))
    ci_upper = c(ci_upper, exp(confint(model,method="Wald")[3,2]))
  } 
}

length(outcomes) * length(expos)
pred = c()
seq(1, length(expos))
for (i in seq(1,length(expos))) {pred = c(pred, rep(expos[i], length(outcomes))) }
length(pred)
outcomes_vec = c(rep(outcomes, length(expos)))
formula1<-as.character(unlist(formula))
regression = data.frame(pred=pred, outcome=outcomes_vec, beta=b_stand, ci_lower=ci_lower, ci_upper=ci_upper, pval=p_vals, sample=sample,formula=formula1)
regression$sign = "" 
regression$sign[regression$pval <= 0.05] = "*" 

results <- list()
results[["children_negative-binomial"]] <- regression


## adolescent ##
names(adolescent)
outcomes=names(adolescent)[62:63]
expos=names(adolescent)[c(69:76)] 
for (vec in c("p_vals","b_stand","sample","formula","ci_lower","ci_upper")) {
  assign(vectores, numeric())
}

for (expo in expos) {
  for (outcome in outcomes) {
    model <- glmer.nb(get(outcome) ~ get(expo)
                    +AGE+SES+SEX
                    +(1|SCHOOL), data=adolescent, verbose = T,
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    p_vals = c(p_vals, summary(model)$coef[2,4]) 
    b_stand = c(b_stand, exp(summary(model)$coef[2,1])) 
    sample = c(sample, nobs(model))
    formula = c(formula, formula(model))
    ci_lower = c(ci_lower, exp(confint(model,method="Wald")[3,1]))
    ci_upper = c(ci_upper, exp(confint(model,method="Wald")[3,2]))
  } 
}

length(outcomes) * length(expos)
pred = c()
seq(1, length(expos))
for (i in seq(1,length(expos))) {pred = c(pred, rep(expos[i], length(outcomes))) }
length(pred)
outcomes_vec = c(rep(outcomes, length(expos)))
formula1<-as.character(unlist(formula))
regression = data.frame(pred=pred, outcome=outcomes_vec, beta=b_stand, ci_lower=ci_lower, ci_upper=ci_upper, pval=p_vals, sample=sample,formula=formula1)
regression$sign = "" 
regression$sign[regression$pval <= 0.05] = "*" 

results[["adolescent_negative-binomial"]] <- regression

#Save results
write.xlsx(results, "Age_Stratified.xlsx")




## girls ##
names(girls)
outcomes=names(girls)[62:63]
expos=names(girls)[c(69:76)] 
for (vec in c("p_vals","b_stand","sample","formula","ci_lower","ci_upper")) {
  assign(vectores, numeric())
}
for (expo in expos) {
  for (outcome in outcomes) {
    model <- glmer.nb(get(outcome) ~ get(expo)
                    +AGE+SES
                    +(1|SCHOOL), data=girls, verbose = T,
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    p_vals = c(p_vals, summary(model)$coef[2,4]) 
    b_stand = c(b_stand, exp(summary(model)$coef[2,1]))
    sample = c(sample, nobs(model))
    formula = c(formula, formula(model))
    ci_lower = c(ci_lower, exp(confint(model,method="Wald")[3,1]))
    ci_upper = c(ci_upper, exp(confint(model,method="Wald")[3,2]))
  } 
}

length(outcomes) * length(expos)
pred = c()
seq(1, length(expos))
for (i in seq(1,length(expos))) {pred = c(pred, rep(expos[i], length(outcomes))) }
length(pred)
outcomes_vec = c(rep(outcomes, length(expos)))
formula1<-as.character(unlist(formula))
regression = data.frame(pred=pred, outcome=outcomes_vec, beta=b_stand, ci_lower=ci_lower, ci_upper=ci_upper, pval=p_vals, sample=sample,formula=formula1)
regression$sign = "" 
regression$sign[regression$pval <= 0.05] = "*" 

results <- list()
results[["girls_negative-binomial"]] <- regression


## boys ##
names(boys)
outcomes=names(boys)[62:63]
expos=names(boys)[c(69:76)] 
for (vec in c("p_vals","b_stand","sample","formula","ci_lower","ci_upper")) {
  assign(vectores, numeric())
}
for (expo in expos) {
  for (outcome in outcomes) {
    model <- glmer.nb(get(outcome) ~ get(expo)
                    +AGE+SES
                    +(1|SCHOOL), data=boys, verbose = T,
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    p_vals = c(p_vals, summary(model)$coef[2,4]) 
    b_stand = c(b_stand, exp(summary(model)$coef[2,1]))
    sample = c(sample, nobs(model))
    formula = c(formula, formula(model))
    ci_lower = c(ci_lower, exp(confint(model,method="Wald")[3,1]))
    ci_upper = c(ci_upper, exp(confint(model,method="Wald")[3,2]))
  } 
}

length(outcomes) * length(expos)
pred = c()
seq(1, length(expos))
for (i in seq(1,length(expos))) {pred = c(pred, rep(expos[i], length(outcomes))) }
length(pred)
outcomes_vec = c(rep(outcomes, length(expos)))
formula1<-as.character(unlist(formula))  
regression = data.frame(pred=pred, outcome=outcomes_vec, beta=b_stand, ci_lower=ci_lower, ci_upper=ci_upper, pval=p_vals, sample=sample,formula=formula1)
regression$sign = "" 
regression$sign[regression$pval <= 0.05] = "*" 

results[["boys_negative-binomial"]] <- regression

#Save results
write.xlsx(results, "Sex_Stratified.xlsx")
