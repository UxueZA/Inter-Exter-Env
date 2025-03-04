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
IQR_table <- data.frame(expos=expos)

for (i in expos) {
  Q1 <- quantile(df[[i]])[2]
  IQR_table[IQR_table$expo == i,"Q1"] <- Q1
  Q3 <- quantile(df[[i]])[4]
  IQR_table[IQR_table$expo == i,"Q3"] <- Q3
  df$new_col <- (df[[i]])/(Q3-Q1)
  df$new_col <- unlist(df$new_col)
  names(df)[names(df)=="new_col"] <- paste0(i,"_IQR")
}

IQR_table$IQR <- IQR_table$Q3 - IQR_table$Q1

#Guardar tabla IQR
write.xlsx(IQR_table, "IQR_table.xlsx")

#############################
### NEGATIVE BINOMIAL IQR ###
#############################
names(df)
outcomes=names(df)[62:63] 
expos=names(df)[c(69:76)]
for (vec in c("p_vals","b_stand","sample","formula","ci_lower","ci_upper")) {
  assign(vectores, numeric())
}
for (expo in expos) {
  for (outcome in outcomes) {
    model <- glmer.nb(get(outcome) ~ get(expo)
                    +SEX+AGE+SES
                    +(1|SCHOOL), data=df, verbose = T,
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    p_vals = c(p_vals, summary(model)$coef[2,4]) 
    b_stand = c(b_stand, exp(summary(model)$coef[2,1]) )
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
regression$FDR <- p.adjust(regression$pval, method = "fdr")

results <- list()
results[["main_negative-binomial"]] <- regression



### PRS ###
names(df)
outcomes=names(df)[62:63]
expos=names(df)[29:32] 
for (vec in c("p_vals","b_stand","sample","formula","ci_lower","ci_upper")) {
  assign(vectores, numeric())
}
for (expo in expos) {
  for (outcome in outcomes) {
    model <- glmer.nb(get(outcome) ~ scale(get(expo))
                    +SEX+AGE+SES
                    +scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10)
                    +(1|SCHOOL), data=df, verbose = T,
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
regression <- regression[c(2,3,5,8),]

results <- list()
results[["prs_negative-binomial"]] <- regression


### /SES/SEX/AGE ###
for (vec in c("p_vals","b_stand","sample","formula","ci_lower","ci_upper")) {
  assign(vectores, numeric())
}
outcomes=names(df)[62:63] 
for(outcome in outcomes) {
    model <- glmer.nb(get(outcome) ~ SEX
                    +AGE+SES
                    +(1|SCHOOL), data=df, verbose = T,
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    p_vals = c(p_vals, summary(model)$coef[2,4]) 
    b_stand = c(b_stand, exp(summary(model)$coef[2,1]))
    sample = c(sample, nobs(model))
    formula = c(formula, formula(model))
    ci_lower = c(ci_lower, exp(confint(model,method="Wald")[3,1]))
    ci_upper = c(ci_upper, exp(confint(model,method="Wald")[3,2]))

    model <- glmer.nb(get(outcome) ~ AGE
                    +SEX+SES
                    +(1|SCHOOL), data=df, verbose = T,
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    p_vals = c(p_vals, summary(model)$coef[2,4]) 
    b_stand = c(b_stand, exp(summary(model)$coef[2,1]))
    sample = c(sample, nobs(model))
    formula = c(formula, formula(model))
    ci_lower = c(ci_lower, exp(confint(model,method="Wald")[3,1]))
    ci_upper = c(ci_upper, exp(confint(model,method="Wald")[3,2]))

    model <- glmer.nb(get(outcome) ~ SES
                    +AGE+SEX
                    +(1|SCHOOL), data=df, verbose = T,
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    p_vals = c(p_vals, summary(model)$coef[2,4]) 
    b_stand = c(b_stand, exp(summary(model)$coef[2,1]))
    sample = c(sample, nobs(model))
    formula = c(formula, formula(model))
    ci_lower = c(ci_lower, exp(confint(model,method="Wald")[3,1]))
    ci_upper = c(ci_upper, exp(confint(model,method="Wald")[3,2]))
}

length(outcomes) * 3
pred = c()
pred = c(rep(c("SEX","AGE","index_SES"), length(outcomes)))
length(pred)
outcomes_vec = c()
for (i in c(1:2)) {outcomes_vec = c(outcomes_vec, rep(outcomes[i], 3))}
length(outcomes_vec)
formula1<-as.character(unlist(formula))
regression = data.frame(pred=pred, outcome=outcomes_vec, beta=b_stand, ci_lower=ci_lower, ci_upper=ci_upper, pval=p_vals, sample=sample,formula=formula1)
regression$sign = "" 
regression$sign[regression$pval <= 0.05] = "*" 

results[["sociodemographic"]] <- regression

### Save results ###
write.xlsx(results, "Broadband_Scales.xlsx")




##################
### SUBSCALES ###
##################
names(df)
outcomes=names(df)[c(53:55,59,60)] 
expos=names(df)[c(72:74)] 
for (vec in c("p_vals","b_stand","sample","formula","ci_lower","ci_upper")) {
  assign(vectores, numeric())
}
for (expo in expos) {
  for (outcome in outcomes) {
    model <- glmer.nb(get(outcome) ~ get(expo)
                    +SEX+AGE+SES
                    +(1|SCHOOL), data=df, verbose = T,
                    control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=100000)))
    p_vals = c(p_vals, summary(model)$coef[2,4]) 
    b_stand = c(b_stand, exp(summary(model)$coef[2,1]) )
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
regression <- regression[c(1:3,6:8,14,15),]

result <- list()
result[["subscales_negative-binomial"]] <- regression

### Save results ###
write.xlsx(result, "Syndrom_Scales.xlsx")
