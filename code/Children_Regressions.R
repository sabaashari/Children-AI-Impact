#Library imports ###########
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#Library imports ###########
library(mlogit)
library(tidyverse)
library(tidyr)
library(dplyr)
library(scales)
library(gridExtra)
library(MASS)
library(survey)
library(nnet)
library("gridExtra")
library(aod)
library(car)
library(gam)
library(ResourceSelection)
library(stargazer)
library(pROC)
library(margins)
library(brant) 
library(VGAM)
library(clm)

## To do: 
## 3. update git page. 
# 4. AME for child15, what is the issue? I can just leave it the way it is and come back to this later
# 5. review the tests and see if they are all correct

### Function definitions ####
run_logit <- function(df, vars, option = 'normal'){
  #questions <- c('gen_use_child', 'gen_use_parent', 'gen_ai_heard', 'gen_ai_talked', 'gen_use_household')
  questions <- c("child11_1", 
    "child11_2", 'child11_3', "child11_4", "child11_5", 'child11_6', 
    "child11_7", "child11_8", 'child11_9', 'child11_10', 'child11_12', 
    'child11_13', 'child11_14', 'child11_15')
  summary_all <- data.frame()
  vif_all <- data.frame()
  for (i in 1:length(questions))
  {
    formula <- as.formula(paste(questions[i], "~", paste(vars, collapse = "+"))) 
    if (option=='survey'){
      survey_design <- svydesign(
        ids = ~1,
        weights = ~ weight, # Survey weights variable - I should check this to see
        data = df
      )
      model <- svyglm(formula, design=survey_design, family= quasibinomial())
    }
    else
      model <- glm(formula, data = df , family = quasibinomial(), weights = df$weight)
    print(questions[i])
    s <- summary(model)
    marginal_effects <- margins(model,design=survey_design )
    
    ### Hosmer-Lemeshow test - 
    #High p-value (> 0.05): Indicates that the model fits the data well. 
    #The null hypothesis (that the observed and expected frequencies are similar) is not rejected, 
    #suggesting no evidence of lack of fit.
    fit <- model$fitted
    hoslem_test <- hoslem.test(df[[questions[i]]], fit, g = 10)
    print(hoslem_test)
    
    roc_curve <- roc(df[[questions[i]]], fit)
    auc_value <- auc(roc_curve)
    
    ##High p-value (> 0.05) The null hypothesis (that the observed and expected frequencies are similar) is not rejected, 
    #suggesting no evidence of lack of fit. - This is more straightforward version of hoslem test
    r <- (df[[questions[i]]] - fit)/(sqrt(fit*(1-fit)))
    p <- 1- pchisq(sum(r^2), df = 929 - s$df[1]-1)
    print(paste("Pschisq_1",p))
    
    #### Deviance and Likelihood Ratio Test - 
    # A low p-value (typically < 0.05) indicates that the predictors significantly improve the model fit 
    # which means the null hypothesis is rejected, suggesting significant difference between model and null model
    null_model <- glm(as.formula(paste(questions[i], "~ 1")), data = df,  family =quasibinomial(), weights = df$weight)
    anov <- anova(null_model, model, test = "Chisq")
    print(anov)
    
    
    ### difference between model and null model - higher is better, greater than zero, p_value < 0.05 - like anova 
    print("Pschisq_2:") ## tests
    print(1- pchisq(s$null.deviance-s$deviance, df= s$df[1]))
    
    print(vif(model))
    
    # Pseudo R-squared
    R2_McFadden <- 1 - (model$deviance / model$null.deviance)
    print(paste("R2_McFadde:", R2_McFadden))
    
    R2_CoxSnell <- 1 - exp(( model$null.deviance - model$deviance) / 929)
    print(paste("R2_CoxSnell:", R2_CoxSnell))
    
    R2_Nagelkerke <- R2_CoxSnell / (1 - exp(model$null.deviance/929))
    print(paste("R2_Nagelkerke:", R2_Nagelkerke))
    
    summary <- as.data.frame(summary(model)$coefficients)
    names(summary)[names(summary) == "Pr(>|t|)" | names(summary) == "Pr(>|z|)"] <- "p_value"
    summary$coef_names <- rownames(summary)
    tmp <- summary(marginal_effects) %>% dplyr:: select(factor, AME)
    names(tmp)[names(tmp) == "factor"] <- "coef_names"
    summary <- merge(summary,tmp, by = "coef_names")
    summary <- summary[c('coef_names', 'Estimate','p_value', 'AME')]
    
    summary <- rbind(summary,c('R2_McFadden',as.double(R2_McFadden),'',''))
    summary <- rbind(summary,c('AUC',as.double(auc_value),'',''))
    summary <- rbind(summary, c('hoslem_test',hoslem_test$statistic, as.double(hoslem_test$p.value),''))
    summary <- rbind(summary, c('AIC',AIC(model)[2],'',''))
    summary <- rbind(summary, c('Annova',as.double(anov[2,4]), as.double(anov[2,5]),''))
    summary$p_value <- as.double(summary$p_value)
    summary$Estimate <- as.double(summary$Estimate)
    summary <- summary %>% mutate(
      p_value = case_when(
        p_value < 0.01 ~ paste(round(p_value,4),'**'), 
        p_value < 0.05 ~ paste(round(p_value,4),'*'),
        p_value < 0.1 ~ paste(round(p_value,4),'~'),
        TRUE ~ as.character(round(p_value,4))), 
      Estimate  = round(Estimate,4))
    names(summary)[2:4] <- paste(names(summary)[2:4],questions[i])
    if (i==1)
    {
      summary_all <- summary
      vif_all <- vif(model)
    }
    else
    {
      summary_all <- merge(summary_all,summary, by = 'coef_names', all = TRUE)
      vif_all <- cbind(vif_all, vif(model))
    }
  }
  vif_all <- cbind(vars, vif_all)
  return (list(table = summary_all, vif = vif_all, model = model))
}

run_ordered_logit <- function(dd, formula, question) {
  
  vglm_model <- vglm(formula, data = dd, weights = dd$weight,
    family = cumulative(parallel = FALSE ~ 1  + medical_area_aggregatedRadiology, reverse = TRUE))
  ctable <- coef(summary(vglm_model))
  result <- data.frame('coef' = ctable[, "Estimate"], 'p_value'= ctable[, "Pr(>|z|)"]) 
  result <- round(result,4)
  result <- result %>% mutate(
    p_value = case_when(
      p_value < 0.01 ~ paste(p_value,'**'), 
      p_value < 0.05 ~ paste(p_value,'*'),
      p_value < 0.1 ~ paste(p_value,'~'),
      TRUE ~ as.character(p_value)))
  result$coef_names <- rownames(result)
  names(result)[1:2] <- paste(names(result)[1:2], question)
  return (result= result)
}

####..Read data and clean it ####
df <- read_csv("../../Data/Alan Turing Institute - Generative AI (children) - Labels 021224.csv")
df <- df %>% rename(school_type = parent2, learning_needs = parent3, internet_access = parent8, 
         gen_use_child = child8, gen_use_parent = parent10, gen_ai_desc = child6, 
         gen_ai_heard = child2, gen_ai_talked = child3, how_often = child12 , gen_use_household = parent11)


regions <- c("profile_GOR_yks_1", "profile_GOR_yks_2", "profile_GOR_yks_3", "profile_GOR_yks_4",
            "profile_GOR_yks_6", "profile_GOR_yks_7", "profile_GOR_yks_8")

dd <- df %>%  dplyr:: mutate(across(all_of(regions), 
                                    ~ case_when(
                                      .%in% c("Yes")   ~ 1,
                                      .%in% c("No")  ~ 0 )))

#clean dataset
df <- df %>% mutate(region = case_when(
  profile_GOR_yks_1 == 'Yes' ~ "North",
  profile_GOR_yks_2 == 'Yes' ~ "Midlands",
  profile_GOR_yks_3 == 'Yes' ~ "London",
  profile_GOR_yks_4 == 'Yes' ~ "East, South",
  profile_GOR_yks_6 == 'Yes' ~ "Wales",
  profile_GOR_yks_7 == 'Yes' ~ "Scotland",
  profile_GOR_yks_8 == 'Yes' ~ "Northern Ireland",
  TRUE~ NA_character_))

df <- df %>% mutate(socialgrade = case_when(
  profile_socialgrade_cie_yks_1 == 'Yes' ~ "AB",
  profile_socialgrade_cie_yks_2 == 'Yes' ~ "C1",
  profile_socialgrade_cie_yks_3 == 'Yes' ~ "C2",
  profile_socialgrade_cie_yks_4 == 'Yes' ~ "DE",
  TRUE~ NA_character_))


df$socialgrade <- relevel(factor(df$socialgrade), ref = "DE")

df <- df %>% rename(North = profile_GOR_yks_1, Midlands = profile_GOR_yks_2, 
                    Lodnon = profile_GOR_yks_3, East_South = profile_GOR_yks_4,
                    Net_England = profile_GOR_yks_5, Wales = profile_GOR_yks_6, 
                    Scotland = profile_GOR_yks_7, Northern_Ireland = profile_GOR_yks_8)
df$childage <- as.character(df$childage)

df$gen_use_child<- ifelse(df$gen_use_child=='Yes' ,1,0)
df$gen_use_parent<- ifelse(grepl('Yes',df$gen_use_parent,'Yes') ,1,0)
df$gen_ai_heard<- ifelse(grepl('Yes',df$gen_ai_heard,'Yes') ,1,0)
df$gen_ai_talked<- ifelse(grepl('Yes',df$gen_ai_talked,'Yes') ,1,0)
df[df$learning_needs=='Prefer not to say', 'learning_needs'] <- "Yes"
df[df$school_type=='Homeschooling', 'school_type'] <- "Other"

demographics <- c('childgender','school_type', 'childage', 'learning_needs', 'gen_use_child', 'gen_use_parent', 'socialgrade', 'region')

basic_stat <- df %>% dplyr::select(all_of(c(demographics, 'weight'))) %>%  
  reshape2::melt('weight')%>% group_by(variable,value) %>% summarise(n_w = sum(weight), n = n()) %>% 
  mutate(freq = round(n/sum(n)*100), freq_w = round(n_w/sum(n_w)*100)) 

school_stat_gender <- df %>% group_by(school_type,childgender) %>% summarise(n = n(), n_w= sum(weight)) %>% 
  ungroup() %>% group_by(school_type) %>% mutate(freq = round(n/sum(n)*100), freq_w = round(n_w/sum(n_w)*100))
colnames(school_stat_gender)[1:2] <- c('variable','value')

school_stat_learning <- df %>% group_by(school_type,learning_needs) %>% summarise(n = n(), n_w = sum(weight)) %>% 
  ungroup() %>% group_by(school_type) %>% mutate(freq = round(n/sum(n)*100), freq_w = round(n_w/sum(n_w)*100))
colnames(school_stat_learning)[1:2] <- c('variable','value')
basic_stat <- rbind(basic_stat,school_stat_gender,school_stat_learning)
write.xlsx(school_stat_regions, '../../Data/basic.stat.xlsx')

school_stat_socialgrade <- df %>% group_by(school_type,socialgrade) %>% summarise(n = n(), n_w = sum(weight)) %>% 
  ungroup() %>% group_by(socialgrade) %>% mutate(freq = round(n/sum(n)*100), freq_w = round(n_w/sum(n_w)*100))
colnames(school_stat_socialgrade)[1:2] <- c('variable','value')

school_stat_regions <- df %>% group_by(school_type,region) %>% summarise(n = n(), n_w = sum(weight)) %>% 
  ungroup() %>% group_by(region) %>% mutate(freq = round(n/sum(n)*100), freq_w = round(n_w/sum(n_w)*100))
colnames(school_stat_regions)[1:2] <- c('variable','value')
school_stat <- rbind(school_stat_socialgrade,school_stat_regions)
write.xlsx(school_stat, '../../Data/school_stats.xlsx')


### Use Regressions ####
vars <- c('childgender','school_type', 'childage', 'learning_needs', 'region', 'socialgrade')
questions <- c('gen_use_child', 'gen_use_parent', 'gen_ai_heard', 'gen_ai_talked')
df$gen_use_household <- ifelse(df$gen_use_household %in% c("Yes, I have used generative AI tools", 
                                                           "I have not, but someone else in my household has used generative AI tools"), 1, 0)

df <- df %>% mutate(socialgrade = case_when(
  socialgrade %in% c('AB', "C1") ~ "ABC1",
  socialgrade %in% c('C2', "DE") ~ "C2DE",
  TRUE~ NA_character_))

output <- run_logit(df, vars, 'survey')
AI_use_results <- output$table
write.xlsx(AI_use_results, "../../Data/Children_use_Regressions.xlsx")

## How often ####
dd <- df %>% filter(gen_use_child==1)
formula <- as.formula(paste("how_often", "~", paste(vars, collapse = "+")))

dd <- dd %>% filter(how_often !="I'm not sure") %>% dplyr:: mutate(across(all_of('how_often'), 
                                    ~ case_when(
                                      .%in% c("Every day")   ~ 6,
                                      .%in% c("A few times a week")  ~ 5,
                                      .%in% c("Once every few weeks")  ~ 4,
                                      .%in% c("Once a month")  ~ 3,
                                      .%in% c("I have only used it a few times")  ~ 2,
                                      .%in% c("I have only used it once")  ~ 1,
                                      TRUE ~ 0
                                    )))
result <- run_ordered_logit(dd, formula, "how_often")

### AI purposes ####
child11 <- "What have you used generative AI for?" ## gen AI users
gen_ai_purpose <- df %>% filter(child_used_ai == 'Yes') %>% dplyr::select('weight', contains('child11')) %>% 
  reshape2::melt('weight')%>% group_by(variable,value) %>% summarise(n_weighted = sum(weight), n = n()) %>% 
  mutate(freq = round(n/sum(n),2), freq_weighted = round(n_weighted/sum(n_weighted),2))  %>% filter(value == 'Yes')
choices <- c('Entertainment (to play around)','Playing with my friends',
             'Curiosity','To find out information or learn about something',
             'Getting advice on something personal','To help me with my homework or schoolwork',
             'Inspiration for new ideas','Chatting and keeping me company','Writing stories',
             'Writing poems or lyrics','Writing non-fiction','Creating fun pictures','Making songs',
             'Other (open [child11other]) [open] please specify', 'I’m not sure')

questions <- c("child11_1", 
               "child11_2", 'child11_3', "child11_4", "child11_5", 'child11_6', 
               "child11_7", "child11_8", 'child11_9', 'child11_10', 'child11_12', 
               'child11_13', 'child11_14', 'child11_15')
child_dict <- hash(keys = questions , values = choices)
gen_ai_purpose$variable <- choices

dd <- dd %>% filter(child_used_ai == 'Yes') %>% dplyr:: mutate(across(all_of(questions), 
                                                                          ~ case_when(
                                                                            .%in% c("Yes")~ 1,
                                                                            TRUE ~ 0
                                                                          )))
output <- run_logit(dd, vars, 'survey')
results <- output$table
write.xlsx(results, "../../Data/AI_use_cases_children_reg.xlsx")
## Feelings, 15: children used AI ####
child15 <- "Take a minute to read each of these sentences, and then choose from the following options:"
child_dict <- hash(
  keys = c("child15_1", 
           "child15_2", 'child15_3', "child15_4", "child15_5", 
           'child15_6', "child15_7", "child15_8", 'child15_9'),
  values = c('self_expression', 'communication', 'understanding_me', 'control',
             'understand_output', 'new_ideas','creativity', 'correct_output', 'share_anything'))

questions <- ls(child_dict)
dd <- df %>%  dplyr:: mutate(across(all_of(questions), 
                             ~ case_when(
                               .%in% c("Always")   ~ 2,
                               .%in% c("Sometimes")  ~ 1,
                               .%in% c("I'm not sure")  ~ 0,
                               .%in% c("Never")  ~ -1,
                               TRUE ~ 0
                             )))


dd <- dd %>% mutate(across(all_of(questions), ~ factor(.x, ordered = TRUE)))
dd <-  dd%>%filter(gen_use_child ==1 & school_type!='Other') ## not enough data in school_type other
vars <- c('childgender', 'childage', 'learning_needs', 'school_type', 'socialgrade','region')
all_result <- data.frame()
for (i in 1:length(questions))
{
  question <- questions[i]
  formula <- as.formula(paste(question, "~", paste(vars, collapse = "+"))) 
  result <- run_ordered_logit(dd, formula, child_dict[[question]])
  if (i==1)
    all_result <- result
  else
    all_result <- merge(all_result,result, by = "coef_names", all = TRUE)
}
child15_results <- all_result
## Children Feelings: Scary, exciting ####
vars <- c('childgender', 'childage', 'learning_needs', 'school_type', 'gen_use_child', 'socialgrade','region')
child20 <- c("child20_scary","schild20_exciting")
dd <- df %>% filter(school_type != 'Other')
dd <- dd %>%   mutate(across(child20_scary, ~ case_when(
                               .%in% c("I don't find it scary/confusing")  ~ 1,
                               .%in% c("2")   ~ 2,
                               .%in% c("3", "Not sure")  ~ 3,
                               .%in% c("4")   ~ 4,
                               .%in% c("5 - I find it scary/confusing")  ~ 5,
                               TRUE ~ 0
                             )))

dd <- dd %>%   mutate(across(child20_exciting, ~ case_when(
  .%in% c("5 - I don't find it exciting")  ~ 1,
  .%in% c("4")   ~ 2,
  .%in% c("3", "Not sure")  ~ 3,
  .%in% c("2")   ~ 4,
  .%in% c("1 - I find it exciting")  ~ 5,
  TRUE ~ 0
)))

formula <- as.formula(paste('child20_scary', "~", paste(vars, collapse = "+"))) 
all_result <- run_ordered_logit(dd, formula, 'child20_scary')
formula <- as.formula(paste('child20_exciting', "~", paste(vars, collapse = "+"))) 
result <- run_ordered_logit(dd, formula, 'child20_exciting')
all_result <- merge(all_result,result, by = "coef_names", all = TRUE)
child20_results <- all_result



## parent13: feelings of gen-AI user parents ####
parent_13 <- "To what extent do you feel positively or negatively about your child's use of generative AI?"
dd <- df %>% filter(gen_use_parent==1, school_type!='Other')
dd <- dd %>%   mutate(across(parent13, ~ case_when(
  .%in% c("Very negatively")  ~ -2,
  .%in% c("Fairly negatively")   ~ -1,
  .%in% c("Don't know", "Neither positively nor negatively")  ~ 0,
  .%in% c("Fairly positively")   ~ 1,
  .%in% c("Very positively")  ~ 2,
  TRUE ~ 0
)))
vars <- c('childgender','school_type', 'childage', 'learning_needs', 'socialgrade','region')
formula <- as.formula(paste('parent13', "~", paste(vars, collapse = "+"))) 
parent13_results <- run_ordered_logit(dd, formula, 'positive_negative')


## parent15: feelings of all parents ####
parent15 <- "How concerned or unconcerned are you by each of the following? (All parents)" ## all parents

questions <- c( 'parent15_1', 'parent15_2', 'parent15_3', 'parent15_4', 'parent15_5')
question_text <-  c("inappropriate_information",
           "too_trusting_not_think_critically",
           "personal_information_share",
           "cheat_in_school",
           "false_information")

dd <- df %>% filter(school_type!='Other')  %>% mutate(across(all_of(questions), 
                             ~ case_when(
                               .%in% c("Very unconcerned")   ~ 1,
                               .%in% c("Fairly unconcerned")  ~ 2,
                               .%in% c("Neither concerned nor unconcerned","Don't know")  ~ 3,
                               .%in% c("Fairly concerned")  ~ 4,
                               .%in% c("Very concerned")  ~ 5,
                               TRUE ~ 0
                             )))

vars <- c('childgender', 'childage', 'learning_needs', 'school_type', 'gen_use_parent', 'socialgrade','region')
all_result <- data.frame()
for (i in 1:length(questions))
{
  question <- questions[i]
  formula <- as.formula(paste(question, "~", paste(vars, collapse = "+"))) 
  result <- run_ordered_logit(dd, formula, question_text[i])
  if (i==1)
    all_result <- result
  else
    all_result <- merge(all_result,result, by = "coef_names", all = TRUE)
}
parent15_results <- all_result

#### save the result ####
OUT <- createWorkbook()
addWorksheet(OUT, "AI_use")
addWorksheet(OUT, "child15")
addWorksheet(OUT, "child20")
addWorksheet(OUT, "parent13")
addWorksheet(OUT, "parent15")

writeData(OUT, sheet = "AI_use", x = AI_use_results)
writeData(OUT, sheet = "child15", x = child15_results)
writeData(OUT, sheet = "child20", x = child20_results)
writeData(OUT, sheet = "parent13", x = parent13_results)
writeData(OUT, sheet = "parent15", x = parent15_results)

saveWorkbook(OUT,  "../../Data/Regressions.xlsx")
           
