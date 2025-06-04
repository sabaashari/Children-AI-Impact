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
library(openxlsx)


## To do: 
## 3. update git page. 
# 4. AME for child15, what is the issue? I can just leave it the way it is and come back to this later
# 5. review the tests and see if they are all correct

### Function definitions ####
clean_data <- function(df)
{
  # childrens age
  df[df$`age-range` %in% "Early years (0-5 years old)", 'age-range'] <- "0-5"
  df[df$`age-range` %in% "Key Stage 1 (5-7 years old)", 'age-range'] <- "5-7"
  df[df$`age-range` %in% "Key Stage 2 (7-11 years old)", 'age-range'] <- "7-11"
  df[df$`age-range` %in% "Key Stage 3 (11-14 years old)", 'age-range'] <- "11-14"
  df[df$`age-range` %in% "Key Stage 4 (14-16 years old)", 'age-range'] <- "14-16"
  df$`age-range` <- factor(df$`age-range`, levels = c("0-5","5-7", "7-11", "11-14", "14-16"), ordered = TRUE)
  df <- df %>% arrange(`age-range`)
  names(df)[names(df)=='age-range'] <- "Children-age"
  # Teachers' age
  df[df$age %in% c("60 years and over"), 'age'] <- "60+"
  df[df$age %in% c("30-34", "35-39"), 'age'] <- "30-39"
  df[df$age %in% c("40-44", "45-49"), 'age'] <- "40-49"
  df$age <- factor(df$age, levels = c("Under 30", "30-39", "40-49", "50-59", "60+"), ordered = TRUE)
  df <- df %>% arrange(age)
  names(df)[names(df)=='age'] <- "Teacher-age"
  
  ##geo-information
  df[df$`geo-info` %in% c("South East", "South West"), 'geo-info'] <- "South_England"
  df[df$`geo-info` %in% c("North East & Yorkshire", "North West"), 'geo-info'] <- "North_England"
  
  # years' experience
  df[df$`years-experience` %in% "Less than 5 years", "years-experience" ] <- "Less than 5"
  df[df$`years-experience` %in% "5-10 years", "years-experience"] <- "5-10"
  df[df$`years-experience` %in% "10-15 years", "years-experience"] <- "10-15"
  df[df$`years-experience` %in% "15-20 years", "years-experience"] <- "15-20"
  df[df$`years-experience` %in% "More than 20 years", "years-experience"] <- "More than 20"
  df$`years-experience` <- factor(df$`years-experience`, levels = c( "Less than 5", "5-10", "10-15", "15-20", "More than 20"), ordered = TRUE)
  df <- df %>% arrange(`years-experience`)
  
  ## School type
  df[df$`type-of-school` %in% "Other (please specify)", 'type-of-school'] <- "Other"
  df[df$`type-of-school` %in% "Private school (fee-paying)", 'type-of-school'] <- "Private"
  df[df$`type-of-school` %in% "State school", 'type-of-school'] <- "State"
  df$`type-of-school` <- factor(df$`type-of-school`, levels = c("State","Private", "Other"), ordered = TRUE)
  
  job_titles <- c("Teaching assistant", "Primary school teacher", "Secondary school teacher", "Special education needs", "Headteacher", "Other")
  subject_areas <- c("English", "Maths", "Science", "Design and technology", "History", "Geography", "Art and design", "Music", 
                     "Physical education", "Computing", "Ancient and modern foreign languages", "Other")
  
  for (job in job_titles)
    df[job] <- ifelse(grepl(job, df$`job-title`), 'Yes', 'No')
  for (subject in subject_areas)
    df[subject] <- ifelse(grepl(subject, df$`subject-area`), "Yes", "No")
  
  cases_list <- c("Designing exams", "designing homework assignments", "Marking exams", "Student feedback", 
                  "Lesson planning and research", "Developing personalised learning plans for students", 
                  "Generating educational content for classroom presentations", 
                  "Responding to parents' emails", "Other")
  for (case in cases_list)
    df[case] <- ifelse(grepl(case, df$`activities-genai`), 'Yes', "No")
  
  df <- df %>% mutate(system = ifelse(!is.na(`multiple-systems`), `multiple-systems`,`systems-used`))
  df <- df %>% mutate(statement_1 = ifelse(!is.na(`multiple-systems`), `statements-multiple_1`,`statement-single_1`))
  df <- df %>% mutate(statement_2 = ifelse(!is.na(`multiple-systems`), `statements-multiple_2`,`statement-single_2`))
  df <- df %>% mutate(statement_3 = ifelse(!is.na(`multiple-systems`), `statements-multiple_3`,`statement-single_3`))
  df <- df %>% mutate(statement_4 = ifelse(!is.na(`multiple-systems`), `statements-multiple_4`,`statement-single_4`))
  df$teacher_use<- ifelse(df$`genai-at-work` %in% 'Yes' ,1,0)
  
  system_lists <- c("ChatGPT", "Dall-E", "Midjourney", "Claude", "Perplexity", "Gemini", "CoPilot", "DeepSeek", "Other")
  for (system in system_lists)
    df[system] = ifelse(grepl(system, df$`systems-used`),'Yes', 'No')
  return (df)
}

run_logit <- function(dd, vars,question,  option = 'normal')
{
  summary_all <- data.frame()
  vif_all <- data.frame()
  formula <- as.formula(paste(question, "~", paste(vars, collapse = "+"))) 
  if (option=='survey'){
    survey_design <- svydesign(
      ids = ~1,
      weights = ~ weight, # Survey weights variable - I should check this to see
      data = dd
    )
    model <- svyglm(formula, design=survey_design, family= quasibinomial())
  }
  else
    model <- glm(formula, data = dd , family = quasibinomial())
  
  s <- summary(model)
  
  ### Hosmer-Lemeshow test - 
  #High p-value (> 0.05): Indicates that the model fits the data well. 
  #The null hypothesis (that the observed and expected frequencies are similar) is not rejected, 
  #suggesting no evidence of lack of fit.
  fit <- model$fitted
  hoslem_test <- hoslem.test(dd[[question]], fit, g = 10)
  print(hoslem_test)
  
  roc_curve <- roc(dd[[question]], fit)
  auc_value <- auc(roc_curve)
  
  ##High p-value (> 0.05) The null hypothesis (that the observed and expected frequencies are similar) is not rejected, 
  #suggesting no evidence of lack of fit. - This is more straightforward version of hoslem test
  r <- (dd[[question]] - fit)/(sqrt(fit*(1-fit)))
  p <- 1- pchisq(sum(r^2), df = 929 - s$df[1]-1)
  print(paste("Pschisq_1",p))
  
  #### Deviance and Likelihood Ratio Test - 
  # A low p-value (typically < 0.05) indicates that the predictors significantly improve the model fit 
  # which means the null hypothesis is rejected, suggesting significant difference between model and null model
  null_model <- glm(as.formula(paste(question, "~ 1")), data = dd,  family =quasibinomial())
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
  summary <- summary[c('coef_names', 'Estimate','p_value')]
  
  summary <- rbind(summary,c('R2_McFadden',as.double(R2_McFadden),''))
  summary <- rbind(summary,c('AUC',as.double(auc_value),''))
  summary <- rbind(summary, c('hoslem_test',hoslem_test$statistic, as.double(hoslem_test$p.value)))
  summary <- rbind(summary, c('AIC',AIC(model)[2],''))
  summary <- rbind(summary, c('Annova',as.double(anov[2,4]), as.double(anov[2,5])))
  summary$p_value <- as.double(summary$p_value)
  summary$Estimate <- as.double(summary$Estimate)
  summary <- summary %>% mutate(p_value = case_when(
      p_value < 0.01 ~ paste(round(p_value,4),'**'), 
      p_value < 0.05 ~ paste(round(p_value,4),'*'),
      p_value < 0.1 ~ paste(round(p_value,4),'~'),
      TRUE ~ as.character(round(p_value,4))), 
    Estimate  = round(Estimate,4))
  names(summary)[2:3] <- paste(names(summary)[2:3],question)
  return (list(table = summary, vif = vif(model), model = model))
}

run_ordered_logit <- function(dd, formula, question) {
  
  vglm_model <- vglm(formula, data = dd,family = cumulative(link = logitlink, parallel = TRUE))
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

## Read data and clean ####
df <- read.xlsx("../../Data/Understanding teachers' perspectives on AI - Final dataset.xlsx")
df <- df[2:nrow(df),]
df <- clean_data(df)
job_titles <- c("Teaching_assistant", "Primary_school_teacher", "Secondary_school_teacher", "Special_education_needs", "Headteacher")
subject_areas <- c("English", "Maths", "Science", "Design_and_technology", "History", "Geography", "Art_and_design", "Music", 
                   "Physical_education", "Ancient_and_modern_foreign_languages", "Computing")
df$weight <- 1
colnames(df) <- gsub("-", "_", colnames(df))
colnames(df) <- gsub(" ", "_", colnames(df))
df$type_of_school <- as.character(df$type_of_school)  # Remove ordering
df$type_of_school <- factor(df$type_of_school, levels = c("State", "Private", "Other"))
str(df$type_of_school)
df <- df %>% filter(apply(df, 1, function(row) !any(row %in% "Prefer not to say")))
df <- df %>% filter(!(working_hours %in% "Other (please specify)"))
OUT <- createWorkbook()


## Teachers' use ####
dd <- df %>% filter(gender %in% c('Female', 'Male'), type_of_school %in% c("Private", "State"))
vars <- c("Teacher_age", "gender", "geo_info", "type_of_school","additional_needs", "working_hours", 
          "Children_age", "years_experience", job_titles, subject_areas)
dd$Children_age <- factor(dd$Children_age, ordered = FALSE)
dd$Teacher_age <- factor(dd$Teacher_age, ordered = FALSE)
dd$years_experience <- factor(dd$years_experience, ordered = FALSE)

output <- run_logit(dd, vars, 'teacher_use', 'normal')
teacher_use_results <- output$table

a <- lm("teacher_use~ Children_age", dd)
summary(a)
a <- lm(as.formula(paste("teacher_use", "~", paste("type_of_school", collapse = "+"))), dd) 
t.test(teacher_use ~ type_of_school, data = dd)

table_gender_usage <- table(dd$type_of_school, dd$teacher_use)
chisq.test(table_gender_usage[1:2,])

## How teachers access  ####
dd <- df %>% filter(gender %in% c('Female', 'Male'),teacher_use==1)
table(dd$system_access)
dd$system_access <- ifelse(dd$system_access %in% "My school provides access to it through an institutional license", 1, 0)
vars <- c("Teacher_age", "gender", "geo_info", "type_of_school","additional_needs", "working_hours", 
          "Children_age", "years_experience", job_titles, subject_areas)
a <- lm("system_access~ type_of_school", dd)
output <- run_logit(dd, vars, 'system_access', 'normal')
teacher_access_results <- output$table

### students use ####
vars <- c("Teacher_age", "gender", "geo_info", "type_of_school", "additional_needs", "working_hours", "type_of_school", "Children_age", "teacher_use")
df$student_use <- ifelse(grepl("I am aware of students", df$use_of_gen_ai), 1, 0)
dd <- df %>% filter(gender %in% c('Female', 'Male'), type_of_school %in% c("Private", "State"))
output <- run_logit(dd, vars, 'student_use', 'normal')
children_use_results <- output$table

use_result <- merge(teacher_use_results,children_use_results, by = "coef_names", all = TRUE)
addWorksheet(OUT, "AI_use")
writeData(OUT, sheet = "AI_use", x = use_result)

## Teachers' use case ####
questions <- c("Designing_exams", "designing_homework_assignments", "Marking_exams", "Student_feedback", 
                "Lesson_planning_and_research", "Developing_personalised_learning_plans_for_students", 
                "Generating_educational_content_for_classroom_presentations", 
                "Responding_to_parents'_emails", "Other")
job_titles <- c("Teaching_assistant", "Primary_school_teacher", "Secondary_school_teacher", "Special_education_needs", "Headteacher")
subject_areas <- c("English", "Maths", "Science", "Design_and_technology", "History", "Geography", "Art_and_design", "Music", 
                   "Physical_education", "Ancient_and_modern_foreign_languages", "Computing")

vars <- c("Teacher_age", "gender", "type_of_school", "working_hours", "type_of_school", "Children_age", job_titles[2:5], subject_areas)

dd <- df %>% filter(teacher_use==1) %>%
  mutate(across(all_of(questions), ~ ifelse(. == "Yes", 1, 0)))
dd <- dd %>% filter(type_of_school %in% c('Private', 'State'))
dd$type_of_school <- as.character(dd$type_of_school)  # Remove ordering
dd <- dd %>% filter(gender %in% c('Female', 'Male'))
teachers_use_case <- data.frame()
for (i in 1:7)
{
  question <- questions[i]
  formula <- as.formula(paste(question, "~", paste(vars, collapse = "+"))) 
  output <- run_logit(dd, vars, question)
  result <- output$table
  #model <- glm(formula, data = dd , family = quasibinomial())
  #vif(model)
  if (i==1)
    teachers_use_case <- result
  else
    teachers_use_case <- merge(teachers_use_case,result, by = "coef_names", all = TRUE)
}
addWorksheet(OUT, "Teachers_use_case")
writeData(OUT, sheet = "Teachers_use_case", x = teachers_use_case)


### Teachers' statements for generative AI users ####
#df <- df %>% mutate(system_sum = rowSums(across(all_of(system_lists), ~.=="Yes")))
statements <- c("I feel confident using the system", "The system has increased my productivity", 
                "The system has had a positive impact on my teaching", "I trust the outputs of the system")
system_lists <- c( "CoPilot",  "Gemini","Other")

questions <- c('statement_1', 'statement_2', 'statement_3', 'statement_4')
dd <- df %>% filter(teacher_use==1) %>%  dplyr:: mutate(across(all_of(questions), 
                                    ~ case_when(
                                      .%in% c("Strongly agree")   ~ 2,
                                      .%in% c("Agree")  ~ 1,
                                      .%in% c("Neither agree nor disagree")  ~ 0,
                                      .%in% c("Disagree")  ~ -1,
                                      .%in% c("Strongly disagree")  ~ -2,
                                      TRUE ~ 0
                                    )))
dd <- dd %>% filter(type_of_school %in% c('State', 'Private'))
dd$type_of_school <- as.character(dd$type_of_school)  # Remove ordering
#dd$type_of_school <- factor(dd$type_of_school, levels = c("State", "Private"))
dd <- dd %>% filter(gender %in% c('Female', 'Male'))
# str(dd$type_of_school)

job_titles <- c("Teaching_assistant", "Primary_school_teacher", "Secondary_school_teacher", "Headteacher", "Special_education_needs")
dd$Teacher_age <- factor(dd$Teacher_age, ordered = FALSE)
a <- lm("statement_1~ gender + type_of_school + Teacher_age + working_hours + 
        CoPilot + Gemini + Teaching_assistant + Primary_school_teacher + Secondary_school_teacher + Headteacher ", dd)


vars <- c("gender", "type_of_school", "Teacher_age", "working_hours",job_titles[1:4], system_lists)
dd <- dd %>% mutate(across(all_of(questions), ~ factor(.x, ordered = TRUE)))
all_result <- data.frame()
for (i in 1:4)
{
  question <- statements[i]
  statement_mixed <- paste0("statement_",i)
  formula <- as.formula(paste(statement_mixed, "~", paste(vars, collapse = "+"))) 
  result <- run_ordered_logit(dd, formula, question)
  if (i==1)
    all_result <- result
  else
    all_result <- merge(all_result,result, by = "coef_names", all = TRUE)
}
write.xlsx(all_result, "../../Data/Teacher_statement_regressions.xlsx")
addWorksheet(OUT, "AI_users_statements")
writeData(OUT, sheet = "AI_users_statements", x = all_result)
saveWorkbook(OUT,  "../../Data/Teachers_Regressions.xlsx")

### Teachers' statements for all teachers ####
statements <- c("Generative AI should be more widely used in the classroom by teachers", 
                "I believe I would be able to tell if a student submitted work that had been made with generative AI", 
                "Generative AI makes me worried about my job security", 
                "Generative AI can help me enhance the skills I have", 
                "Generative AI can be used to make the process of marking student work more fair", 
                "Generative AI can reduce the amount of time teachers spend working overtime")
questions <- c('teacher_statements_1', 'teacher_statements_2', 'teacher_statements_3', 'teacher_statements_4', 'teacher_statements_5', 'teacher_statements_6')
dd <- df %>% dplyr:: mutate(across(all_of(questions), ~ case_when(
                                                                 .%in% c("Strongly agree")   ~ 2,
                                                                 .%in% c("Agree")  ~ 1,
                                                                 .%in% c("Neither agree nor disagree")  ~ 0,
                                                                 .%in% c("Disagree")  ~ -1,
                                                                 .%in% c("Strongly disagree")  ~ -2,
                                                                 TRUE ~ 0
                                                               )))

dd <- dd %>% filter(type_of_school %in% c('State', 'Private'))
dd$type_of_school <- as.character(dd$type_of_school)  # Remove ordering
#dd$type_of_school <- factor(dd$type_of_school, levels = c("State", "Private"))
dd <- dd %>% filter(gender %in% c('Female', 'Male'))
# str(dd$type_of_school)

job_titles <- c("Teaching_assistant", "Primary_school_teacher", "Secondary_school_teacher", "Headteacher", "Special_education_needs")
dd$Teacher_age <- factor(dd$Teacher_age, ordered = FALSE)
a <- lm("teacher-statements_1~ gender + type_of_school + Teacher_age + working_hours + 
        CoPilot + Gemini + Teaching_assistant + Primary_school_teacher + Secondary_school_teacher + Headteacher ", dd)


vars <- c("gender", "type_of_school", "Teacher_age", "working_hours",job_titles[1:4], 'teacher_use')
dd <- dd %>% mutate(across(all_of(questions), ~ factor(.x, ordered = TRUE)))
all_result <- data.frame()
for (i in 1:6)
{
  question <- statements[i]
  statement_mixed <- paste0("teacher_statements_",i)
  formula <- as.formula(paste(statement_mixed, "~", paste(vars, collapse = "+"))) 
  result <- run_ordered_logit(dd, formula, question)
  if (i==1)
    all_result <- result
  else
    all_result <- merge(all_result,result, by = "coef_names", all = TRUE)
}
write.xlsx(all_result, "../../Data/teacher_statments_Regressions_all.xlsx")

### Teachers' overall feelings ####
feelings <- c("Optimistic", "Confident", "Curious", "Neutral", "Cautious", "Uncertain", "Sceptical", "Concerned", "Overwhelmed")
feelings_df <- data.frame()
dd <- df %>% filter(gender %in% c('Female', 'Male'), type_of_school %in% c("Private", "State"))
dd$Children_age <- factor(dd$Children_age, ordered = FALSE)
dd$Teacher_age <- factor(dd$Teacher_age, ordered = FALSE)
dd$years_experience <- factor(dd$years_experience, ordered = FALSE)
vars <- c("Teacher_age", "gender", "geo_info", "type_of_school","additional_needs", "working_hours", 
          "Children_age", "years_experience", job_titles, subject_areas, "genai_at_work")
all_result <- data.frame()
for (i in 1:length(feelings))
{
  feeling <- feelings[i]
  dd[feeling] = ifelse(grepl(feeling, dd$overall_teachers),1, 0)
  formula <- as.formula(paste(feeling, "~", paste(vars, collapse = "+"))) 
  output <- run_logit(dd, vars, feeling)
  result <- output$table
  if (i==1)
    all_result <- result
  else
    all_result <- merge(all_result,result, by = "coef_names", all = TRUE)
}
write.xlsx(all_result, "../../Data/teacher_feelings_Regressions.xlsx")

### Teachers' task statements ####
vars <- c("Teacher_age", "gender", "geo_info", "type_of_school","additional_needs", "working_hours", 
          "Children_age", "years_experience", job_titles, subject_areas, "genai_at_work")
statements <- c("Preparing lesson plans", 
                "Delivering lessons to students", 
                "Developing educational content to meet the needs of different learners", 
                "Assessing student performance", 
                "Maintaining a positive learning environment", 
                "Guiding students on academic and personal development", 
                "Engaging with parents, other teachers, and school administrators to support student learning", 
                "Keeping up to date with subject knowledge and teaching methods")
all_result <- data.frame()
for (i in 1:8){
  statement_impact <- paste0("statements_tasks_",i)
  dd <- df %>% filter(!is.na(.data[[statement_impact]])) 
  dd[statement_impact] = ifelse(dd[statement_impact]=="Replace teachers in performing the task",1, 0)
  formula <- as.formula(paste(statement_impact, "~", paste(vars, collapse = "+"))) 
  output <- run_logit(dd, vars,statement_impact)
  result <- output$table
  if (i==1)
    all_result <- result
  else
    all_result <- merge(all_result,result, by = "coef_names", all = TRUE)
}

### Students' use cases ####
vars <- c("type_of_school", "Children_age", "teacher_use", subject_areas)
df$student_use <- ifelse(grepl("I am aware of students", df$use_of_gen_ai), "Yes", "No") 
student_cases <- c("Research during class time", "Research at home", 
                   "Developing and drafting ideas to help them get started on an assignmen", 
                   "Writing and submitting AI", 
                   "Doing their exams", "Other", "I'm not sure")
students_use_case <- data.frame()
for (i in 1: 5)
{
  case <- student_cases[i]
  df[case] <- ifelse(grepl(case, df$gen_ai_branch), 1, 0)
  dd <- df %>% filter(student_use %in% 'Yes', 
                      gender %in% c('Female', 'Male'), type_of_school %in% c("Private", "State"))
  colnames(dd) <- gsub(" ", "_", colnames(dd))
  case <- gsub(" ", "_",case)
  formula <- as.formula(paste(case, "~", paste(vars, collapse = "+"))) 
  output <- run_logit(dd, vars, case)
  result <- output$table
  if (i==1)
    students_use_case <- result
  else
    students_use_case <- merge(students_use_case,result, by = "coef_names", all = TRUE)
}
write.xlsx(students_use_case, "../../Data/student_use_cases_regressions.xlsx")

### Assigned work ####
vars <- c("type_of_school", "Children_age", "teacher_use", subject_areas)
dd <- df %>% filter(student_use %in% 'Yes', 
                    gender %in% c('Female', 'Male'), type_of_school %in% c("Private", "State"))
dd$assigned_work <- ifelse(dd$assigned_work %in% 'Yes', 1, 0)
formula <- as.formula(paste("assigned_work", "~", paste(vars, collapse = "+"))) 
output <- run_logit(dd, vars, "assigned_work")
result <- output$table

## Effects on students for AI users ####
table(df %>% filter(impact_work %in% 'Yes') %>% dplyr::select(statements_impact_1), useNA = 'always')
statements <- c("Generative AI has had a positive impact on the creativity of students' work", 
                "Generative AI has made the ideas students are submitting less diverse", 
                "Generative AI has had a negative impact on students' engagement in classwork")
statement_impact <- c("statements_impact_1", "statements_impact_2", "statements_impact_3")
dd <- df %>% filter(impact_work %in% 'Yes') %>%  dplyr:: mutate(across(all_of(statement_impact), 
                                                               ~ case_when(
                                                                 .%in% c("Strongly agree")   ~ 1,
                                                                 .%in% c("Agree")  ~ 1,
                                                                 .%in% c("Neither agree nor disagree", "Don't know")  ~ 0,
                                                                 .%in% c("Disagree")  ~ -1,
                                                                 .%in% c("Strongly disagree")  ~ -1,
                                                                 TRUE ~ 0
                                                               )))
dd <- dd %>% mutate(across(all_of(statement_impact), ~ factor(.x, ordered = TRUE)))
vars <- c("type_of_school", "Children_age", "teacher_use", subject_areas)
all_result <- data.frame()
dd <- dd %>% filter(gender %in% c('Female', 'Male'), type_of_school %in% c("Private", "State"))
for (i in 1:3)
{
  question <- statements[i]
  statement_mixed <- paste0("statements_impact_",i)
  formula <- as.formula(paste(statement_mixed, "~", paste(vars, collapse = "+"))) 
  result <- run_ordered_logit(dd, formula, question)
  if (i==1)
    all_result <- result
  else
    all_result <- merge(all_result,result, by = "coef_names", all = TRUE)
}
OUT <- createWorkbook()
addWorksheet(OUT, "AI_user_statements")
writeData(OUT, sheet = "AI_user_statements", x = all_result)

## Effects on students for All students ####
statements <- c("I am worried generative AI will negatively impact students' critical thinking skills", 
                "Generative AI can help foster students' creativity", 
                "I am worried that generative AI might limit the level of engagement teachers have with students", 
                "I am worried about the impact generative AI may have on children's wellbeing", 
                "Generative AI is a great tool to support students with additional needs")
statement_impact <- c("student_statements_1", "student_statements_2", "student_statements_3", "student_statements_4", "student_statements_5")
df$student_use <- ifelse(grepl("I am aware of students", df$use_of_gen_ai), "Yes", "No")
dd <- df %>% dplyr:: mutate(across(all_of(statement_impact), 
                         ~ case_when(
                           .%in% c("Strongly agree")   ~ 2,
                           .%in% c("Agree")  ~ 1,
                           .%in% c("Neither agree nor disagree", "Don't know")  ~ 0,
                           .%in% c("Disagree")  ~ -1,
                           .%in% c("Strongly disagree")  ~ -2,
                           TRUE ~ 0
                         )))
dd <- dd %>% mutate(across(all_of(statement_impact), ~ factor(.x, ordered = TRUE)))
vars <- c("type_of_school", "Children_age", "teacher_use", "", subject_areas, "gender", "student_use")
all_result <- data.frame()
dd <- dd %>% filter(gender %in% c('Female', 'Male'), type_of_school %in% c("Private", "State"))
for (i in 1:5)
{
  question <- statements[i]
  statement_mixed <- paste0("student_statements_",i)
  formula <- as.formula(paste(statement_mixed, "~", paste(vars, collapse = "+"))) 
  result <- run_ordered_logit(dd, formula, question)
  if (i==1)
    all_result <- result
  else
    all_result <- merge(all_result,result, by = "coef_names", all = TRUE)
}

addWorksheet(OUT, "All_statements")
writeData(OUT, sheet = "All_statements", x = all_result)
saveWorkbook(OUT,  "../../Data/Students_Regressions.xlsx")
