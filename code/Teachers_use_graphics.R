#Library imports
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(scales)
library(tidyr)
library(hash)
library(rlang)
library(grid)
library(openxlsx)

## Function definitions and global variables ####
polished_graph <- function(df, demo, x_title, plot_title)
{
  custom_colors <-c("#1F9C92", "#FF6F32", "#897FCB", "#FFC72C", "#3F88C5", "#BBBBBB", "pink","red", "purple", "green" , "#335511", "#44FFDD")
  p <- ggplot(df, aes(x = reorder(str_wrap(variable, width = 40), freq), y = freq*100, fill = value)) +
    geom_bar(stat = 'identity', position = 'dodge', width = 0.6, color = "black") +  # Add borders to bars
    scale_fill_manual(values = custom_colors)  +  # More appealing colors
    # Adjust text label position & size
    geom_text(aes(label = paste0(round(freq * 100), "%")),
              position = position_dodge(width = 0.5), vjust = 0.5, hjust = 1,
              size = 4.5, color = "black", fontface = "bold") + 
    # Labels & Titles
    labs(
      fill = str_wrap(demo,width = 20),
      title = str_wrap(plot_title, width = 30),  # Wrapping title for better display
      x = NULL,  # Remove x-axis label
      y = str_wrap(x_title, width = 50)) +
    theme_minimal() +  # Cleaner background with larger base text size
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"),  # Emphasize title
      plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
      axis.text = element_text(size = 14, color = "black", face = "bold"),
      axis.title = element_text(size = 16, face = 'bold'),
      strip.text = element_text(size = 14, face = 'bold'),
      legend.title =  element_text(size = 16, face = 'bold'),
      legend.text = element_text(size = 16)
    )
  return (p)
}

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
  df[df$age %in% "60 years and over", 'age'] <- "60+"
  df$age <- factor(df$age, levels = c("Under 30", "30-34", "35-39", "40-44", "45-49", "50-59", "60+"), ordered = TRUE)
  df <- df %>% arrange(age)
  names(df)[names(df)=='age'] <- "Teacher-age"
  
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
  
  
  return (df)
}

df <- read.xlsx("../Data/confidential/Understanding teachers' perspectives on AI - Final dataset.xlsx")
df <- df[2:nrow(df),]
df <- clean_data(df)
#### Demographic information ####
all_demographcis <- c("Teacher-age", "gender", "geo-info", "school-language", "type-of-school", "job-title", "age-range", "subject-area", 
                      "additional-needs", "years-experience", "working-hours", "teaching-time")

useful_demographics <- c("Teacher-age", "gender", "type-of-school", "Children-age", 
                         "additional-needs", "years-experience", "working-hours")

other_demographics <- c("job-title","subject-area")
job_titles <- c("Teaching assistant", "Primary school teacher", "Secondary school teacher", "Special education needs", "Headteacher", "Other")
subject_areas <- c("English", "Maths", "Science", "Design and technology", "History", "Geography", "Art and design", "Music", 
                   "Physical education", "Computing", "Ancient and modern foreign languages", "Other")

dd <- df %>% select(all_of(c(useful_demographics, "IPAddress"))) %>% reshape2::melt("IPAddress") %>% 
  group_by(variable, value) %>% summarise(n = n()) %>% group_by(variable) %>% mutate(freq = round(n/sum(n)*100))

for (job in job_titles)
{
  df[job] <- ifelse(grepl(job, df$`job-title`), 'Yes', 'No')
  dd <- rbind(dd, data.frame(variable = "job-title", value = job,  n = 
                               sum(grepl(job, df$`job-title`)), freq =  round(100*sum(grepl(job, df$`job-title`))/1001) ))
}
for (subject in subject_areas)
{
  df[subject] <- ifelse(grepl(subject, df$`subject-area`), "Yes", "No")
  dd <- rbind(dd, data.frame(variable = "subject-area", value = subject,  n = 
                               sum(grepl(subject, df$`subject-area`)), freq =  round(100*sum(grepl(subject, df$`subject-area`))/1001) )) 
}

## Table 1 - AI use and awareness - graphics - school type, age-range, geo-info ####
use_lists <- c("Before today, I had not heard of generative AI", 
  "I use generative AI in my personal time outside of work", 
  "I am aware of colleagues using generative AI in their work", 
  "I am aware of students", "None")
gen_ai_use <- data.frame()
for (use_type in use_lists)
  gen_ai_use <- rbind(gen_ai_use, data.frame(question = use_type, n =  sum(grepl(use_type, df$`use-of-gen-ai`)), 
                                             freq = round(sum(grepl(use_type, df$`use-of-gen-ai`))/1001*100) ))

gen_ai_use <- rbind(gen_ai_use, data.frame(question = "I use generative AI in my work", n = table(df$`genai-at-work`)['Yes'], 
                                           freq = round(table(df$`genai-at-work`)['Yes']/1001*100)))

write.xlsx(gen_ai_use, "../../Data/gen_ai_use.xlsx")


## Table 2 - For those who used AI, graphics- which systems they used? - free text analysis later for others. ####
system_lists <- c("ChatGPT", "Dall-E", "Midjourney", "Claude", "Perplexity", "Gemini", "CoPilot", "DeepSeek", "Other")
gen_ai_systems_demo <- data.frame()
gen_ai_systems <- data.frame()
for (system in system_lists)
{
  temp <- data.frame(question = system, n =  sum(grepl(system, df$`systems-used`)), 
                     freq = round(sum(grepl(system, df$`systems-used`))/sum(!is.na(df$`systems-used`))*100))
  gen_ai_systems <- rbind(gen_ai_systems, temp)
  for (demo in useful_demographics)
  {
    df[system] = ifelse(grepl(system, df$`systems-used`),'Yes', 'No')
    temp <-  df %>% group_by_at(c(demo,system)) %>% summarise(n = n()) %>% group_by_at(demo) %>%
      mutate(freq = round(n/sum(n)*100))
    temp$variable <- demo
    names(temp)[names(temp) == demo] <- "value"
    names(temp)[names(temp) == system] <- "tt"
    temp$question <- system
    gen_ai_systems_demo <- rbind(gen_ai_systems_demo, temp %>% filter(tt %in% 'Yes') %>% select(-tt))
    
  }
}

#if they are using more than one system - which system they use most often? 
gen_ai_systems <- df%>% group_by(`multiple-systems`) %>% summarise(n_most_often = n()) %>% filter(!is.na(`multiple-systems`)) %>% 
  mutate(freq_most_often = round(n_most_often/sum(n_most_often)*100)) %>% 
  rename(question = `multiple-systems`) %>% merge(gen_ai_systems, by = "question")

gen_ai_systems <- arrange(gen_ai_systems, -freq)
write.xlsx(gen_ai_systems, "../../Data/gen_ai_systems.xlsx")
## Table 4 - Demographics for Students use ####
df$student_use <- ifelse(grepl("I am aware of students", df$`use-of-gen-ai`), "Yes", "No") 
gen_ai_use_df <- data.frame(n = table(df$student_use)['Yes'], 
                            freq = round(table(df$student_use)['Yes']/1001*100), variable = "all", value = "all")
p <- polished_graph(gen_ai_use_df, "all","","")
plot_list <- list(p)
i <- 2
for (demo in c("genai-at-work", "type-of-school","Children-age"))
{
  temp <-  df %>% group_by_at(c(demo,'student_use')) %>% summarise(n = n()) %>% group_by_at(demo) %>%
    mutate(freq = round(n/sum(n)*100)) %>% filter(student_use %in% 'Yes', n > 10) %>% dplyr::select(-student_use)
  temp$variable <- demo
  names(temp)[names(temp) == demo] <- "value"
  gen_ai_use_df <- rbind(gen_ai_use_df, temp)
  p <- polished_graph(temp,demo, "","")
  plot_list[[i]] <- p 
  i <- (i+1)
}
grid_plots <- do.call(arrangeGrob, c(plot_list))
a <- grid.arrange(grid_plots,top = textGrob(str_wrap("I am aware of students using generative AI for schoolwork", width = 70), 
                                            gp = gpar(fontsize = 18, fontface = "bold")))
students_use_df_subjects <- data.frame()
subject_areas <- c("English", "Maths", "Science", "Design and technology", "History", "Geography", "Art and design", "Music", 
                   "Physical education", "Computing", "Ancient and modern foreign languages", "Other")
for (subject in subject_areas)
{
  temp <- df %>% filter(.data[[subject]]%in% 'Yes') %>% group_by_at(c(subject,'student_use')) %>% summarise(n = n()) %>% group_by_at(subject) %>%
    mutate(freq = round(n/sum(n)*100))  %>%  filter(student_use %in% 'Yes', n > 10) %>% dplyr::select(-student_use)
  temp[temp[subject]%in% 'Yes', subject] <- subject
  names(temp)[names(temp) == subject] <- "value"
  temp$variable <- 'subject-area'
  students_use_df_subjects <- rbind(students_use_df_subjects, temp)
}
gen_ai_use_df <- rbind(gen_ai_use_df, students_use_df_subjects)


p <- polished_graph(students_use_df_subjects,"subject-area", ""
                    ,"I am aware of students using generative AI for schoolwork")
ggsave("../../Figures/Children/gen_ai_use_children_3.jpg", plot = p, height = 4, width = 10)
ggsave("../../Figures/Teachers/gen_ai_use_children_1.jpg", plot = a, height = 9, width = 12)


## Figure 1 - Demographics for Teachers' use ####

gen_ai_use_df <- data.frame(question = "I use generative AI in my work", n = table(df$`genai-at-work`)['Yes'], 
                            freq = table(df$`genai-at-work`)['Yes']/1001, variable = "all", value = "all")
p <- polished_graph(gen_ai_use_df, "all","","")
plot_list <- list(p)
i <- 2
for (demo in c("gender", "Children-age"))
{
  temp <-  df %>% group_by_at(c(demo,'genai-at-work')) %>% summarise(n = n()) %>% group_by_at(demo) %>%
    mutate(freq = n/sum(n)) %>% filter(`genai-at-work` %in% 'Yes', n > 10) %>% select(-`genai-at-work`)
  temp$variable <- demo
  names(temp)[names(temp) == demo] <- "value"
  temp$question <- "I use generative AI in my work"
  gen_ai_use_df <- rbind(gen_ai_use_df, temp)
  p <- polished_graph(temp,demo, "","")
  plot_list[[i]] <- p 
  i <- (i+1)
}
grid_plots <- do.call(arrangeGrob, c(plot_list))
a <- grid.arrange(plot_list[[1]],plot_list[[2]], plot_list[[3]], nrow = 1,  top = textGrob(str_wrap("I use generative AI in my work", width = 70), 
                                            gp = gpar(fontsize = 18, fontface = "bold")))
ggsave("../../Figures/Teachers/gen_ai_use_teacher_1.jpg", plot = a, height = 9, width = 12)


i <- 1
plot_list <- list()
for (demo in c("Teacher-age", "years-experience"))
{
  temp <-  df %>% group_by_at(c(demo,'genai-at-work')) %>% summarise(n = n()) %>% group_by_at(demo) %>%
    mutate(freq = n/sum(n)) %>% filter(`genai-at-work` %in% 'Yes', n > 10) %>% select(-`genai-at-work`)
  temp$variable <- demo
  names(temp)[names(temp) == demo] <- "value"
  temp$question <- "I use generative AI in my work"
  gen_ai_use_df <- rbind(gen_ai_use_df, temp)
  p <- polished_graph(temp,demo, "","")
  plot_list[[i]] <- p 
  i <- (i+1)
}
grid_plots <- do.call(arrangeGrob, c(plot_list))
a <- grid.arrange(grid_plots,top = textGrob(str_wrap("I use generative AI in my work", width = 70), 
                                            gp = gpar(fontsize = 18, fontface = "bold")))
ggsave("../../Figures/Teachers/gen_ai_use_teacher_2.jpg", plot = a, height = 5, width = 10)

##job title and subjects. 
gen_ai_use_df_jobs <- data.frame()
for (job in job_titles)
{
  df[job] <- ifelse(grepl(job, df$`job-title`), 'Yes', 'No')
  temp <- df %>% filter(.data[[job]]%in% 'Yes') %>% group_by_at(c(job,'genai-at-work')) %>% summarise(n = n()) %>% group_by_at(job) %>%
    mutate(freq = n/sum(n)) %>%  filter(`genai-at-work` %in% 'Yes', n > 10) %>% select(-`genai-at-work`)
  temp[temp[job]%in% 'Yes', job] <- job
  temp$variable <- 'job-title'
  names(temp)[names(temp) == job] <- "value"
  gen_ai_use_df_jobs <- rbind(gen_ai_use_df_jobs, temp)
  
}

gen_ai_use_df_subjects <- data.frame()
for (subject in subject_areas)
{
  df[subject] <- ifelse(grepl(subject, df$`subject-area`), "Yes", "No")
  temp <- df %>% filter(.data[[subject]]%in% 'Yes') %>% group_by_at(c(subject,'genai-at-work')) %>% summarise(n = n()) %>% group_by_at(subject) %>%
    mutate(freq = n/sum(n)) %>%  filter(`genai-at-work` %in% 'Yes', n > 10) %>% select(-`genai-at-work`)
  temp[temp[subject]%in% 'Yes', subject] <- subject
  temp$variable <- 'subject-area'
  names(temp)[names(temp) == subject] <- "value"
  gen_ai_use_df_subjects <- rbind(gen_ai_use_df_subjects, temp)
}
p1 <- polished_graph(gen_ai_use_df_jobs,'job-title', "","")
p2 <- polished_graph(gen_ai_use_df_subjects,'subject-area', "","")
a <- grid.arrange(p1, p2,top = textGrob(str_wrap("I use generative AI in my work", width = 70), 
                                            gp = gpar(fontsize = 18, fontface = "bold")))
ggsave("../../Figures/Teachers/gen_ai_use_teacher_3.jpg", plot = a, height = 6, width = 12)


## Figure 2 - Teachers' use cases ####
cases_list <- c("Designing exams", "designing homework assignments", "Marking exams", "Student feedback", 
                "Lesson planning and research", "Developing personalised learning plans for students", 
                "Generating educational content for classroom presentations", 
                "Responding to parents' emails", "Other")
use_cases_df <- data.frame()
teacher_df <- data.frame()
for (case in cases_list)
  teacher_df <- rbind(teacher_df, data.frame(variable = case, value = sum(grepl(case, df$`activities-genai`)), 
                                             freq = sum(grepl(case, df$`activities-genai`))/sum(!is.na(df$`activities-genai`))))


custom_colors <- c(
  "#897FCB",    # Soft Mustard Yellow (warm but not too bright)
  "#1F9C92",     # Muted Blue (trustworthy & strong)
  "#FF6F32",       # Soft Olive Green (exploratory & natural)
  "#FFC72C",       # Light Gray (balanced & neutral)
  "#3F88C5",      # Muted Orange (gentle alertness)
  "#D6A883",     # Soft Brownish Orange (hesitant, warm)
  "#BBBBBB",     # Muted Reddish-Brown (doubtful but subtle)
  "#B56576",     # Soft Rose (mild concern)
  "#886F83"    # Dusty Purple (deep emotion but not harsh)
)

ggplot(teacher_df, aes(x = reorder(str_wrap(variable, width = 40), freq), y = freq, fill = variable)) +
  geom_bar(stat = 'identity', position = 'stack', fill = "dodgerblue4", alpha = 0.7, width = 0.6) +
  scale_y_continuous(labels = percent_format()) + scale_fill_manual(values = custom_colors) +
  # text label position & size
  geom_text(aes(label = paste0(round(freq* 100), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +  
  # Labels & Titles
  labs(
    title = str_wrap("You have said that you are aware of students using generative AI for their
schoolwork. What specifically are they using it for?", 
                     width = 60),
    x = "",
    y = "% of children using Generative AI (n = 397)"
  ) +  
  coord_flip() + 
  # Theme
  theme_light() + 
  theme(
    strip.text = element_text(size = 14, face = 'bold'),
    axis.text = element_text(size = 15, face = 'bold', color = "black"),
    plot.title = element_text(hjust = 0,size = 16, face = "bold", color = "darkblue"),
    axis.title.x = element_text(size = 15, face = 'bold')
  )

## Figure 8 - Students' use cases ####
student_cases <- c("Research during class time", "Research at home", 
                   "Developing and drafting ideas to help them get started on an assignmen", 
                   "Writing and submitting AI-generated work as their own", 
                   "Doing their exams", "Other", "I'm not sure")
student_df <- data.frame()
for (case in student_cases)
  student_df <- rbind(student_df, data.frame(variable = case, value = sum(grepl(case, df$`gen-ai-branch`)), 
                                             freq = sum(grepl(case, df$`gen-ai-branch`))/sum(!is.na(df$`gen-ai-branch`))))


custom_colors <- c(
  "#897FCB",    # Soft Mustard Yellow (warm but not too bright)
  "#1F9C92",     # Muted Blue (trustworthy & strong)
  "#FF6F32",       # Soft Olive Green (exploratory & natural)
  "#FFC72C",       # Light Gray (balanced & neutral)
  "#3F88C5",      # Muted Orange (gentle alertness)
  "#D6A883",     # Soft Brownish Orange (hesitant, warm)
  "#BBBBBB",     # Muted Reddish-Brown (doubtful but subtle)
  "#B56576",     # Soft Rose (mild concern)
  "#886F83"    # Dusty Purple (deep emotion but not harsh)
)

ggplot(student_df, aes(x = reorder(str_wrap(variable, width = 40), freq), y = freq, fill = variable)) +
  geom_bar(stat = 'identity', position = 'stack', fill = "dodgerblue4", alpha = 0.7, width = 0.6) +
  scale_y_continuous(labels = percent_format()) + scale_fill_manual(values = custom_colors) +
  # text label position & size
  geom_text(aes(label = paste0(round(freq* 100), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +  
  # Labels & Titles
  labs(
    title = str_wrap("You have said that you are aware of students using generative AI for their
schoolwork. What specifically are they using it for?", 
                     width = 60),
    x = "",
    y = "% of children using Generative AI (n = 397)"
  ) +  
  coord_flip() + 
  # Theme
  theme_light() + 
  theme(
    strip.text = element_text(size = 14, face = 'bold'),
    axis.text = element_text(size = 15, face = 'bold', color = "black"),
    plot.title = element_text(hjust = 0,size = 16, face = "bold", color = "darkblue"),
    axis.title.x = element_text(size = 15, face = 'bold')
  )
ggsave("../../Figures/Students/Ai_usecases_students.jpg", width = 12, height = 6, dpi = 300)



## Figure 9 - Students' use cases for public vs private schools ####
use_cases_df <- data.frame()
for (case in student_cases)
{
  df[case] <- ifelse(grepl(case, df$`gen-ai-branch`), 'Yes', "No")
  
  dd <- df %>% filter(student_use %in% 'Yes')
  demo <- "type-of-school"
  temp <-  dd %>% group_by_at(c(demo,case)) %>% summarise(n = n()) %>% group_by_at(demo) %>%
    mutate(freq = n/sum(n)) %>% filter(.data[[case]] %in% 'Yes', n > 10) 
  names(temp)[names(temp) == demo] <- "value"
  temp$variable <- case
  use_cases_df <- rbind(use_cases_df, temp %>% dplyr::select(value, variable, freq, n))
}
use_cases_df <- use_cases_df %>% filter(value %in% c("State", "Private"))
polished_graph(use_cases_df, "School Type", "", "") + coord_flip()
ggsave("../../Figures/Students/use_case_public_private.jpg", height = 5, width = 10)


## How they access the systems- for those who use AI at work- whether their school has subscription? ####

## All 
school_internet <- df %>% group_by(`school-internet`) %>% summarise(n = n())%>% mutate(freq = round(n/sum(n)*100))
names(school_internet)[names(school_internet)=="school-internet"] <- "variable"
school_internet$question <- "Does your school have internet?"

school_awareness <- df %>% filter(`system-access` %in% 'Through a personal account that is not affiliated with my school') %>% 
  group_by(`school-awareness`) %>% summarise(n = n()) %>% mutate(freq = round(n/sum(n)*100))
names(school_awareness)[names(school_awareness)=="school-awareness"] <- "variable"
school_awareness$question <- "Is your school aware that you are using generative AI for work?"

student_mobile <- df %>% group_by(`student-mobile`) %>% summarise(n = n())  %>% mutate(freq = round(n/sum(n)*100))
student_mobile$question <- "Are students allowed mobile phone access during the school day?"
names(student_mobile)[names(student_mobile)=="student-mobile"] <- "variable"

system_access <- df %>% filter(`genai-at-work` %in% 'Yes') %>% group_by(`system-access`) %>% 
  summarise(n = n()) %>% mutate(freq = round(100*n/sum(n)))
system_access$question <- "How are you accessing the system for work?"
names(system_access)[names(system_access)=="system-access"] <- "variable"

a <- rbind(school_internet %>% arrange(by = -freq), school_awareness %>% arrange(by = -freq),
           student_mobile %>% arrange(by = -freq) , system_access %>% arrange(by = -freq))
write.xlsx(a, "../../Data/internet.xlsx")

## school types
school_internet <- df %>% group_by(`school-internet`,`type-of-school`) %>% summarise(n = n()) %>% 
  group_by(`type-of-school`) %>% mutate(freq = n/sum(n)) %>% filter(!is.na(`school-internet`))
school_internet$question <- "Does your school have internet?"
names(school_internet)[names(school_internet)=="school-internet"] <- "variable"
names(school_internet)[names(school_internet)=="type-of-school"] <- "value"

student_mobile <- df %>% group_by(`student-mobile`,`type-of-school`) %>% summarise(n = n()) %>% 
  group_by(`type-of-school`) %>% mutate(freq = n/sum(n))
student_mobile$question <- "Are students allowed mobile phone access during the school day?"
names(student_mobile)[names(student_mobile)=="student-mobile"] <- "variable"
names(student_mobile)[names(student_mobile)=="type-of-school"] <- "value"

school_awareness <- df %>% filter(`system-access` %in% 'Through a personal account that is not affiliated with my school') %>% 
  group_by(`school-awareness`) %>% group_by(`school-awareness`,`type-of-school`) %>% summarise(n = n()) %>% 
  group_by(`type-of-school`) %>% mutate(freq = n/sum(n))
school_awareness$question <- "Is your school aware that you are using generative AI for work?"
names(school_awareness)[names(school_awareness)=="school-awareness"] <- "variable"
names(school_awareness)[names(school_awareness)=="type-of-school"] <- "value"

system_access <- df %>% filter(`genai-at-work` %in% 'Yes') %>% group_by(`system-access`,`type-of-school`) %>% summarise(n = n()) %>% 
  group_by(`type-of-school`) %>% mutate(freq = n/sum(n))
system_access$question <- "How are you accessing the system for work?"
names(system_access)[names(system_access)=="system-access"] <- "variable"
names(system_access)[names(system_access)=="type-of-school"] <- "value"

p1 <- polished_graph(school_internet, "type-of-school", "", "Does your school have internet?") + 
  coord_flip() + theme(legend.position = "top", plot.title = element_text( size = 15))
p2 <- polished_graph(student_mobile, "type-of-school", "", "Are students allowed mobile phone access during the school day?") + 
  coord_flip() + theme(legend.position = "none", plot.title = element_text( size = 15))
p3 <- polished_graph(school_awareness, "type-of-school", "", "Is your school aware that you are using generative AI for work?") + 
  coord_flip() + theme(legend.position = "none",  plot.title = element_text( size = 15))
p4 <- polished_graph(system_access, "type-of-school", "", "How are you accessing the system for work?") + 
  coord_flip() + theme( legend.position = "none",  plot.title = element_text( size = 15))
a <- grid.arrange(p1, p2,p3,p4, ncol = 2)
ggsave("../../Figures/Internet.jpg", plot = a, height = 7, width = 13)


## Access types in AI types in private and school systems. 
system_access <- df %>% filter(`genai-at-work` %in% 'Yes') %>% group_by(`system-access`,`type-of-school`) %>% summarise(n = n()) %>% 
  group_by(`type-of-school`) %>% mutate(freq = n/sum(n))
system_access$variable <- "How are you accessing the system for work?"
names(system_access)[names(system_access)=="system-access"] <- "value"

subscription1 <- df %>% filter(`system-access` %in% "My school provides access to it through an institutional license") %>% group_by(`genai-subscription1`,`type-of-school`) %>% summarise(n = n()) %>% 
  group_by(`type-of-school`) %>% mutate(freq = n/sum(n))
subscription1$variable <- "Please indicate all the systems your school provides you with access"
names(subscription1)[names(subscription1)=="genai-subscription1"] <- "value"


subscription2 <- df %>% filter(`system-access` != "My school provides access to it through an institutional license") %>% group_by(`genai-subscription-2`,`type-of-school`) %>% summarise(n = n()) %>% 
  group_by(`type-of-school`) %>% mutate(freq = n/sum(n))
subscription1$variable <- "Does your school have an institutional license to any generative AI systems?"
names(subscription1)[names(subscription1)=="genai-subscription-2"] <- "value"

systems2 <- df %>% filter(`genai-subscription-2` %in% "Yes") %>% group_by(`school-systems`,`type-of-school`) %>% summarise(n = n()) %>% 
  group_by(`type-of-school`) %>% mutate(freq = n/sum(n))
systems2$variable <- "Which of the following systems does your school have a license to?"
names(systems2)[names(systems2)=="school-systems"] <- "value"




### Student use cases- all demographics ####
use_cases_df <- data.frame()
for (case in student_cases)
{
  df[case] <- ifelse(grepl(case, df$`gen-ai-branch`), 'Yes', "No")
  temp <- data.frame(variable = case, value = "all", n = sum(grepl(case, df$`gen-ai-branch`)), 
                     freq = sum(grepl(case, df$`gen-ai-branch`))/sum(!is.na(df$`gen-ai-branch`)))
  use_cases_df <- rbind(use_cases_df, temp)
  temp$variable <- "all"
  p <- polished_graph(temp,"all", "","")
  i <- 2
  plot_list <- list(p)
  dd <- df %>% filter(student_use %in% 'Yes')
  for (demo in c("type-of-school", "Children-age", 
                 "additional-needs"))
  {
    temp <-  dd %>% group_by_at(c(demo,case)) %>% summarise(n = n()) %>% group_by_at(demo) %>%
      mutate(freq = n/sum(n)) %>% filter(.data[[case]] %in% 'Yes', n > 10) 
    temp$variable <- demo
    names(temp)[names(temp) == demo] <- "value"
    p <- polished_graph(temp,demo, "","")
    plot_list[[i]] <- p 
    i <- (i+1)
  }
  grid_plots <- do.call(arrangeGrob, c(plot_list))
  a <- grid.arrange(grid_plots,top = textGrob(str_wrap(case, width = 70), 
                                              gp = gpar(fontsize = 18, fontface = "bold")))
  ggsave(paste0("../../Figures/Children/",case, "1.jpg"), plot = a, height = 7, width = 9)
  students_use_df_subjects <- data.frame()
  for (subject in subject_areas)
  {
    temp <- dd %>% filter(.data[[subject]]%in% 'Yes') %>% group_by_at(c(subject,case)) %>% summarise(n = n()) %>% group_by_at(subject) %>%
      mutate(freq = n/sum(n))  %>%  filter(.data[[case]] %in% 'Yes', n > 10)
    temp[temp[subject]%in% 'Yes', subject] <- subject
    names(temp)[names(temp) == subject] <- "value"
    temp$variable <- 'subject-area'
    students_use_df_subjects <- rbind(students_use_df_subjects, temp)
  }
  p <- polished_graph(students_use_df_subjects,'subject-area', "",case)
  ggsave(paste0("../../Figures/Children/",case, "2.jpg"), plot = p, height = 4, width = 12)
}

## Assigned-work ####
dd <- df %>% filter(student_use %in% 'Yes')
temp <- data.frame(variable = "assigned-work", value = "all", n = table(dd$`assigned-work`)['Yes'], 
                   freq = table(dd$`assigned-work`)['Yes']/ 397)
temp$variable <- "all"
p <- polished_graph(temp,"all", "","")
i <- 2
plot_list <- list(p)
for (demo in c("type-of-school", "Children-age",  "additional-needs"))
{
  temp <-  dd %>% group_by_at(c(demo,'assigned-work')) %>% summarise(n = n()) %>% group_by_at(demo) %>%
    mutate(freq = n/sum(n)) %>% filter(.data[['assigned-work']] %in% 'Yes', n > 10) 
  temp$variable <- demo
  names(temp)[names(temp) == demo] <- "value"
  p <- polished_graph(temp,demo, "","")
  plot_list[[i]] <- p 
  i <- (i+1)
}
grid_plots <- do.call(arrangeGrob, c(plot_list))
a <- grid.arrange(grid_plots,top = textGrob(str_wrap("Have you assigned work to your students which asks them to use generative AI to complete?", width = 70), 
                                            gp = gpar(fontsize = 18, fontface = "bold")))
ggsave(paste0("../../Figures/Children/","assigned-work1", ".jpg"), plot = a, height = 7, width = 9)

assigned_work_df <- data.frame()
for (subject in subject_areas)
{
  temp <- dd %>% filter(.data[[subject]]%in% 'Yes') %>% group_by_at(c(subject,'assigned-work')) %>% summarise(n = n()) %>% group_by_at(subject) %>%
    mutate(freq = n/sum(n))  %>%  filter(.data[['assigned-work']] %in% 'Yes', n > 10) 
  temp[temp[subject]%in% 'Yes', subject] <- subject
  names(temp)[names(temp) == subject] <- "value"
  temp$variable <- 'subject-area'
  assigned_work_df <- rbind(assigned_work_df, temp)
}
p <- polished_graph(assigned_work_df,'subject-area', "",
                    "Have you assigned work to your students which asks them to use generative AI to complete?")
ggsave(paste0("../../Figures/Children/","assigned-work2", ".jpg"), plot = p, height = 4, width = 12)
#########


