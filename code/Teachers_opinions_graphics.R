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

polished_graph <- function(temp, demo)
{
  custom_colors <-c("#FF6F32", "#1F9C92",  "#897FCB", "#FFC72C", "#3F88C5", "#BBBBBB", "pink","red", "purple", "green" , "#335511", "#44FFDD")
  p <- ggplot(temp, aes(x = choice, y = freq*100, fill = value)) +
    geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
    scale_fill_manual(values = custom_colors) + 
    geom_text(aes(label = paste0(round(freq * 100), "%")),
              position = position_dodge(width = 0.7), vjust = 1.5, 
              size = 4, color = "black", fontface = "bold") +  # White text with bold font
    # Labels & Titles
    labs(
      fill = str_wrap(demo,width = 10),
      title = str_wrap("", width = 30),  # Wrapping title for better display
      x = NULL,  # Remove x-axis label
      y = str_wrap("", width = 50)) +
    theme_minimal() +  # Cleaner background with larger base text size
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 1, size = 14, face = "bold", color = "darkblue"),  # Emphasize title
      plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
      axis.text = element_text(size = 12, color = "black", face = "bold"),
      axis.title = element_text(size = 16, face = 'bold'),
      strip.text = element_text(size = 14, face = 'bold'),
      legend.title =  element_text(size = 16, face = 'bold'),
      legend.text = element_text(size = 16))
  return (p)
  
}
### Read data and clean ####
df <- read.xlsx("../Data/confidential/Understanding teachers' perspectives on AI - Final dataset.xlsx")
df <- df[2:nrow(df),]
df <- clean_data(df)
job_titles <- c("Teaching assistant", "Primary school teacher", "Secondary school teacher", "Special education needs", "Headteacher", "Other")
subject_areas <- c("English", "Maths", "Science", "Design and technology", "History", "Geography", "Art and design", "Music", 
                   "Physical education", "Computing", "Ancient and modern foreign languages", "Other")

## Effect of AI on the quality of Teachers' performance ####
cases_list <- c("Designing exams", "designing homework assignments", "Marking exams", "Student feedback", 
                "Lesson planning and research", "Developing personalised learning plans for students", 
                "Generating educational content for classroom presentations", 
                "Responding to parents' emails", "Other")

performance <- data.frame()
i <- 1
for (case in cases_list){
  dd <- df %>% filter(.data[[case]] %in% 'Yes')
  useful_genai <- paste0("useful-genai_", i)
  #dd[is.na(dd[useful_genai]), useful_genai] <- "Neither agree nor disagree"
  dd <- dd %>%   mutate(across(all_of(useful_genai),
                               ~case_when(
                                 .%in% c("Strongly agree", "Agree")   ~ "Agree",
                                 .%in% c("Neither agree nor disagree", "Neutral")  ~ "Neither agree nor disagree",
                                 .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                                 TRUE~ "Neither agree nor disagree"
                               )))
  temp <- dd %>% group_by_at(useful_genai) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
  names(temp)[names(temp) == useful_genai] <- "choice"
  temp$choice <- factor(temp$choice, levels = c("Strongly agree", "Agree", "Neither agree nor disagree","Disagree","Strongly disagree"), ordered = TRUE)
  temp <- temp %>% arrange(choice)
  temp$use_case <- case
  performance <- rbind(performance, temp)
  i <- i+1
}

ggplot(performance, aes(x= choice, y =  freq, fill = choice)) +
  geom_bar(stat='identity', position='dodge', alpha=0.8, color='black', width = 0.5) +
  facet_wrap(~str_wrap(use_case, width = 30), ncol=3) + 
  scale_y_continuous(labels=scales::percent_format()) + xlab('') + ylab('') +
  scale_fill_manual('', values = c('dodgerblue4', 'white', 'indianred'), breaks=c('Agree', 'Neither agree nor disagree', 'Disagree'))+
  theme_minimal() +   theme(strip.text = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 12)) +  
  geom_text(aes(label = paste0(round(freq*100), "%")), 
            position = position_stack(vjust = 0.5), size = 5, color ="black", fontface = "bold")+ 
  labs(title = str_wrap("Using generative AI has significantly improved the quality of my performance when it comes to..", width = 60)) + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"), 
        legend.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 11, color = "black", face = "bold"), legend.position = "noe", 
        legend.title = element_blank(), axis.text.x = element_text(angle = 20, hjust =1 ))

ggsave("../../Figures/Teachers/performance_all.jpg", width = 10, height = 7, dpi = 300)


## Teachers' system types ####
system_lists <- c("ChatGPT", "Dall-E", "Midjourney", "Claude", "Perplexity", "Gemini", "CoPilot", "DeepSeek", "Other")
for (system in system_lists)
  df[system] = ifelse(grepl(system, df$`systems-used`),'Yes', 'No')

df <- df %>% mutate(system = ifelse(!is.na(`multiple-systems`), `multiple-systems`,`systems-used`))
system_df <- df %>% filter(`genai-at-work` %in% 'Yes') %>% group_by(system)  %>% summarise(n = n()) %>% mutate(freq = n/sum(n))


ggplot(system_df, aes(x = reorder(system, -freq), y = freq)) +
  geom_bar(stat = 'identity', position = 'stack', fill = "dodgerblue4", alpha = 0.7, width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  # text label position & size
  geom_text(aes(label = paste0(round(freq * 100), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "black", fontface = "bold") +  
  labs(
    y = "",
    x = "",
    title = "what is the system you use? \n % of Generative AI Users (n = 661)"
  ) +
  theme_light() + 
  theme(
    strip.text = element_text(size = 14, face = 'bold'),
    axis.text = element_text(size = 12, face = 'bold', color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold", color = "dodgerblue4"),
    axis.title.x = element_text(size = 15, face = 'bold')
  )
ggsave('../../Figures/Teachers/systems.jpg')

### Figure 3 - Teachers' statements for generative AI users ####
statements <- c("I feel confident using the system", "The system has increased my productivity", 
                "The system has had a positive impact on my teaching", "I trust the outputs of the system")


all_statements <- data.frame()
for (i in 1:4){
  statement_mixed <- paste0("statement_",i)
  
  temp <- df %>% mutate(across(all_of(statement_mixed),
                               ~case_when(
                                 .%in% c("Strongly agree", "Agree")   ~ "Agree",
                                 .%in% c("Neither agree nor disagree", "Neutral")  ~ "Neither agree nor disagree",
                                 .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                                 TRUE~ "Neutral"
                               )))
  temp <- temp %>% filter(`genai-at-work` %in% 'Yes') %>% group_by_at(statement_mixed) %>% summarise(n = n()) %>% 
    mutate(freq = n/sum(n))
  
  names(temp)[names(temp) == statement_mixed] <- "choice"
  temp$choice <- factor(temp$choice, levels = c("Agree", "Neither agree nor disagree","Disagree"), ordered = TRUE)
  temp <- temp %>% arrange(choice)
  temp$statement <- statements[i]
  all_statements <- rbind(all_statements, temp %>% filter(!is.na(choice)))
  i <- i+1
}
ggplot(all_statements, aes(x= choice, y =  freq, fill = choice)) +
  geom_bar(stat='identity', position='dodge', alpha=0.8, color='black', width = 0.5) +
  facet_wrap(~str_wrap(statement, width = 40), ncol=2) + 
  scale_y_continuous(labels=scales::percent_format()) + xlab('') + ylab('') +
  scale_fill_manual('', values = c('dodgerblue4', 'white', 'indianred'), breaks=c('Agree', 'Neither agree nor disagree', 'Disagree'))+
  theme_minimal() +   theme(strip.text = element_text(size = 15, face = 'bold'), axis.text.x = element_text(size = 12)) +  
  geom_text(aes(label = paste0(round(freq*100), "%")), 
            position = position_stack(vjust = 0.5), size = 5, color ="black", fontface = "bold")+ 
  labs(title = str_wrap("For AI users,  n = 661", width = 60), size = 14) + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold", color = "darkblue"), 
        legend.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12, color = "black", angle = 20, hjust = 1, face= 'bold'), legend.position = "none", 
        legend.title = element_blank(), 
        panel.spacing = unit(3, "lines"))# Emphasize title
ggsave("../../Figures/Teachers/statements.jpg", width = 10, height = 6, dpi = 300)

###Figure 4-  Teachers' statements for all teachers ####
statements <- c("Generative AI should be more widely used in the classroom by teachers", 
                "I believe I would be able to tell if a student submitted work that had been made with generative AI", 
                "Generative AI makes me worried about my job security", 
                "Generative AI can help me enhance the skills I have", 
                "Generative AI can be used to make the process of marking student work more fair", 
                "Generative AI can reduce the amount of time teachers spend working overtime")
all_statements <- data.frame()
for (i in 1:6){
  
  statement_impact <- paste0("teacher-statements_",i)
  dd <- df %>%  filter(!is.na(.data[[statement_impact]])) %>% mutate(across(all_of(statement_impact),
                                                                            ~case_when(
                                                                              .%in% c("Strongly agree", "Agree")   ~ "Agree",
                                                                              .%in% c("Neither agree nor disagree", "Don't know")  ~ "Neither agree nor disagree",
                                                                              .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                                                                              TRUE~ "Neither agree nor disagree"
                                                                            )))
  dd[statement_impact] <- ifelse(dd[statement_impact] %in% "Strongly agree", "Agree", dd[statement_impact])
  temp <- dd %>% filter(!is.na(.data[[statement_impact]])) %>% group_by_at(statement_impact) %>% summarise(n = n()) %>% 
    mutate(freq = n/sum(n))
  names(temp)[names(temp) == statement_impact] <- "choice"
  temp$choice <- factor(temp$choice, levels = c("Strongly agree", "Agree", "Neither agree nor disagree","Disagree","Strongly disagree", "Don't know"), ordered = TRUE)
  temp <- temp %>% arrange(choice)
  temp$statement <- statements[i]
  temp$question <- i
  all_statements <- rbind(all_statements, temp %>% filter(!is.na(choice)))
}
all_statements$question <- factor(all_statements$question, 
                                  levels = c(1,2,3,4,5,6)) 
ggplot(all_statements, aes(x= choice, y =  freq, fill = choice)) +
  geom_bar(stat='identity', position='dodge', alpha=0.8, color='black', width = 0.5) +
  facet_wrap(~str_wrap(statement, width = 45), ncol=2) + 
  scale_y_continuous(labels=scales::percent_format()) + xlab('') + ylab('') +
  scale_fill_manual('', values = c('dodgerblue4', 'white', 'indianred'), breaks=c('Agree', 'Neither agree nor disagree', 'Disagree'))+
  labs(title = str_wrap("For All teachers,  n = 1001", width = 60)) + 
  theme_minimal() +   theme(strip.text = element_text(size = 13, face = 'bold'),
                            axis.text.x = element_text(size = 12)) +  
  geom_text(aes(label = paste0(round(freq*100), "%")), 
            position = position_stack(vjust = 0.5), size = 5, color ="black", fontface = "bold")+ 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold", color = "darkblue"), 
        legend.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 11, color = "black", face = "bold"), legend.position = "none", 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 20, hjust =1 ),
        panel.spacing = unit(2, "lines"))# Emphasize title
ggsave("../../Figures/Teachers/teacher-statements-all.jpg", width = 10, height = 9, dpi = 300)



#### figure 5 - Teachers' statements for AI vs non-AI users ####

demo_statements <- data.frame()
plot_list <- list()
for (i in 1:6)
{
  statement_impact <- paste0("teacher-statements_",i)
  dd <- df %>%   mutate(across(all_of(statement_impact),
                               ~case_when(
                                 .%in% c("Strongly agree", "Agree")   ~ "Agree",
                                 .%in% c("Neither agree nor disagree", "Don't know")  ~ "Neither agree nor disagree",
                                 .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                                 TRUE~ "Neither agree nor disagree"
                               )))
  demo <- 'genai-at-work'
  temp <- dd %>% group_by_at(c(demo,statement_impact)) %>% 
    summarise(n = n()) %>% group_by_at(demo) %>% mutate(freq = n/sum(n), variable = demo) %>% filter(sum(n) >10 )
  
  colnames(temp)[colnames(temp) == demo] <- "value"
  colnames(temp)[colnames(temp) == statement_impact] <- "choice"
  temp$question <- statements[i]
  temp$choice <- factor(temp$choice, levels = c("Agree", "Neither agree nor disagree", "Disagree"))
  temp <- temp %>% arrange(choice)
  demo_statements <- rbind(demo_statements, temp)
  p <- polished_graph(temp, demo) + labs(title = str_wrap(statements[i], width = 40), fill = str_wrap("used AI",width = 10)) + 
    theme(axis.text.x = element_text(angle = 20, hjust = 1)) 
  if (i==1)
    p <- p + theme(legend.position = "right")
  else
    p <- p + theme(legend.position = "none")
  plot_list[[i]] <- p 
}
grid_plots <- do.call(arrangeGrob, c(plot_list))
a <- grid.arrange(grid_plots, top = textGrob(str_wrap("For AI vs non-AI users: n_AI=661 , n_non_AI=340 ", width = 70), 
                                             gp = gpar(fontsize = 16, fontface = "bold"),y = 0.7))
ggsave("../../Figures/Teachers/teacher-statament_AI_non.jpg", plot = a, height = 10, width = 10)
#,top = textGrob(str_wrap(statements[i], width = 70), gp = gpar(fontsize = 16, fontface = "bold")))

### Figure 6- Teachers' overall feelings ####

feelings <- c("Optimistic", "Confident", "Curious", "Neutral", "Cautious", "Uncertain", "Sceptical", "Concerned", "Overwhelmed")
feelings_df <- data.frame()
for (feeling in feelings)
{
  df[feeling] = ifelse(grepl(feeling, df$`overall-teachers`),'Yes', 'No')
  temp <- data.frame(variable = feeling, value = "all", n = sum(grepl(feeling, df$`overall-teachers`)), 
                     freq = sum(grepl(feeling, df$`overall-teachers`))/sum(!is.na(df$`overall-teachers`)))
  feelings_df <- rbind(feelings_df, temp)
}

custom_colors <- c(
  "Optimistic" = "#897FCB",    # Soft Mustard Yellow (warm but not too bright)
  "Confident" = "#1F9C92",     # Muted Blue (trustworthy & strong)
  "Curious" = "#FF6F32",       # Soft Olive Green (exploratory & natural)
  "Neutral" = "#B0B0B0",       # Light Gray (balanced & neutral)
  "Cautious" =  "#3F88C5",      # Muted Orange (gentle alertness)
  "Uncertain" = "#D6A883",     # Soft Brownish Orange (hesitant, warm)
  "Sceptical" = "#335511",     # Muted Reddish-Brown (doubtful but subtle)
  "Concerned" = "#B56576",     # Soft Rose (mild concern)
  "Overwhelmed" = "#886F83"    # Dusty Purple (deep emotion but not harsh)
)
ggplot(feelings_df, aes(x = reorder(variable, -freq), y = freq, fill = variable)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_manual(values = custom_colors)  + 
  geom_text(aes(label = paste0(round(freq* 100), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "black", fontface = "bold") +  
  # Labels & Titles
  labs(
    title = str_wrap("How do you feel about teachers using AI for work: All teachers, n = 1001", width = 60),
    x = "",
    y = ""
  ) +  
  # Theme
  theme_light() + 
  theme(
    strip.text = element_text(size = 14, face = 'bold'),
    axis.text = element_text(size = 12, face = 'bold', color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold", color = "darkblue"),
    legend.position = "None", 
    axis.title = element_text()
  )
ggsave("../../Figures/Teachers/Overall.jpg", width = 11, height = 5, dpi = 300)

## Figure 7 - Overall feelings for AI vs non AI users ####
feelings <- c("Optimistic", "Confident", "Curious", "Neutral", "Cautious", "Uncertain", "Sceptical", "Concerned", "Overwhelmed")
demo_feelings <- data.frame()
for (i in 1:length(feelings))
{
  demo <- 'genai-at-work'
  feeling <- feelings[i]
  temp <-  df %>% group_by_at(c(demo,feeling)) %>% summarise(n = n()) %>% group_by_at(demo) %>%
    mutate(freq = n/sum(n)) %>% filter(.data[[feeling]] %in% 'Yes', n >10) %>% dplyr:: select(-all_of(feeling))
  temp$choice <- feeling
  colnames(temp)[colnames(temp)==demo] <- "value"
  demo_feelings <- rbind(demo_feelings, temp)
  
}
p <- polished_graph(demo_feelings, "Used AI") + 
  theme(axis.text.x = element_text(angle = 20, hjust = 1, size = 14)) + labs(
    title = str_wrap("How do you feel about teachers using AI for work: AI versus non-AI users: n_AI:661 , n_non_AI:340", width = 60))
ggsave("../../Figures/Teachers/feelings_AI_non.jpg", plot = p, height = 5, width = 10)




## Figure 10 -Effect on Students for AI users ####
table(df$`impact-work`)
table(df %>% filter(`impact-work` %in% 'Yes') %>% dplyr::select(`statements-impact_1`), useNA = 'always')
statements <- c("Generative AI has had a positive impact on the creativity of students' work", 
                "Generative AI has made the ideas students are submitting less diverse", 
                "Generative AI has had a negative impact on students' engagement in classwork")
all_statements <- data.frame()
for (i in 1:3){
  
  statement_impact <- paste0("statements-impact_",i)
  dd <- df %>%   mutate(across(all_of(statement_impact),
                               ~case_when(
                                 .%in% c("Strongly agree", "Agree")   ~ "Agree",
                                 .%in% c("Neither agree nor disagree", "Don't know")  ~ "Neither agree nor disagree",
                                 .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                                 TRUE~ "Neither agree nor disagree"
                               )))
  dd[statement_impact] <- ifelse(dd[statement_impact] %in% "Strongly agree", "Agree", dd[statement_impact])
  temp <- dd %>% filter(`impact-work` %in% 'Yes') %>% group_by_at(statement_impact) %>% summarise(n = n()) %>% 
    mutate(freq = n/sum(n))
  names(temp)[names(temp) == statement_impact] <- "choice"
  temp$choice <- factor(temp$choice, levels = c("Strongly agree", "Agree", "Neither agree nor disagree","Disagree","Strongly disagree", "Don't know"), ordered = TRUE)
  temp <- temp %>% arrange(choice)
  temp$statement <- statements[i]
  all_statements <- rbind(all_statements, temp %>% filter(!is.na(choice)))
  i <- i+1
}
ggplot(all_statements, aes(x= choice, y =  freq, fill = choice)) +
  geom_bar(stat='identity', position='dodge', alpha=0.8, color='black', width = 0.4) +
  facet_wrap(~str_wrap(statement, width = 40), ncol=3) + 
  scale_y_continuous(labels=scales::percent_format()) + xlab('') + ylab('') +
  scale_fill_manual('', values = c('dodgerblue4', 'white', 'indianred'), breaks=c('Agree', 'Neither agree nor disagree', 'Disagree'))+
  theme_minimal() +   theme(strip.text = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 12)) +  
  geom_text(aes(label = paste0(round(freq*100), "%")), 
            position = position_stack(vjust = 0.5), size = 5, color ="black", fontface = "bold")+ 
  labs(title = str_wrap("How do you feel as though generative AI has impacted the work or engagement of your students? n = 244", width = 60), 
       size = 15) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"), 
        legend.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 11, color = "black", face = "bold"), legend.position = "noe", 
        legend.title = element_blank(), axis.text.x = element_text(angle = 20, hjust =1 ), 
        panel.spacing = unit(1, "lines"))# Emphasize title
ggsave("../../Figures/Students/statement-impacts.jpg", width = 12, height = 5, dpi = 300)


## Figure 11 -Effect on students for AI users for teachers using AI or not####
statements <- c("Generative AI has had a positive impact on the creativity of students' work", 
                "Generative AI has made the ideas students are submitting less diverse", 
                "Generative AI has had a negative impact on students' engagement in classwork")
demo_statements <- data.frame()
plot_list <- list()
for (i in 1:3)
{
  statement_impact <- paste0("statements-impact_",i)
  dd <- df %>%   mutate(across(all_of(statement_impact),
                               ~case_when(
                                 .%in% c("Strongly agree", "Agree")   ~ "Agree",
                                 .%in% c("Neither agree nor disagree", "Don't know")  ~ "Neither agree nor disagree",
                                 .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                                 TRUE~ "Neither agree nor disagree"
                               )))
  demo <- 'genai-at-work'
  temp <- dd  %>% filter(`impact-work` %in% 'Yes') %>% group_by_at(c(demo,statement_impact)) %>% 
    summarise(n = n()) %>% group_by_at(demo) %>% mutate(freq = n/sum(n), variable = demo) %>% filter(sum(n) >10 )
  
  colnames(temp)[colnames(temp) == demo] <- "value"
  colnames(temp)[colnames(temp) == statement_impact] <- "choice"
  temp$question <- statements[i]
  temp$choice <- factor(temp$choice, levels = c("Agree", "Neither agree nor disagree", "Disagree"))
  temp <- temp %>% arrange(choice)
  demo_statements <- rbind(demo_statements, temp)
  p <- polished_graph(temp, demo) + labs(title = str_wrap(statements[i], width = 50), fill = str_wrap("used AI",width = 10)) + 
    theme(axis.text.x = element_text(angle = 20, hjust = 1)) 
  if (i==1)
    p <- p + theme(legend.position = "right")
  else
    p <- p + theme(legend.position = "none")
  plot_list[[i]] <- p 
}

a <- grid.arrange(plot_list[[1]],plot_list[[2]], plot_list[[3]], nrow = 1)

## Figure 12 - Effect on Students for all ####
statements <- c("I am worried generative AI will negatively impact students' critical thinking skills", 
                "Generative AI can help foster students' creativity", 
                "I am worried that generative AI might limit the level of engagement teachers have with students", 
                "I am worried about the impact generative AI may have on children's wellbeing", 
                "Generative AI is a great tool to support students with additional needs")
all_statements <- data.frame()
for (i in 1:5){
  
  statement_impact <- paste0("student-statements_",i)
  dd <- df %>%  filter(!is.na(.data[[statement_impact]])) %>% mutate(across(all_of(statement_impact),
                                                                            ~case_when(
                                                                              .%in% c("Strongly agree", "Agree")   ~ "Agree",
                                                                              .%in% c("Neither agree nor disagree", "Don't know")  ~ "Neither agree nor disagree",
                                                                              .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                                                                              TRUE~ "Neither agree nor disagree"
                                                                            )))
  dd[statement_impact] <- ifelse(dd[statement_impact] %in% "Strongly agree", "Agree", dd[statement_impact])
  temp <- dd %>% filter(!is.na(.data[[statement_impact]])) %>% group_by_at(statement_impact) %>% summarise(n = n()) %>% 
    mutate(freq = n/sum(n))
  names(temp)[names(temp) == statement_impact] <- "choice"
  temp$choice <- factor(temp$choice, levels = c("Strongly agree", "Agree", "Neither agree nor disagree","Disagree","Strongly disagree", "Don't know"), ordered = TRUE)
  temp <- temp %>% arrange(choice)
  temp$statement <- statements[i]
  all_statements <- rbind(all_statements, temp %>% filter(!is.na(choice)))
  i <- i+1
}
ggplot(all_statements, aes(x= choice, y =  freq, fill = choice)) +
  geom_bar(stat='identity', position='dodge', alpha=0.8, color='black', width = 0.5) +
  facet_wrap(~str_wrap(statement, width = 40), ncol=3) + 
  scale_y_continuous(labels=scales::percent_format()) + xlab('') + ylab('') +
  scale_fill_manual('', values = c('dodgerblue4', 'white', 'indianred'), breaks=c('Agree', 'Neither agree nor disagree', 'Disagree'))+
  theme_minimal() +   
  labs(title = str_wrap("General views on students' engagement with generative AI, n = 1001", width = 70), 
       size = 15) +
  theme(strip.text = element_text(size = 14, face = 'bold'), axis.text.x = element_text(size = 12)) +  
  geom_text(aes(label = paste0(round(freq*100), "%")), 
            position = position_stack(vjust = 0.5), size = 5, color ="black", fontface = "bold")+ 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"), 
        legend.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 11, color = "black", face = "bold"), legend.position = "noe", 
        legend.title = element_blank(), axis.text.x = element_text(angle = 20, hjust =1 ))# Emphasize title
ggsave("../../Figures/Students/student-statements-all.jpg", width = 12, height = 7, dpi = 300)



## Figure 13 - Effects on Students for AI versus non AI users ####
statements <- c("I am worried generative AI will negatively impact students' critical thinking skills", 
                "Generative AI can help foster students' creativity", 
                "I am worried that generative AI might limit the level of engagement teachers have with students", 
                "I am worried about the impact generative AI may have on children's wellbeing", 
                "Generative AI is a great tool to support students with additional needs")

demo_statements <- data.frame()
plot_list <- list()
for (i in 1:5)
{
  statement_impact <- paste0("student-statements_",i)
  dd <- df %>%   mutate(across(all_of(statement_impact),
                               ~case_when(
                                 .%in% c("Strongly agree", "Agree")   ~ "Agree",
                                 .%in% c("Neither agree nor disagree", "Don't know")  ~ "Neither agree nor disagree",
                                 .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                                 TRUE~ "Neither agree nor disagree"
                               )))
  demo <- 'genai-at-work'
  temp <- dd %>% group_by_at(c(demo,statement_impact)) %>% 
    summarise(n = n()) %>% group_by_at(demo) %>% mutate(freq = n/sum(n), variable = demo) %>% filter(sum(n) >10 )
  
  colnames(temp)[colnames(temp) == demo] <- "value"
  colnames(temp)[colnames(temp) == statement_impact] <- "choice"
  temp$question <- statements[i]
  temp$choice <- factor(temp$choice, levels = c("Agree", "Neither agree nor disagree", "Disagree"))
  temp <- temp %>% arrange(choice)
  demo_statements <- rbind(demo_statements, temp)
  p <- polished_graph(temp, demo) + labs(title = str_wrap(statements[i], width = 50), fill = str_wrap("used AI",width = 10)) + 
    theme(axis.text.x = element_text(angle = 20, hjust = 1)) 
  if (i==1)
    p <- p + theme(legend.position = "right")
  else
    p <- p + theme(legend.position = "none")
  plot_list[[i]] <- p 
}
grid_plots <- do.call(arrangeGrob, c(plot_list))
a <- grid.arrange(grid_plots, top = textGrob(str_wrap("General views on students' engagement with generative AI for teachers who use AI vs non AI users: n_AI=661 , n_non_AI=340 ", width = 70), 
                                             gp = gpar(fontsize = 15, fontface = "bold"),y = 0.5))
ggsave("../../Figures/Students/student-stataments_AI_non.jpg", plot = a, height = 10, width = 10)





## Policy ####
policy_df <- df %>% group_by(policy) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
ggplot(policy_df, aes(x = reorder(policy, -freq), y = freq, fill = policy)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer()  + 
  geom_text(aes(label = paste0(round(freq* 100), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "black", fontface = "bold") +  
  # Labels & Titles
  labs(
    title = str_wrap("Does your school have an AI policy, or guidance on how to use AI in a school setting?", width = 70),
    x = "",
    y = "% of all teachers (n = 1001)"
  ) +  
  # Theme
  theme_light() + 
  theme(
    strip.text = element_text(size = 14, face = 'bold'),
    axis.text = element_text(size = 12, face = 'bold', color = "black", angle = 20, hjust = 1),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    axis.title.x = element_text(size = 15, face = 'bold'), legend.position = "None"
  )
ggsave("../../Figures/Teachers/policy-all.jpg", width = 11, height = 5, dpi = 300)


temp <-  df %>% group_by(`type-of-school`, policy) %>% summarise(n = n()) %>% group_by(`type-of-school`) %>%
  mutate(freq = n/sum(n))
names(temp)[names(temp) == 'type-of-school'] <- "value"
names(temp)[names(temp) == 'policy'] <- "choice"

temp$choice <- factor(str_wrap(temp$choice, width = 20), levels = c(str_wrap("Yes - for both students and teachers", width = 20), str_wrap("Yes - for students only", width = 20), 
                                                                    str_wrap("Yes - for teachers only", width = 20),str_wrap("I'm not sure", width = 20), str_wrap("No", width = 20)), ordered = TRUE)
temp <- temp %>% arrange(choice)

p <- polished_graph(temp,'type-of-school')
ggsave("../../Figures/Teachers/policy-schools.jpg", width = 11, height = 5, dpi = 300)

policy_df_yes <- df %>% filter(grepl("Yes", policy)) %>% group_by(`policy-yes`) %>% summarise(n = n()) %>% mutate(freq = n/sum(n))
p1 <- ggplot(policy_df_yes, aes(x = reorder(`policy-yes`, -freq), y = freq, fill = `policy-yes`)) +
  geom_bar(stat = 'identity', position = 'stack', width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_brewer()  + 
  geom_text(aes(label = paste0(round(freq* 100), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "black", fontface = "bold") +  
  # Labels & Titles
  labs(
    title = "",
    x = "All",
    y = "% of teachers who said Yes to the policy question (n = 264)"
  ) +  
  # Theme
  theme_light() + 
  theme(
    strip.text = element_text(size = 14, face = 'bold'),
    axis.text = element_text(size = 12, face = 'bold', color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold"),
    axis.title.x = element_text(size = 15, face = 'bold'), legend.position = "None"
  )
temp <-  df %>% filter(grepl("Yes", policy)) %>% group_by(`type-of-school`, `policy-yes`) %>% summarise(n = n()) %>% group_by(`type-of-school`) %>%
  mutate(freq = n/sum(n))
names(temp)[names(temp) == 'type-of-school'] <- "value"
names(temp)[names(temp) == 'policy-yes'] <- "choice"

temp$choice <- factor(temp$choice, levels = c("Yes", "No","I'm not sure")) 
temp <- temp %>% arrange(choice)
p2 <- polished_graph(temp,'type-of-school')


a <- grid.arrange(p1,p2, ncol = 2,top = textGrob(str_wrap("In your opinion, is the policy or guidance sufficient?", width = 70), 
                                                 gp = gpar(fontsize = 18, fontface = "bold")))

ggsave("../../Figures/Teachers/policy-yes.jpg", width = 11, height = 5, dpi = 300, plot = a)







#### Archive ####
### Demographics for teachers' statements- all teachers ####
statements <- c("Generative AI should be more widely used in the classroom by teachers", 
                "I believe I would be able to tell if a student submitted work that had been made with generative AI", 
                "Generative AI makes me worried about my job security", 
                "Generative AI can help me enhance the skills I have", 
                "Generative AI can be used to make the process of marking student work more fair", 
                "Generative AI can reduce the amount of time teachers spend working overtime")


demo_statements <- data.frame()
for (i in 1:6)
{
  statement_impact <- paste0("teacher-statements_",i)
  dd <- df %>%   mutate(across(all_of(statement_impact),
                               ~case_when(
                                 .%in% c("Strongly agree", "Agree")   ~ "Agree",
                                 .%in% c("Neither agree nor disagree", "Don't know")  ~ "Neither agree nor disagree",
                                 .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                                 TRUE~ "Neither agree nor disagree"
                               )))
  j <- 1
  plot_list <- list()
  for (demo in c("gender", "type-of-school", "Teacher-age","years-experience", 'genai-at-work'))
  {
    temp <- dd %>% group_by_at(c(demo,statement_impact)) %>% 
      summarise(n = n()) %>% group_by_at(demo) %>% mutate(freq = n/sum(n), variable = demo) %>% filter(sum(n) >10 )
    
    colnames(temp)[colnames(temp) == demo] <- "value"
    colnames(temp)[colnames(temp) == statement_impact] <- "choice"
    temp$question <- statements[i]
    temp$choice <- factor(temp$choice, levels = c("Agree", "Neither agree nor disagree", "Disagree"))
    temp <- temp %>% arrange(choice)
    demo_statements <- rbind(demo_statements, temp)
    p <- polished_graph(temp, demo)
    plot_list[[j]] <- p + theme(axis.text.x = element_text(angle = 20, hjust = 1))
    j <- (j+1)
  }
  grid_plots <- do.call(arrangeGrob, c(plot_list))
  a <- grid.arrange(grid_plots,top = textGrob(str_wrap(statements[i], width = 70), 
                                              gp = gpar(fontsize = 16, fontface = "bold")))
  ggsave(paste0("../../Figures/Teachers/teacher-statament_",i, "1.jpg"), plot = a, height = 10, width = 11)
  
  gen_ai_use_df_jobs <- data.frame()
  for (job in job_titles)
  {
    temp <- dd  %>% filter(.data[[job]]%in% 'Yes') %>% group_by_at(c(job,statement_impact)) %>% summarise(n = n()) %>% group_by_at(job) %>%
      mutate(freq = n/sum(n))
    temp$variable <- 'job-title'
    names(temp)[names(temp) == job] <- "value"
    colnames(temp)[colnames(temp) == statement_impact] <- "choice"
    temp$value <- job
    temp$choice <- factor(temp$choice, levels = c("Agree", "Neither agree nor disagree", "Disagree"))
    temp <- temp %>% arrange(choice)
    gen_ai_use_df_jobs <- rbind(gen_ai_use_df_jobs, temp)
  }
  demo_statements <- rbind(demo_statements, gen_ai_use_df_jobs)
  gen_ai_use_df_subjects <- data.frame()
  for (subject in subject_areas)
  {
    temp <- dd  %>% filter(.data[[subject]]%in% 'Yes') %>% group_by_at(c(subject,statement_impact)) %>% summarise(n = n()) %>% group_by_at(subject) %>%
      mutate(freq = n/sum(n))
    temp$variable <- 'subject-area'
    names(temp)[names(temp) == subject] <- "value"
    colnames(temp)[colnames(temp) == statement_impact] <- "choice"
    temp$value <- subject
    temp$choice <- factor(temp$choice, levels = c("Agree", "Neither agree nor disagree", "Disagree"))
    temp <- temp %>% arrange(choice)
    gen_ai_use_df_subjects <- rbind(gen_ai_use_df_subjects, temp)
  }
  demo_statements <- rbind(demo_statements, gen_ai_use_df_subjects)
  p1 <- polished_graph(gen_ai_use_df_jobs, 'job-title') + theme(axis.text.x = element_text(angle = 20, hjust = 1))
  p2 <- polished_graph(gen_ai_use_df_subjects, 'subject-area') + theme(axis.text.x = element_text(angle = 20, hjust = 1))
  a <- grid.arrange(p1, p2,top = textGrob(str_wrap(statements[i], width = 70), 
                                          gp = gpar(fontsize = 16, fontface = "bold")))
  
  ggsave(paste0("../../Figures/Teachers/teacher-statament_",i, "2.jpg"), plot = a, height = 7, width = 11)
}





### Teachers' task statements for all teachers ####
statements <- c("Preparing lesson plans", 
                "Delivering lessons to students", 
                "Developing educational content to meet the needs of different learners", 
                "Assessing student performance", 
                "Maintaining a positive learning environment", 
                "Guiding students on academic and personal development", 
                "Engaging with parents, other teachers, and school administrators to support student learning", 
                "Keeping up to date with subject knowledge and teaching methods")
all_statements <- data.frame()
for (i in 1:8){
  
  statement_impact <- paste0("statements_tasks_",i)
  temp <- df %>% filter(!is.na(.data[[statement_impact]])) %>% group_by_at(statement_impact) %>% summarise(n = n()) %>% 
    mutate(freq = n/sum(n))
  names(temp)[names(temp) == statement_impact] <- "choice"
  temp[temp$choice=='Replace teachers in performing the task', 'choice'] <- 'Replace'
  temp[temp$choice=='Support teachers in performing the task', 'choice'] <- 'Support'
  temp$choice <- factor(temp$choice, levels = c("Support", "Replace"), ordered = TRUE)
  temp <- temp %>% arrange(choice)
  temp$statement <- statements[i]
  all_statements <- rbind(all_statements, temp %>% filter(!is.na(choice)))
  i <- i+1
}
ggplot(all_statements, aes(x= choice, y =  freq, fill = choice)) +
  geom_bar(stat='identity', position='stack', alpha=0.8, color='black', width = 0.5) +
  facet_wrap(~str_wrap(statement, width = 50), ncol=2) + 
  scale_y_continuous(labels=scales::percent_format()) + xlab('') + ylab('') +
  scale_fill_manual('', values = c('dodgerblue4', 'indianred'), breaks=c('Support', 'Replace'))+
  theme_minimal() +   theme(strip.text = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 12)) +  
  geom_text(aes(label = paste0(round(freq*100), "%")), 
            position = position_stack(vjust = 0.5), size = 5, color ="black", fontface = "bold")+ 
  labs(title = str_wrap("Do you believe generative AI has the potential to support teachers in performing the task by executing one or more aspects of the task,
                        or to replace teachers in performing the task by fully executing all aspects of the task.", width = 70))+
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 16, face = "bold", color = "darkblue"), 
        legend.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 11, color = "black", face = "bold"), legend.position = "noe", 
        legend.title = element_blank(), 
        axis.text.x = element_text(angle = 0, hjust =0.5 ))# Emphasize title
ggsave("../../Figures/Teachers/task-statements-all.jpg", width = 10, height = 10, dpi = 300)


## task statements for AI vs non-AI users ####
all_statements <- data.frame()
for (i in 1:8){
  
  demo <- 'genai_at_work'
  statement_impact <- paste0("statements_tasks_",i)
  temp <- df %>% filter(!is.na(.data[[statement_impact]])) %>% group_by_at(c(statement_impact, demo)) %>% 
    summarise(n = n()) %>% group_by_at(demo) %>% mutate(freq = n/sum(n)) 
  colnames(temp)[colnames(temp)==demo] <- "value"
  temp$choice <- statements[i]
  all_statements <- rbind(all_statements, temp  %>% dplyr::select(value, n, freq, choice))
  
}
p <- polished_graph(all_statements, "Used AI")+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 14)) + labs(
    title = str_wrap("How do you feel about teachers using AI for work: AI versus non-AI users: n_AI:661 , n_non_AI:340", width = 60))
ggsave("../../Figures/Teachers/feelings_AI_non.jpg", plot = p, height = 5, width = 10)


## Demographics for teachers' overall feelings ####
for (feeling in feelings)
{
  plot_list <- list()
  i <- 1
  for (demo in c("gender", "type-of-school", "working-hours", "Teacher-age", 
                 "additional-needs", "genai-at-work"))
  {
    temp <-  df %>% group_by_at(c(demo,feeling)) %>% summarise(n = n()) %>% group_by_at(demo) %>%
      mutate(freq = n/sum(n)) %>% filter(.data[[feeling]] %in% 'Yes', n >10) 
    temp$choice <- demo
    names(temp)[names(temp) == demo] <- "value"
    p <- polished_graph(temp,demo)
    plot_list[[i]] <- p 
    i <- (i+1)
  }
  grid_plots <- do.call(arrangeGrob, c(plot_list))
  a <- grid.arrange(grid_plots,top = textGrob(str_wrap(feeling, width = 70), 
                                              gp = gpar(fontsize = 18, fontface = "bold")))
  ggsave(paste0("../../Figures/Teachers/",feeling, "1.jpg"), plot = a, height = 9, width = 10)
  gen_ai_use_df_jobs <- data.frame()
  for (job in job_titles)
  {
    temp <- df %>% filter(.data[[job]]%in% 'Yes') %>% group_by_at(c(job,feeling)) %>% summarise(n = n()) %>% group_by_at(job) %>%
      mutate(freq = n/sum(n)) %>%  filter(.data[[feeling]] %in% 'Yes')
    temp[temp[job]%in% 'Yes', job] <- job
    temp$choice <- 'job-title'
    names(temp)[names(temp) == job] <- "value"
    gen_ai_use_df_jobs <- rbind(gen_ai_use_df_jobs, temp)
  }
  gen_ai_use_df_subjects <- data.frame()
  for (subject in subject_areas)
  {
    temp <- df %>% filter(.data[[subject]]%in% 'Yes') %>% group_by_at(c(subject,feeling)) %>% summarise(n = n()) %>% group_by_at(subject) %>%
      mutate(freq = n/sum(n)) %>%  filter(.data[[feeling]] %in% 'Yes')
    temp[temp[subject]%in% 'Yes', subject] <- subject
    temp$choice <- 'subject-area'
    names(temp)[names(temp) == subject] <- "value"
    gen_ai_use_df_subjects <- rbind(gen_ai_use_df_subjects, temp)
  }
  p1 <- polished_graph(gen_ai_use_df_jobs,'job-title')
  p2 <- polished_graph(gen_ai_use_df_subjects,'subject-area')
  a <- grid.arrange(p1, p2,top = textGrob(str_wrap(feeling, width = 70), 
                                          gp = gpar(fontsize = 18, fontface = "bold")))
  ggsave(paste0("../../Figures/Teachers/",feeling, "2.jpg"), plot = a, height = 7, width = 12)
}


## Demographics for students' effects - All students ####
df$student_use <- ifelse(grepl("I am aware of students", df$`use-of-gen-ai`), "Yes", "No") 
statements <- c("I am worried generative AI will negatively impact students' critical thinking skills", 
                "Generative AI can help foster students' creativity", 
                "I am worried that generative AI might limit the level of engagement teachers have with students", 
                "I am worried about the impact generative AI may have on children's wellbeing", 
                "Generative AI is a great tool to support students with additional needs")
demo_statements <- data.frame()
for (i in 1:5)
{
  statement_impact <- paste0("student-statements_",i)
  dd <- df %>%   mutate(across(all_of(statement_impact),
                               ~case_when(
                                 .%in% c("Strongly agree", "Agree")   ~ "Agree",
                                 .%in% c("Neither agree nor disagree", "Don't know")  ~ "Neither agree nor disagree",
                                 .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                                 TRUE~ "Neither agree nor disagree"
                               )))
  j <- 1
  plot_list <- list()
  for (demo in c("gender", "type-of-school", "Teacher-age", "Children-age", 'genai-at-work', 'student_use'))
  {
    temp <- dd %>% group_by_at(c(demo,statement_impact)) %>% 
      summarise(n = n()) %>% group_by_at(demo) %>% mutate(freq = n/sum(n), variable = demo) %>% filter(sum(n) >10 )
    
    colnames(temp)[colnames(temp) == demo] <- "value"
    colnames(temp)[colnames(temp) == statement_impact] <- "choice"
    temp$question <- statements[i]
    temp$choice <- factor(temp$choice, levels = c("Agree", "Neither agree nor disagree", "Disagree"))
    temp <- temp %>% arrange(choice)
    demo_statements <- rbind(demo_statements, temp)
    p <- polished_graph(temp, demo)
    plot_list[[j]] <- p + theme(axis.text.x = element_text(angle = 20, hjust = 1))
    j <- (j+1)
  }
  grid_plots <- do.call(arrangeGrob, c(plot_list))
  a <- grid.arrange(grid_plots,top = textGrob(str_wrap(statements[i], width = 70), 
                                              gp = gpar(fontsize = 16, fontface = "bold")))
  ggsave(paste0("../../Figures/Students/student-statament_",i, "1.jpg"), plot = a, height = 10, width = 11)
  
  gen_ai_use_df_jobs <- data.frame()
  for (job in job_titles)
  {
    temp <- dd  %>% filter(.data[[job]]%in% 'Yes') %>% group_by_at(c(job,statement_impact)) %>% summarise(n = n()) %>% group_by_at(job) %>%
      mutate(freq = n/sum(n))
    temp$variable <- 'job-title'
    names(temp)[names(temp) == job] <- "value"
    colnames(temp)[colnames(temp) == statement_impact] <- "choice"
    temp$value <- job
    temp$choice <- factor(temp$choice, levels = c("Agree", "Neither agree nor disagree", "Disagree"))
    temp <- temp %>% arrange(choice)
    gen_ai_use_df_jobs <- rbind(gen_ai_use_df_jobs, temp)
  }
  demo_statements <- rbind(demo_statements, gen_ai_use_df_jobs)
  gen_ai_use_df_subjects <- data.frame()
  for (subject in subject_areas)
  {
    temp <- dd  %>% filter(.data[[subject]]%in% 'Yes') %>% group_by_at(c(subject,statement_impact)) %>% summarise(n = n()) %>% group_by_at(subject) %>%
      mutate(freq = n/sum(n))
    temp$variable <- 'subject-area'
    names(temp)[names(temp) == subject] <- "value"
    colnames(temp)[colnames(temp) == statement_impact] <- "choice"
    temp$value <- subject
    temp$choice <- factor(temp$choice, levels = c("Agree", "Neither agree nor disagree", "Disagree"))
    temp <- temp %>% arrange(choice)
    gen_ai_use_df_subjects <- rbind(gen_ai_use_df_subjects, temp)
  }
  demo_statements <- rbind(demo_statements, gen_ai_use_df_subjects)
  p1 <- polished_graph(gen_ai_use_df_jobs, 'job-title') + theme(axis.text.x = element_text(angle = 20, hjust = 1))
  p2 <- polished_graph(gen_ai_use_df_subjects, 'subject-area') + theme(axis.text.x = element_text(angle = 20, hjust = 1))
  a <- grid.arrange(p1, p2,top = textGrob(str_wrap(statements[i], width = 70), 
                                          gp = gpar(fontsize = 16, fontface = "bold")))
  
  ggsave(paste0("../../Figures/Students/student-statament_",i, "2.jpg"), plot = a, height = 7, width = 11)
  
}


### demographics for statements ####
job_titles <- c("Teaching assistant", "Primary school teacher", "Secondary school teacher", "Special education needs", "Headteacher", "Other")
subject_areas <- c("English", "Maths", "Science", "Design and technology", "History", "Geography", "Art and design", "Music", 
                   "Physical education", "Computing", "Ancient and modern foreign languages", "Other")

demo_statements <- data.frame()
for (i in 1:4)
{
  statement_mixed <- paste0("statement_",i)
  df <- df %>% mutate(across(all_of(statement_mixed),
                             ~case_when(
                               .%in% c("Strongly agree", "Agree")   ~ "Agree",
                               .%in% c("Neither agree nor disagree", "Neutral")  ~ "Neutral",
                               .%in% c("Strongly disagree", "Disagree")  ~ "Disagree",
                               TRUE~ "Neutral"
                             )))
  j <- 1
  plot_list <- list()
  for (demo in c("gender", "type-of-school", "Teacher-age", "years-experience"))
  {
    temp <- df  %>% filter(`genai-at-work` %in% 'Yes') %>% group_by_at(c(demo,statement_mixed)) %>% 
      summarise(n = n()) %>% group_by_at(demo) %>% mutate(freq = n/sum(n), variable = demo) %>% filter(sum(n) >10 )
    
    colnames(temp)[colnames(temp) == demo] <- "value"
    colnames(temp)[colnames(temp) == statement_mixed] <- "choice"
    temp$question <- statements[i]
    temp$choice <- factor(temp$choice, levels = c("Agree", "Neutral", "Disagree"))
    temp <- temp %>% arrange(choice)
    demo_statements <- rbind(demo_statements, temp)
    p <- polished_graph(temp, demo)
    plot_list[[j]] <- p 
    j <- (j+1)
  }
  grid_plots <- do.call(arrangeGrob, c(plot_list))
  a <- grid.arrange(grid_plots,top = textGrob(str_wrap(statements[i], width = 70), 
                                              gp = gpar(fontsize = 18, fontface = "bold")))
  ggsave(paste0("../../Figures/Teachers/statament_",i, "1.jpg"), plot = a, height = 7, width = 12)
  
  gen_ai_use_df_jobs <- data.frame()
  for (job in job_titles)
  {
    temp <- df  %>% filter(`genai-at-work` %in% 'Yes', .data[[job]]%in% 'Yes') %>% group_by_at(c(job,statement_mixed)) %>% summarise(n = n()) %>% group_by_at(job) %>%
      mutate(freq = n/sum(n))
    temp$variable <- 'job-title'
    names(temp)[names(temp) == job] <- "value"
    colnames(temp)[colnames(temp) == statement_mixed] <- "choice"
    temp$value <- job
    gen_ai_use_df_jobs <- rbind(gen_ai_use_df_jobs, temp)
  }
  demo_statements <- rbind(demo_statements, gen_ai_use_df_jobs)
  gen_ai_use_df_subjects <- data.frame()
  for (subject in subject_areas)
  {
    temp <- df  %>% filter(`genai-at-work` %in% 'Yes', .data[[subject]]%in% 'Yes') %>% group_by_at(c(subject,statement_mixed)) %>% summarise(n = n()) %>% group_by_at(subject) %>%
      mutate(freq = n/sum(n))
    temp$variable <- 'subject-area'
    names(temp)[names(temp) == subject] <- "value"
    colnames(temp)[colnames(temp) == statement_mixed] <- "choice"
    temp$value <- subject
    gen_ai_use_df_subjects <- rbind(gen_ai_use_df_subjects, temp)
  }
  demo_statements <- rbind(demo_statements, gen_ai_use_df_subjects)
  p1 <- polished_graph(gen_ai_use_df_jobs, 'job-title')
  p2 <- polished_graph(gen_ai_use_df_subjects, 'subject-area')
  a <- grid.arrange(p1, p2,top = textGrob(str_wrap(statements[i], width = 70), 
                                          gp = gpar(fontsize = 18, fontface = "bold")))
  
  ggsave(paste0("../../Figures/Teachers/statament_",i, "2.jpg"), plot = a, height = 7, width = 12)
  
  
}