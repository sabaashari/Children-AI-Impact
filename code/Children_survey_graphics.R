library(readr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(stringr)
library(scales)
library(tidyr)
library(rlang)
library(hash)
library(grid)
library(openxlsx)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## define indices, change them to something, change the plots to canvas and edit them!
## to do: write it in a for loop for each of these demographics and then decide
## also check it with the regressions

polished_graph <- function(df, demo, x_title, plot_title)
{
  groups <- unique(df$value)
  custom_colors <-c("#1F9C92", "#FF6F32", "#897FCB", "#FFC72C", "#3F88C5", "#BBBBBB", "pink","red", "purple", "green" , "#335511", "#44FFDD")
  match_indices <- match(df$value, groups)
  if(length(groups)==2)
    positions <- c(-0.2, 0.2)
  else if (length(groups)==3)
    positions <- c(-0.2, 0, 0.2)
  else if (length(groups)==4)
    positions <- c(-0.3, 0, 0.2, 0.4)
  else
    positions <- c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)
  p <- ggplot(df, aes(x = choice,y = freq_weighted*100, fill = value)) +
    geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
    scale_fill_manual(values = custom_colors)  +  # More appealing colors
    # Adjust text label position & size
    geom_text(aes(label = paste0(round(freq_weighted * 100), "")),
              position = position_nudge(x = positions[match_indices], y = 3), 
              size = 4, color = "black", fontface = "bold") + 
    labs(
      fill = demo,
      title = str_wrap(plot_title, width = 60),  # Wrapping title for better display
      x = NULL,  # Remove x-axis label
      y = str_wrap(x_title, width = 50)) +
     theme_minimal() +  # Cleaner background with larger base text size
    theme(
      plot.title = element_text(hjust = 0.5, vjust = 1, size = 22, face = "bold", color = "darkblue"),  # Emphasize title
      plot.subtitle = element_text(hjust = 0.5, size = 14, face = "italic", color = "gray40"),  # Subtitle style
      axis.text = element_text(size = 12, color = "black", face = "bold"),
      axis.text.x = element_text(size = 11, color = "black", face = "bold"),
      axis.title.y = element_text(size = 12, face = 'bold'),
      axis.title.x = element_text(size = 11, face = 'bold'),
      strip.text = element_text(size = 14, face = 'bold'),
      legend.title =  element_text(size = 12, face = 'bold'),
      legend.text = element_text(size = 12),
      legend.position = "top",  # Move legend to the top for better visibility
      #panel.grid = element_blank(),  # Remove gridlines for cleaner look
      #panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add border around the plot
    )
  return (p)
}
  

polished_graph_2 <- function(temp, demo)
{
  custom_colors <-c("#FF6F32", "#1F9C92",  "#897FCB", "#FFC72C", "#3F88C5", "#BBBBBB", "pink","red", "purple", "green" , "#335511", "#44FFDD")
  p <- ggplot(temp, aes(x = choice, y = freq*100, fill = value)) +
    geom_bar(stat = 'identity', position = 'dodge', width = 0.7, color = "black") +  # Add borders to bars
    scale_fill_manual(values = custom_colors) + 
    geom_text(aes(label = paste0(round(freq * 100))),
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

### Read data and clean it ####
df <- read_csv("../Data/confidential/Alan Turing Institute - Generative AI (children) - Labels 021224.csv")
df <- rename(df, school_type = parent2)
df <- rename(df, learning_needs = parent3)
df <- rename(df, child_used_ai = child8)
df <- rename(df,  gen_use_parent = parent10)
df <- rename(df,  internet_access = parent8)
df <- rename(df,  gen_ai_desc = child6)
df <- rename(df,  gen_ai_desc_not_heard = child7)
df <- rename(df,  gen_ai_heard = child2)
df <- rename(df,  gen_ai_talked = child3)
df <- rename(df,  how_often = child12)
df <- rename(df,  gen_use_household = parent11)
df[grepl('No', df$gen_use_parent), 'gen_use_parent'] <- 'No'
df[grepl('Yes', df$gen_use_parent), 'gen_use_parent'] <- 'Yes'
df[grepl('Private', df$school_type), 'school_type'] <- 'Private'
df[grepl('State', df$school_type), 'school_type'] <- 'State'
df[grepl('Home', df$school_type), 'school_type'] <- 'Home'

df <- df %>% mutate(region = case_when(
  profile_GOR_yks_1 == 'Yes' ~ "North",
  profile_GOR_yks_2 == 'Yes' ~ "Midlands",
  profile_GOR_yks_3 == 'Yes' ~ "London",
  profile_GOR_yks_4 == 'Yes' ~ "East, South",
  profile_GOR_yks_6 == 'Yes' ~ "Wales",
  profile_GOR_yks_7 == 'Yes' ~ "Scotland",
  profile_GOR_yks_8 == 'Yes' ~ "Northern Ireland",
  TRUE~ NA_character_))

df <- df %>% mutate(region_aggregate = case_when(
  profile_GOR_yks_1 == 'Yes' | profile_GOR_yks_2 == 'Yes' | profile_GOR_yks_3 == 'Yes' | profile_GOR_yks_4 == 'Yes' ~ "England",
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
df$childage <- as.character(df$childage)
df <- df %>% mutate(socialgrade = case_when(
  socialgrade %in% c('AB', "C1") ~ "ABC1",
  socialgrade %in% c('C2', "DE") ~ "C2DE",
  TRUE~ NA_character_))

## Demographic table ####
demographics <- c("learning_needs")
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


## Figures 1- 4 - AI use and awareness, use cases, time spent ####
school_type <- "Which of the following best describes the type of education your child receives?"
learning_needs <- "Does your child have any additional learning needs?"
child_used_ai <- "Have you ever used generative AI before?" # all
gen_use_parent <- "Are you aware of whether your child has used any generative AI tools? (All parents)"
gen_ai_desc <- "Which of the following best describes what generative AI is?" ## yes to  child2
gen_ai_heard <- "Have you heard of the term 'Generative AI' before?" #all
gen_ai_talked <- "Have any adults in your life ever talked to you about what AI is, or how it works?" # all
how_often <- "How often do you use generative AI?" ## gen AI users
gen_use_household <- "Have you, or anyone in your household, used generative AI tools? (% of all households, n = 780)"
gen_ai_desc_not_heard <- "The images below are example screenshots from an app. Do they look familiar to you?"

df[grepl("Yes",df$gen_use_household), 'gen_use_household'] <- "Yes, Me or someone else"
df[grepl("No,",df$gen_use_household), 'gen_use_household'] <- "No, neither me nor anyone in my household"
df[grepl("I have not,",df$gen_use_household), 'gen_use_household'] <- "Yes, Me or someone else"
df[grepl("I'm not sure",df$gen_ai_heard), 'gen_ai_heard'] <- "No/I'm not sure"
df[grepl("No",df$gen_ai_heard), 'gen_ai_heard'] <- "No/I'm not sure"

all_questions <- c('child_used_ai', 'gen_use_parent', 'gen_ai_heard', 'gen_ai_talked', 'how_often','gen_use_household', 'gen_ai_desc')

questions <- c('child_used_ai', 'gen_use_household', 'gen_ai_heard', 'gen_ai_talked', 'how_often')
demographics <- list(c('school_type',  'childgender', 'childage', 'region', 'socialgrade'), 
                  c('school_type', 'region', 'socialgrade'), 
                  c('school_type', 'region', 'childage'),
                  c('school_type', 'socialgrade', 'gen_ai_heard'), 
                  c('school_type','childgender', 'learning_needs'))
OUT <- createWorkbook()
dd <- df
for (i in 1:length(questions[1]))
{
  question <- questions[i]
  ai_use_demo <- tibble()
  if (question == 'how_often')
    dd <- df %>% filter(child_used_ai == 'Yes')
  
  temp <- dd %>% group_by_at(question) %>% summarise(n= n(), n_w = sum(weight))  %>% 
    mutate(freq = round(n/sum(n),2), freq_weighted = round(n_w/sum(n_w),2), sum_n = sum(n)) %>% filter(sum_n >= 10 )
  colnames(temp)[colnames(temp) == question] <- "choice"
  temp$value <- "All"
  temp$variable <- "All"
  temp$question <- question
  if(question =='gen_use_household')
    temp$choice <- factor(str_wrap(temp$choice,width = 10), levels = str_wrap(c("Yes, Me or someone else",
                                                                                "No, neither me nor anyone in my household", "I'm not sure"), width = 10))
  if(question %in% c('child_used_ai', 'gen_use_parent', 'gen_ai_heard', 'gen_ai_talked'))
    temp$choice <- factor(temp$choice, levels = c("Yes","No/I'm not sure"))
  if(question == 'how_often')
    temp$choice <- factor(str_wrap(temp$choice, width = 10), 
                          levels = str_wrap(c("Every day", "A few times a week", "Once every few weeks", 
                                              "Once a month", "I have only used it a few times", 
                                              "I have only used it once" , "I'm not sure"), width = 10)) 
  p <- polished_graph(temp, "", "","") + 
    theme(axis.text.x = element_text(angle = 45, size = 10, face = "bold", hjust = 1))
  plot_list <- list(p)
  ai_use_demo <- rbind(ai_use_demo,temp)
  demographic <- unlist(demographics[i])
  for (j in 1:length(demographic))
  {
    temp <- dd %>% group_by_at(c(question,demographic[j])) %>% summarise(n= n(), n_w = sum(weight)) %>% ungroup() %>% 
      group_by_at(demographic[j]) %>% 
      mutate(freq = round(n/sum(n),2), freq_weighted = round(n_w/sum(n_w),2), sum_n = sum(n)) %>%
      mutate(variable = demographic[j]) %>% filter(sum_n >= 10 )
    colnames(temp)[colnames(temp) == demographic[j]] <- "value"
    colnames(temp)[colnames(temp) == question] <- "choice"
    temp$question <- question
    if(question =='gen_use_household')
      temp$choice <- factor(str_wrap(temp$choice,width = 10), levels = str_wrap(c("Yes, Me or someone else",
                                                                                  "No, neither me nor anyone in my household", "I'm not sure"), width = 10))
    if(question %in% c('child_used_ai', 'gen_use_parent', 'gen_ai_heard', 'gen_ai_talked'))
      temp$choice <- factor(temp$choice, levels = c("Yes","No/I'm not sure"))
    if(question == 'how_often')
      temp$choice <- factor(str_wrap(temp$choice, width = 10), 
                            levels = str_wrap(c("Every day", "A few times a week", "Once every few weeks", 
                                                "Once a month", "I have only used it a few times", 
                                                "I have only used it once" , "I'm not sure"), width = 10)) 
    
    temp <- temp %>% arrange(temp, choice)
    if (demographic[j] =='school_type')
      temp <- temp %>% filter(value %in% c('Private', 'State'))
    p <- polished_graph(temp, demographic[j], "", "") + 
      theme(axis.text.x = element_text(angle = 45, size = 10, face = "bold", hjust = 1))
    ai_use_demo <- rbind(ai_use_demo,temp) 
    plot_list[[j+1]] <- p
  }
  grid_plots <- do.call(arrangeGrob, c(plot_list))
  a <- grid.arrange(grid_plots,
                    top = textGrob(str_wrap(get(question), width = 70), gp = gpar(fontsize = 18, fontface = "bold")))
  ggsave(paste0('../../Figures/To_be_polished/Figure',i, question, ".svg"), plot = a, width = 10, height = 10, dpi = 300)
  
  if (length(demographic)==2)
  {
    a <- grid.arrange(plot_list[[1]] , plot_list[[2]], plot_list[[3]],nrow = 1,
                      top = textGrob(str_wrap(get(question), width = 70), gp = gpar(fontsize = 18, fontface = "bold")))
    ggsave(paste0('../../Figures/To_be_polished/Figure ',i, question, ".svg"), plot = a, width = 10, height = 5, dpi = 300)
    
  }
  
  addWorksheet(OUT, question)
  writeData(OUT, sheet = question, x = ai_use_demo %>% dplyr::select(choice, freq_weighted, value, variable, question))
}
saveWorkbook(OUT,  "../data/statistics/AI_use_awareness.xlsx")



## Figure 5 -  generative AI- types of systems used ####
OUT <- createWorkbook()
child9 <- "What is the tool you have used?" ## gen AI users
child10 <- "Which of the tools do you use the **most**? You can only pick one." ## more than one tool
parent12 <- "Which of the following tools does your child use?"
gen_ai_details <- df %>% filter(child_used_ai == 'Yes') %>% dplyr::select('weight', contains('child9')) %>% 
  reshape2::melt('weight')%>% group_by(variable,value) %>% summarise(n_weighted = sum(weight), n = n()) %>% 
  mutate(freq = round(n/sum(n),2), freq_weighted = round(n_weighted/sum(n_weighted),2)) %>% filter(value == 'Yes')

child_details <- c('ChatGPT','Bing Chat', 'My AI by SnapChat', 'Gemini by Google','DALL-E (image generator)',
                   'Billie chatbot by Instagram', 'Replika', 'Stable Diffusion (image generator)',
                   'Other (open [child9other]) [open] please specify', 'I’m not sure')
gen_ai_details$variable <- child_details
addWorksheet(OUT, 'generative_details')
writeData(OUT, sheet = 'generative_details', x = gen_ai_details)


ggplot(gen_ai_details, aes(x = reorder(variable, freq_weighted), y = freq_weighted)) +
  geom_bar(stat = 'identity', position = 'stack', fill = "dodgerblue4", alpha = 0.7, width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  # text label position & size
  geom_text(aes(label = paste0(round(freq_weighted * 100), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +  
  # Labels & Titles
  labs(
    title = str_wrap(child9, width = 50),
    x = "",
    y = "% of Generative AI Users (n = 170)"
  ) +  
  coord_flip() + 
  # Theme
  theme_light() + 
  theme(
    strip.text = element_text(size = 14, face = 'bold'),
    axis.text = element_text(size = 12, face = 'bold', color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 22, face = "bold"),
    axis.title.x = element_text(size = 15, face = 'bold')
  )

# Save with high resolution
ggsave("../../Figures/To_be_polished/Figure 5-Ai_systems.svg", width = 10, height = 6, dpi = 300)



### Figure 6-AI use: purpose ####
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
gen_ai_purpose$variable <- choices
addWorksheet(OUT, 'AI_purpose')
writeData(OUT, sheet = 'AI_purpose', x = gen_ai_purpose)

ggplot(gen_ai_purpose, aes(x = reorder(variable, freq_weighted), y = freq_weighted)) +
  geom_bar(stat = 'identity', position = 'stack', fill = "dodgerblue4", alpha = 0.7, width = 0.6) +
  scale_y_continuous(labels = percent_format()) +
  # text label position & size
  geom_text(aes(label = paste0(round(freq_weighted * 100), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "white", fontface = "bold") +  
  # Labels & Titles
  labs(
    title = str_wrap(child11, width = 50),
    x = "",
    y = "% of Generative AI Users (n = 170)"
  ) +  
  coord_flip() + 
  # Theme
  theme_light() + 
  theme(
    strip.text = element_text(size = 14, face = 'bold'),
    axis.text = element_text(size = 12, face = 'bold', color = "black"),
    plot.title = element_text(hjust = 0.5, vjust = 1, size = 22, face = "bold"),
    axis.title.x = element_text(size = 15, face = 'bold')
  )
ggsave("../../Figures/purpose_all.jpg", width = 10, height = 6, dpi = 300)


## Figure 7-gen AI purpose- demographics ####
child11 <- "What have you used generative AI for?" ## gen AI users
keys <- c('child11_1', 'child11_2', 'child11_3', 'child11_4', 'child11_5', 'child11_6', 'child11_7', 
           'child11_8', 'child11_9', 'child11_10', 'child11_11', 'child11_12', 'child11_13', 'child11_14', 'child11_15')

values = c('Entertainment','Playing with my friends',
             'Curiosity','To find out information or learn about something',
             'Getting advice on something personal','To help me with my homework or schoolwork',
             'Inspiration for new ideas','Chatting and keeping me company','Writing stories',
             'Writing poems or lyrics','Writing non-fiction','Creating fun pictures','Making songs',
             'Others', 'I’m not sure')

purpose_dict <- setNames(as.list(values), keys)
columns <- ls(purpose_dict)
demographics <- c('childgender', 'school_type',  'learning_needs', 'childage', 'socialgrade', 'region')
x_titles <- c("n_females:86 , n_males:84", "n_state_school:126, n_private_school:40", 
              "No_learning_needs:134, yes_learning_need:35" , "n_AB:75, n_C1:54, n_C2:25, n_DE:16",  
              "n_East_South:48, n_London:37 , n_Midlands:22 , n_North:51" , "n_8:21, n_9:29, n_10: 47, n_11: 29, n_12:44")
plot_list <- list()
i <- 1
gen_ai_purpose_demo <- data.frame()
for (demo in c('school_type',  'learning_needs'))
{
  temp <- df %>% filter(child_used_ai == 'Yes') %>% dplyr::select(all_of(c('weight',columns, demo))) %>%  
    pivot_longer(cols = columns, names_to = "choice")  %>% group_by_at(c('choice','value', demo)) %>% 
    summarise(n_weighted = sum(weight), n = n()) %>% group_by_at(c('choice', demo)) %>% 
    mutate(freq = round(n/sum(n),2), sum_n = sum(n),freq_weighted = round(n_weighted/sum(n_weighted),2))  %>% filter(value == 'Yes', sum(n) > 10) %>% dplyr::select(-value)
  temp$choice <- sapply(temp$choice, function(x) purpose_dict[[x]])
  colnames(temp)[colnames(temp) == demo] <- "value"
  temp$varaiable <- demo
  gen_ai_purpose_demo <- rbind(gen_ai_purpose_demo,temp)
  p <- polished_graph(temp, demo = demo,
                 x_title = "" , plot_title = "") + coord_flip()
  plot_list[[i]] <- p
  i <- i+1
  #ggsave(paste0('../../Figures/gen_use_cases_', demo, ".jpg"), plot = p, width = 10, height = 10, dpi = 300)
}
addWorksheet(OUT, 'AI_purpose_demo')

a <- grid.arrange(plot_list[[1]]  , plot_list[[2]] + theme(axis.text.y = element_blank()),
                  top = textGrob(child11, gp = gpar(fontsize = 18, fontface = "bold")), nrow = 1)
ggsave('../../Figures/To_be_polished/Figure 7-gen_use_cases_needs_school.svg', plot = a, width = 10, height = 7, dpi = 300)

writeData(OUT, sheet = 'AI_purpose_demo', x = gen_ai_purpose_demo)
saveWorkbook(OUT,  "../data/statistics/AI_use_cases.xlsx")
## Figure 8-Parents Feeling for all ####
parent_feelings <- data.frame()  
values <-  c("Your child could access inappropriate information",
             "Your child could be too trusting of generative AI and not think critically about the information it provides",
             "Your child could share personal information while using generative AI",
             "Your child could use generative AI to cheat in school",
             "Your child could access false / inaccurate information")
parent15 <- "How concerned or unconcerned are you by each of the following?"

questions <- c('parent15_1', 'parent15_2', 'parent15_3', 'parent15_4', 'parent15_5')
dd <- df %>% mutate(across(all_of(questions),
                           ~case_when(
                             .%in% c("Very unconcerned","Fairly unconcerned")   ~ "unconcerned",
                             .%in% c("Fairly concerned", "Very concerned")  ~ "concerned",
                             .%in% c("Neither concerned nor unconcerned", "Don't know")  ~ "Neutral/not-sure",
                             TRUE~ "Neutral/not-sure"
                           )))
i <- 1
for (question in  questions)
{
  demographic <- c('gen_use_parent')
  temp <- dd %>% group_by_at(question) %>% 
    summarise(n= n(), n_w = sum(weight))  %>% 
    mutate(freq = round(n/sum(n),2), freq_weighted = round(n_w/sum(n_w),2), sum_n = sum(n)) %>% filter(sum_n >= 10 )     
  colnames(temp)[colnames(temp) == question] <- "choice"
  temp$question <- values[i]
  temp$value <- "All"
  temp$choice <- factor(str_wrap(temp$choice, width = 10), 
                        levels = str_wrap(c("unconcerned", "concerned", "Neutral/not-sure"), width = 10))
  temp <- temp %>% arrange(choice)
  parent_feelings<- rbind(parent_feelings,temp)
  i <- i+1
}
p <- ggplot(parent_feelings, aes(x= choice, y =  freq_weighted, fill = choice)) +
  geom_bar(stat='identity', position='dodge', alpha=0.8, color='black', width = 0.5) +
  facet_wrap(~str_wrap(question, width = 50), ncol=2) + 
  scale_fill_manual('', values = c('dodgerblue4', 'indianred', 'white'), breaks=c("unconcerned", "concerned", "Neutral/not-sure"))+
  scale_y_continuous(labels=scales::percent_format()) + xlab('') + ylab('') + 
  theme_minimal() +   theme(strip.text = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 12)) +  
  geom_text(aes(label = paste0(round(freq*100), "%")), 
            position = position_stack(vjust = 0.5), size = 5, color ="black", fontface = "bold")+ 
  labs(title = str_wrap(parent15, width = 70)) + 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"), 
        legend.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 11, color = "black", face = "bold"), legend.position = "noe", 
        legend.title = element_blank(), axis.text.x = element_text(angle = 20, hjust =1 ),  panel.spacing = unit(2, "lines"))


ggsave('../../Figures/parent_feelings_all.jpg', plot = p, width = 10, height = 8, dpi = 300)
ggsave('../../Figures/To_be_polished/Figure8-parent_feelings_all.svg', plot = p, width = 10, height = 8, dpi = 300)
addWorksheet(parent_feeling_excel, 'parent15')
writeData(parent_feeling_excel, sheet = 'parent15', x = parent_feelings)

### Figure 9 - parents' feelings for AI vs non AI users ####
dd[dd$gen_use_household %in% c("I'm not sure", "No, neither me nor anyone in my household"), 
   'gen_use_household'] <- "No/not-sure"

dd[dd$gen_use_parent %in% c("I'm not sure", "No"), 'gen_use_parent'] <- "No/not-sure"

parent_feelings_demo <- data.frame()
i <- 1
plot_list <- c()
demo <- "gen_use_parent"
for (question in  questions)
{
  temp <- dd %>% group_by_at(c(question,demo)) %>% summarise(n= n(), n_w = sum(weight)) %>% ungroup() %>% 
    group_by_at(demo) %>% 
    mutate(freq = round(n/sum(n),2), freq_weighted = round(n_w/sum(n_w),2), sum_n = sum(n)) %>%
    mutate(variable = demo) %>% filter(sum_n > 10)    
  colnames(temp)[colnames(temp) == demo] <- "value"
  colnames(temp)[colnames(temp) == question] <- "choice"
  temp$question <- values[i]
  temp$choice <- factor(str_wrap(temp$choice, width = 10), 
                        levels = str_wrap(c("unconcerned", "concerned", "Neutral/not-sure"), width = 10))
  temp <- temp %>% arrange(choice)
  parent_feelings_demo <- rbind(parent_feelings_demo,temp)
  p <- polished_graph_2(temp, demo) + labs(title = str_wrap(values[i], width = 50), fill = str_wrap("Reported child used AI",width = 40)) + 
    theme(axis.text.x = element_text(angle = 20, hjust = 1)) 
  if (i==1)
    p <- p + theme(legend.position = "top")
  else
    p <- p + theme(legend.position = "none")
  plot_list[[i]] <- p 
  i <- i+1
}
grid_plots <- do.call(arrangeGrob, c(plot_list))
a <- grid.arrange(grid_plots, top = textGrob(str_wrap(parent15, width = 70), 
                                             gp = gpar(fontsize = 16, fontface = "bold"),y = 0.7))

ggsave("../../Figures/parents_feelings_AI_non.jpg", plot = a, height = 10, width = 10)
ggsave("../../Figures/To_be_polished/Figure9-parents_feelings_AI_non.svg", plot = a, height = 10, width = 10)

addWorksheet(parent_feeling_excel, 'parent15_AI_not')
writeData(parent_feeling_excel, sheet = 'parent15_AI_not', x = parent_feelings_demo)
saveWorkbook(parent_feeling_excel, '../data/statistics/parent_feelings.xlsx')


## Figure 10 - Parents' feelings whose children use AI ####
parent_feeling_excel <- createWorkbook()
parent13 <- "To what extent do you feel positively or negatively about your child's use of generative AI?"
dd <- df %>% filter(gen_use_parent %in% "Yes")

dd <- dd %>% mutate(across(all_of('parent13'),
                           ~case_when(
                             .%in% c("Very positively", "Fairly positively")   ~ "positively",
                             .%in% c("Fairly negatively", "Very negatively")  ~ "negatively",
                             .%in% c("Neither positively nor negatively", "Don't know")  ~ "Neutral/not-sure",
                             TRUE~ "Neutral/not-sure"
                           )))

question <- "parent13"
temp <- dd %>% group_by_at(question) %>% 
  summarise(n= n(), n_w = sum(weight))  %>% 
  mutate(freq = round(n/sum(n),2), freq_weighted = round(n_w/sum(n_w),2), sum_n = sum(n)) %>% filter(sum_n >= 10 )     
colnames(temp)[colnames(temp) == question] <- "choice"
temp$question <- question
temp$value <- "All"
temp$variable <- "All"
temp$choice <- factor(str_wrap(temp$choice, width = 10), 
                      levels = str_wrap(c("positively", 
                                          "negatively" , 
                                          "Neutral/not-sure"), width = 10))  

temp <- temp %>% arrange(choice)
parent_feelings_demo <- temp
p <- polished_graph(temp, "", "","") + theme(axis.text.x = element_text(angle = 45, hjust = 1, size  = 10, face = "bold"))
plot_list <- list(p)
dd[dd$gen_use_household %in% c('I have not, but someone else in my household has'), 'gen_use_household'] <- 'Someone else has'

demographic <- c('school_type', 'socialgrade', 'gen_use_household')
for (j in 1:length(demographic))
{
  temp <- dd %>% group_by_at(c(question,demographic[j])) %>% summarise(n= n(), n_w = sum(weight)) %>% ungroup() %>% 
    group_by_at(demographic[j]) %>% 
    mutate(freq = round(n/sum(n),2), freq_weighted = round(n_w/sum(n_w),2), sum_n = sum(n)) %>%
    mutate(variable = demographic[j]) %>% filter(sum_n >= 10)     
  colnames(temp)[colnames(temp) == demographic[j]] <- "value"
  colnames(temp)[colnames(temp) == question] <- "choice"
  temp$question <- question
  temp$choice <- factor(str_wrap(temp$choice, width = 10), 
                        levels = str_wrap(c("positively", 
                                            "negatively" , 
                                            "Neutral/not-sure"), width = 10))  
  temp <- temp %>% arrange(choice)
  parent_feelings_demo <- rbind(parent_feelings_demo,temp)
  p <- polished_graph(temp, demographic[j], "","") + 
    theme(axis.text.x = element_text(angle = 45, size = 10, face = "bold", hjust = 1))
  plot_list[[j+1]] <- p
}
grid_plots <- do.call(arrangeGrob, c(plot_list))
a <- grid.arrange(
  grid_plots,
  top = textGrob(str_wrap(parent13, width = 70), gp = gpar(fontsize = 18, fontface = "bold"))
)
ggsave("../../Figures/To_be_polished/Figure 10-parents_feelings_AI_users.jpg", plot = a, height = 7, width = 10, dpi = 300)

addWorksheet(parent_feeling_excel, 'parent13')
writeData(parent_feeling_excel, sheet = 'parent13', x = parent_feelings_demo)

### Figure 11 - children statements for AI users ####
keys <- c("child15_1", "child15_2", 'child15_3', "child15_4", "child15_5", 
  'child15_6', "child15_7", "child15_8", 'child15_9')

values <-  c('Using the tool helps me express myself', 
           'I have used the tool to communicate something I had a hard time communicating on my own', 
           'I feel as though the tool understands the things I tell it', 
           'I feel that I can control the kind of things the the tool creates for me very well',
           'I feel like I understand how the tool works', 
           'I have used the tool to come up with new ideas',
           'the tool makes me feel more creative', 
           'If the tool tells me something, I believe it is always correct', 
           'I feel like I can share anything with the tool')

i <- 1
children_feelings <- data.frame()

for (question in keys)
{
  temp <- df %>% filter(child_used_ai %in% 'Yes') %>% group_by_at(question) %>% 
    summarise(n= n(), n_w = sum(weight))  %>% 
    mutate(freq = round(n/sum(n),2), freq_weighted = round(n_w/sum(n_w),2), sum_n = sum(n)) %>% filter(sum_n >= 10 )     
  colnames(temp)[colnames(temp) == question] <- "choice"
  temp$question <- values[i]
  temp$value <- "All"
  temp$choice <- factor(temp$choice, levels = c("Always", "Sometimes", "Never", "I'm not sure"), ordered = TRUE)
  temp <- temp %>% arrange(choice)
  children_feelings <- rbind(children_feelings,temp)
  i <- i+1
}
p <- ggplot(children_feelings, aes(x= choice, y =  freq_weighted, fill = choice)) +
  geom_bar(stat='identity', position='dodge', alpha=0.8, color='black', width = 0.5) +
  facet_wrap(~str_wrap(question, width = 40), ncol=3) + 
  scale_y_continuous(labels=scales::percent_format()) + xlab('') + ylab('') + scale_fill_brewer()+
  theme_minimal() +   theme(strip.text = element_text(size = 13, face = 'bold'), axis.text.x = element_text(size = 12)) +  
  geom_text(aes(label = paste0(round(freq*100), "%")), 
            position = position_stack(vjust = 0.5), size = 5, color ="black", fontface = "bold")+ 
  theme(plot.title = element_text(hjust = 0.5, vjust = 1, size = 18, face = "bold", color = "darkblue"), 
        legend.text = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 11, color = "black", face = "bold"), legend.position = "noe", 
        legend.title = element_blank(), axis.text.x = element_text(angle = 20, hjust =1 ),  panel.spacing = unit(2, "lines"))
ggsave('../../Figures/To_be_polished/Figure 11- children_feelings_AI_users.jpg', plot = p, width = 12, height = 8, dpi = 300)

addWorksheet(feeling_excel, "perception_all")
writeData(feeling_excel, "perception_all", children_feelings)



### Figure 12 - children statements by School type ####
i <- 1
children_feelings_demo <- data.frame()
plot_list <- c()
for (question in  keys)
{
  temp <- df %>% filter(child_used_ai %in% 'Yes') %>% group_by_at(c(question,'school_type')) %>% summarise(n= n(), n_w = sum(weight)) %>% ungroup() %>% 
    group_by_at('school_type') %>% 
    mutate(freq = round(n/sum(n),2), freq_weighted = round(n_w/sum(n_w),2), sum_n = sum(n)) %>%
    mutate(variable = 'school_type')   
  colnames(temp)[colnames(temp) == 'school_type'] <- "value"
  colnames(temp)[colnames(temp) == question] <- "choice"
  temp$question <- values[i]
  temp$choice <- factor(temp$choice, levels = c("Always", "Sometimes", "Never", "I'm not sure"), ordered = TRUE)
  temp <- temp %>% arrange(choice)
  temp <- temp %>% filter(value %in% c('Private', 'State'))
  p <- polished_graph_2(temp, 'school_type') + labs(title = str_wrap(values[i], width = 40), fill = str_wrap("school type",width = 20)) + 
    theme(axis.text.x = element_text(angle = 20, hjust = 1)) 
  children_feelings_demo <- rbind(children_feelings_demo, temp)
  if (i==1)
    p <- p + theme(legend.position = "top")
  else
    p <- p + theme(legend.position = "none")
  plot_list[[i]] <- p 
  i <- i+1
}
grid_plots <- do.call(arrangeGrob, c(plot_list))
a <- grid.arrange(grid_plots)
ggsave('../../Figures/To_be_polished/Figure 12-children_feelings_school_type_AI_users.svg', plot = a, width = 12, height = 8, dpi = 300)

addWorksheet(feeling_excel, "perception_school_type")
writeData(feeling_excel, "perception_school_type", children_feelings_demo)

saveWorkbook(feeling_excel,  "../data/statistics/children_feelings.xlsx")







## Figure 13 -Children Feelings for all children ####
df[df$child20_scary=="I don't find it scary/confusing", 'child20_scary'] <- "1- I don't find it scary/confusing"
child15 <- "Take a minute to read each of these sentences, and then choose from the following options:"
keys = c('child20_scary', 'child20_exciting',"child15_1", 
         "child15_2", 'child15_3', "child15_4", "child15_5", 
         'child15_6', "child15_7", "child15_8", 'child15_9')
values = c('I find it scary', 'I find it exciting', 'Using the tool helps me express myself', 
           'I have used the tool to communicate something I had a hard time communicating on my own', 
           'I feel as though the tool understands the things I tell it', 
           'I feel that I can control the kind of things the the tool creates for me very well',
           'I feel like I understand how the tool works', 
           'I have used the tool to come up with new ideas',
           'the tool makes me feel more creative', 
           'If the tool tells me something, I believe it is always correct', 
           'I feel like I can share anything with the tool')
child_dict <- setNames(as.list(values), keys)
demographics <- c('childgender', 'school_type',  'learning_needs', 'socialgrade', 'childage', 'region')
x_titles <- c("n_females:86 , n_males:84", "n_state_school:126, n_private_school:40", "No_learning_needs:134, yes_learning_need:35",
              "n_8:21, n_9:29, n_10: 47, n_11: 29, n_12:44", "n_AB:75, n_C1:54, n_C2:25, n_DE:16","n_East_South:48, n_London:37 , n_Midlands:22 , n_North:51")
questions <- c("child15_1", "child15_2", 'child15_3', "child15_4", "child15_5", 
               'child15_6', "child15_7", "child15_8", 'child15_9')
custom_colors <-c("#1F9C92", "#FF6F32", "#4B3E75", "#FFC72C", "#3F88C5", "black", "pink")
demographics <- c('childgender', 'school_type','child8', 'childage', 'learning_needs', 'socialgrade')
x_titles <- c("n_females:353 , n_males:427", "n_state_school:680, n_private_school:76, n_home_schooling:18", "No_learning_needs:616, yes_learning_need:151",
              "n_8:21, n_9:29, n_10: 47, n_11: 29, n_12:44", "n_AB:282, n_C1:231, n_C2:145, n_DE:122","n_East_South:48, n_London:37 , n_Midlands:22 , n_North:51")


questions <- c('child20_scary', 'child20_exciting')
dd <- df %>% mutate(across(all_of('child20_scary'),
                           ~case_when(
                             .%in% c("1- I don't find it scary/confusing", "2")   ~ "Net-not-scary",
                             .%in% c("5 - I find it scary/confusing", "4")  ~ "Net-scary",
                             .%in% c("Not sure", "3")  ~ "Neutral/not-sure",
                             TRUE~ "Neutral/not-sure"
                           )))
dd <- dd %>% mutate(across(all_of('child20_exciting'),
                           ~case_when(
                             .%in% c("1 - I find it exciting", "2")   ~ "Net-Exciting",
                             .%in% c("5 - I don't find it exciting", "4")  ~ "Net-not-Exciting",
                             .%in% c("Not sure", "3")  ~ "Neutral/not-sure",
                             TRUE~ "Neutral/not-sure"
                           )))
dd[dd$child_used_ai %in% c("I'm not sure", 'No'), 'child_used_ai'] <- "No/not sure" 
dd[dd$gen_ai_talked %in% c("I'm not sure", 'No'), 'gen_ai_talked'] <- "No/not sure" 
demographics <- c('school_type',  'gen_ai_talked')
feeling_excel <- createWorkbook()
for (question in questions)
{
  ai_feeling_demo = data_frame()
  temp <- dd %>% group_by_at(question) %>% 
    summarise(n= n(), n_w = sum(weight))  %>% 
    mutate(freq = round(n/sum(n),2), freq_weighted = round(n_w/sum(n_w),2), sum_n = sum(n)) %>% filter(sum_n >= 10 )     
  colnames(temp)[colnames(temp) == question] <- "choice"
  temp$question <- question
  temp$value <- "All"
  temp$variable <- "All"
  temp <- temp %>% arrange(choice)
  p <- polished_graph(temp, "", "","") + theme(axis.text.x = element_text(angle = 20, hjust =1 ))
  plot_list <- list(p)
  ai_feeling_demo <- rbind(ai_feeling_demo,temp)
  for (j in 1:length(demographics))
  {
    temp <- dd %>% group_by_at(c(question,demographics[j])) %>% summarise(n= n(), n_w = sum(weight)) %>% ungroup() %>% 
      group_by_at(demographics[j]) %>% 
      mutate(freq = round(n/sum(n),2), freq_weighted = round(n_w/sum(n_w),2), sum_n = sum(n)) %>%
      mutate(variable = demographics[j]) %>% filter(sum_n >= 10 )
    colnames(temp)[colnames(temp) == demographics[j]] <- "value"
    colnames(temp)[colnames(temp) == question] <- "choice"
    temp$question <- question
    temp <- temp %>% arrange(choice)
    if (demographics[j]=='school_type')
      temp <- temp %>% filter(value %in% c('Private', 'State'))
    ai_feeling_demo <- rbind(ai_feeling_demo,temp)
    #x_title <- paste0("% of generative AI users in each group:\n",x_titles[j])
    #x_title <- paste0("% in each group:\n",x_titles[j])
    p <- polished_graph(temp, demographics[j], "","") + theme(axis.text.x = element_text(angle = 20, hjust =1 ))
    plot_list[[j+1]] <- p
  }
  grid_plots <- do.call(arrangeGrob, c(plot_list))
  a <- grid.arrange(
    plot_list[[1]], plot_list[[2]], plot_list[[3]], nrow = 1,
    top = textGrob(str_wrap(child_dict[[question]], width = 70), gp = gpar(fontsize = 18, fontface = "bold"))
  )
  ggsave(paste0('../../Figures/To_be_polished/', question, ".jpg"), plot = a, width = 10, height = 5, dpi = 300)
  addWorksheet(feeling_excel, question)
  writeData(feeling_excel, sheet = question, x = ai_feeling_demo)
}