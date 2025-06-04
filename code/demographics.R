## To do: 
## 1. write down main takeaways for pictures, add the menu at the start- check if size and text is ok. 
## 2. regressions and correlations for demographics.
library(hash)
df_labels <- read_csv("../../Data/Alan Turing Institute - Generative AI (children) - Labels 021224.csv")
common_theme <- theme( 
  plot.caption= element_text(size=11, hjust = 0.5, vjust = 0, family="serif"), 
  axis.title.x = element_blank(), 
  axis.title.y = element_blank(), 
  plot.title = element_text(hjust = 0.5, vjust = 1, size = 11),
  plot.subtitle =  element_text(hjust = 0.5, vjust = 1, size = 12, face="bold"), 
  legend.position = "none"
)
### To do: add AI use by parents and children report contradiction for later analysis, which one is better? 
## useful demographics: gender, age, school type, special needs, Gen_AI use ####

df_labels <- rename(df_labels, school_type = parent2)
df_labels <- rename(df_labels, learning_needs = parent3)
df_labels <- rename(df_labels,  internet_access = parent8)
df_labels <- rename(df_labels,  gen_use_child = child8) # this seems more reliable?, not all children know what gen AI is but I guess that is ok!
df_labels <- rename(df_labels,  gen_use_parent = parent10)
df_labels <- rename(df_labels,  gen_ai_desc = child6)
df_labels <- rename(df_labels,  gen_ai_heard = child2)
df_labels <- rename(df_labels,  gen_ai_talked = child3)
df_labels <- rename(df_labels,  how_often = child12)
df_labels <- rename(df_labels,  gen_use_household = parent11)

df_labels$childage <- as.character(df_labels$childage)
#clean dataset
df_labels <- df_labels %>% mutate(region = case_when(
  profile_GOR_yks_1 == 'Yes' ~ "North",
  profile_GOR_yks_2 == 'Yes' ~ "Midlands",
  profile_GOR_yks_3 == 'Yes' ~ "London",
  profile_GOR_yks_4 == 'Yes' ~ "East, South",
  profile_GOR_yks_6 == 'Yes' ~ "Wales",
  profile_GOR_yks_7 == 'Yes' ~ "Scotland",
  profile_GOR_yks_8 == 'Yes' ~ "Northern Ireland",
  TRUE~ NA_character_))

df_labels <- df_labels %>% mutate(region_aggregate = case_when(
  profile_GOR_yks_1 == 'Yes' | profile_GOR_yks_2 == 'Yes' | profile_GOR_yks_3 == 'Yes' | profile_GOR_yks_4 == 'Yes' ~ "England",
  profile_GOR_yks_6 == 'Yes' ~ "Wales",
  profile_GOR_yks_7 == 'Yes' ~ "Scotland",
  profile_GOR_yks_8 == 'Yes' ~ "Northern Ireland",
  TRUE~ NA_character_))

df_labels <- df_labels %>% mutate(socialgrade = case_when(
  profile_socialgrade_cie_yks_1 == 'Yes' ~ "AB",
  profile_socialgrade_cie_yks_2 == 'Yes' ~ "C1",
  profile_socialgrade_cie_yks_3 == 'Yes' ~ "C2",
  profile_socialgrade_cie_yks_4 == 'Yes' ~ "DE",
  TRUE~ NA_character_))

school_type <- "Which of the following best describes the type of education your child receives?"
learning_needs <- "Does your child have any additional learning needs?"
gen_use_child <- "Have you ever used generative AI (gen AI) before? (All children)" # all
gen_use_parent <- "Are you aware of whether your child has used any generative AI tools? (All parents)"
gen_ai_desc <- "Which of the following best describes what generative AI is?" ## yes to  child2
gen_ai_heard <- "Have you heard of the term 'Generative AI' before?" #all
gen_ai_talked <- "Have any adults in your life ever talked to you about what AI is, or how it works?" # all
how_often <- "How often do you use gen AI? (for AI users, n = 170)" ## gen AI users
gen_use_household <- "Have you, or anyone in your household, used generative AI tools? (All parents)"

child_feeling <- "How do you feel about AI?" ## ALL


### checks ####
prop.table(table(df_labels$childgender))*100
prop.table(table(df_labels$childage))*100
prop.table(table(df_labels$school_type))*100


reliability_check <- df_labels %>% dplyr::filter(how_often =='Yes') %>% dplyr::select(c('child6','weight','gen_use_child')) %>% group_by(gen_use_child, child6) %>% 
  summarise(n= n(), n_w = sum(weight)) %>% ungroup() %>% 
  group_by(gen_use_child) %>% mutate(freq_ai = round(n/sum(n)*100,2), freq_weighted_ai = round(n_w/sum(n_w)*100,2)) 

  
temp <- df_labels %>% group_by(gen_use_child, gen_use_parent) %>% summarise(n= n(), n_w = sum(weight))%>% ungroup() %>% 
  group_by(gen_use_child) %>% mutate(freq_ai = round(n/sum(n)*100,2), freq_weighted_ai = round(n_w/sum(n_w)*100,2)) 

colnames(temp)[colnames(temp) == demographic[j]] <- "value"
ai_demo <- rbind(ai_demo,temp) 


table(df_labels$gen_use_child)
table(df_labels$gen_use_parent)

## AI use and awareness by demographics, use cases, time spent ####
questions <- c('gen_use_household', 'gen_use_child', 'gen_use_parent', 'gen_ai_desc', 'gen_ai_heard', 'gen_ai_talked', 'how_often')
demographic <- c('gen_use_child','region', 'region_aggregate', 'socialgrade','childgender','school_type', 'childage', 'learning_needs')
ai_use_demo = data_frame()
for (question in  questions[1])
{
  if (question == 'how_often')
    dd <- df_labels %>% filter(gen_use_child == 'Yes')
  else
    dd <- df_labels
  plot_list <- list()
  for (j in 1:4)
  {
    temp <- dd %>% group_by_at(c(question,demographic[j])) %>% summarise(n= n(), n_w = sum(weight)) %>% ungroup() %>% 
      group_by_at(demographic[j]) %>% 
      mutate(freq = round(n/sum(n)*100), freq_weighted = round(n_w/sum(n_w)*100), sum_n = sum(n)) %>%
      mutate(variable = demographic[j]) %>% filter(sum_n >= 10 )
    colnames(temp)[colnames(temp) == demographic[j]] <- "value"
    colnames(temp)[colnames(temp) == question] <- "choice"
    temp$question <- question
    temp$value <- paste(temp$value,", n=",temp$sum_n)
    if(question == 'how_often')
      temp$choice <- factor(temp$choice, 
                  levels = c("Every day", "A few times a week", "Once every few weeks", 
          "Once a month", "I have only used it a few times", "I have only used it once" , "I'm not sure")) 
    
    p <- ggplot(temp, aes(x= str_wrap(choice, width = 20), y=freq_weighted/100, fill = freq_weighted/100)) +
      geom_bar(stat='identity', color='black', width = 0.4) +
      facet_wrap(~value, scales = "fixed") + 
      scale_y_continuous(labels=scales::percent_format()) + 
      geom_text(aes(label = paste0(round(as.double(freq_weighted)), "%")), 
                position = position_stack(vjust = 1.2), 
                size = 3.5, color ="black") +  labs(
                  title = demographic[j],
                  x = "",
                  y = "% of all children"
                ) + common_theme + theme(
      axis.text.x = element_text(angle = 30, hjust = 1, size = 8))
    
    ai_use_demo <- rbind(ai_use_demo,temp) 
    plot_list[[j]] <- p
  }
  grid_plots <- do.call(arrangeGrob, c(plot_list))
  a <- grid.arrange(
    grid_plots,
    top = textGrob(get(question), gp = gpar(fontsize = 10, fontface = "bold"))
  )
  ggsave(paste0('../../Figures/', question, ".jpg"), plot = a)
}



## Feelings ####
df_labels[df_labels$child20_scary=="I don't find it scary/confusing", 'child20_scary'] <- "1- I don't find it scary/confusing"
child15 <- "Take a minute to read each of these sentences, and then choose from the following options:"
child_dict <- hash(
  keys = c('child20_scary', 'child20_exciting',"child15_1", 
           "child15_2", 'child15_3', "child15_4", "child15_5", 
           'child15_6', "child15_7", "child15_8", 'child15_9'),
  values = c('I find it scary', 'I find it exciting', 'Using the tool helps me express myself', 
             'I have used the tool to communicate something I had a hard time communicating on my own', 
             'I feel as though the tool understands the things I tell it', 
             'I feel that I can control the kind of things the the tool creates for me very well',
             'I feel like I understand how the tool works', 
             'I have used the tool to come up with new ideas',
             'the tool makes me feel more creative', 
             'If the tool tells me something, I believe it is always correct', 
             'I feel like I can share anything with the tool'))
basic_demographic <- c('childgender','school_type', 'childage', 'learning_needs')
ai_use_demo = data_frame()
for (question in  ls(child_dict))
{
  demographic <- basic_demographic
  if(grepl('child15', question))
    dd <- df_labels %>% filter(gen_use_child == 'Yes')
  else
  {
    dd <- df_labels
    demographic <- c(basic_demographic, 'gen_use_child')
  }
  plot_list <- list()
  for (j in 1:length(demographic))
  {
    temp <- dd %>% group_by_at(c(question,demographic[j])) %>% summarise(n= n(), n_w = sum(weight)) %>% ungroup() %>% 
        group_by_at(demographic[j]) %>% 
        mutate(freq = round(n/sum(n)*100), freq_weighted = round(n_w/sum(n_w)*100), sum_n = sum(n)) %>%
        mutate(variable = demographic[j]) %>% filter(sum_n >= 10 )     
    colnames(temp)[colnames(temp) == demographic[j]] <- "value"
    colnames(temp)[colnames(temp) == question] <- "choice"
    temp$question <- question
    temp$value <- paste(temp$value,", n=",temp$sum_n)
    p <- ggplot(temp, aes(x= str_wrap(choice, width = 30), y=freq_weighted/100, fill=freq_weighted)) +
      geom_bar(stat='identity', color='black', width = 0.4) +
      facet_wrap(~str_wrap(value, width=30)) + 
      scale_y_continuous(labels=scales::percent_format()) + 
      geom_text(aes(label = paste0(round(as.double(freq_weighted)), "%")), 
                position = position_stack(vjust = 1.2), 
                size = 3.5, color ="black") +  labs(
                  title = demographic[j],
                  x = "",
                  y = "% of all children"
                ) 
    p <- p + common_theme + theme(
      axis.text.x = element_text(angle = 20, hjust = 1)
    )
    ai_use_demo <- rbind(ai_use_demo,temp) 
    plot_list[[j]] <- p
  }
  ai_use_demo$question <- question
  grid_plots <- do.call(arrangeGrob, c(plot_list))
  a <- grid.arrange(
    grid_plots,
    top = textGrob(str_wrap(child_dict[[question]], width = 70), gp = gpar(fontsize = 10, fontface = "bold"))
  )
  ggsave(paste0('../../Figures/', question, ".jpg"), plot = a)
}
 #df_labels [df_labels$child20 == "I don't find it scary/confusing", 'value'] <- "1- I don't find it scary/confusing"


### Parents' feelings ####
parent15 <- "How concerned or unconcerned are you by each of the following? (All parents)" ## all parents
parent_dict <- hash(
  keys = c( 'parent13', 'parent15_1', 'parent15_2', 'parent15_3', 'parent15_4', 'parent15_5'), 
  values = c("To what extent do you feel positively or negatively about your child's use of generative AI? (parents of Gen-AI users)",
             "Your child could access inappropriate information",
             "Your child could be too trusting of generative AI and not think critically about the information it provides",
             "Your child could share personal information while using generative AI",
             "Your child could use generative AI to cheat in school",
             "Your child could access false / inaccurate information"))
basic_demographic <- c('childgender','school_type', 'childage', 'learning_needs')
for (question in  ls(parent_dict))
{
  demographic <- basic_demographic
  if (question == 'parent13')
    dd <- df_labels %>% filter(grepl("Yes", gen_use_parent))
  else
  {
    dd <- df_labels
    demographic <- c(basic_demographic, 'gen_use_parent')
  }
  plot_list <- list()
  for (j in 1:length(demographic))
  {
    temp <- dd %>% group_by_at(c(question,demographic[j])) %>% summarise(n= n(), n_w = sum(weight)) %>% ungroup() %>% 
      group_by_at(demographic[j]) %>% 
      mutate(freq = round(n/sum(n)*100), freq_weighted = round(n_w/sum(n_w)*100), sum_n = sum(n)) %>%
      mutate(variable = demographic[j]) %>% filter(sum_n >= 10 )     
    colnames(temp)[colnames(temp) == demographic[j]] <- "value"
    colnames(temp)[colnames(temp) == question] <- "choice"
    temp$question <- question
    temp$value <- paste(temp$value,", n=",temp$sum_n)
    p <- ggplot(temp, aes(x= str_wrap(choice, width = 30), y=freq_weighted/100, fill=freq_weighted)) +
      geom_bar(stat='identity', color='black', width = 0.4) +
      facet_wrap(~str_wrap(value, width=30)) + 
      scale_y_continuous(labels=scales::percent_format()) + 
      geom_text(aes(label = paste0(round(as.double(freq_weighted)), "%")), 
                position = position_stack(vjust = 0.7), 
                size = 3.5, color ="black") +  labs(
                  title = demographic[j],
                  x = "",
                  y = "% of all children"
                ) 
    p <- p + common_theme + theme(
      axis.text.x = element_text(angle = 20, hjust = 1)
    )
    ai_use_demo <- rbind(ai_use_demo,temp) 
    plot_list[[j]] <- p
  }
  ai_use_demo$question <- question
  grid_plots <- do.call(arrangeGrob, c(plot_list))
  a <- grid.arrange(
    grid_plots,
    top = textGrob(str_wrap(parent_dict[[question]], width = 70), gp = gpar(fontsize = 10, fontface = "bold"))
  )
  ggsave(paste0('../../Figures/', question, ".jpg"), plot = a)
}
#df_labels [df_labels$child20 == "I don't find it scary/confusing", 'value'] <- "1- I don't find it scary/confusing"

