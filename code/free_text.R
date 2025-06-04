#Library imports
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

df <- read_csv("../../Data/Alan Turing Institute - Generative AI (children) - Labels 021224.csv")
df <- df[c('RecordNo','child1', 'Scenario1', 'Scenario2', 'Scenario3', 'Scenario4')]

## Scenario-1 ####
google <- df %>% filter(grepl("google", Scenario1, ignore.case = TRUE) | 
                     grepl("computer", Scenario1, ignore.case = TRUE) |
                     grepl("internet", df$Scenario1, ignore.case = TRUE) |
                       grepl("online", df$Scenario1, ignore.case = TRUE) |
                       grepl("search", df$Scenario1, ignore.case = TRUE)) %>% dplyr::select(Scenario1)

family <- df %>% filter(grepl("famil", df$Scenario1, ignore.case = TRUE) | 
                          grepl("parent", df$Scenario1, ignore.case = TRUE) |
                          grepl("dad", df$Scenario1, ignore.case = TRUE) |
                          grepl("mum", df$Scenario1, ignore.case = TRUE) |
                          grepl("mom", df$Scenario1, ignore.case = TRUE) | 
                          grepl("father", df$Scenario1, ignore.case = TRUE) | 
                          grepl("mother", df$Scenario1, ignore.case = TRUE)) %>% dplyr::select(Scenario1)

book <- df %>% filter(grepl("book", df$Scenario1, ignore.case = TRUE) | 
                        grepl("lib", df$Scenario1, ignore.case = TRUE)) %>% dplyr::select(Scenario1)

AI <- df %>% filter(grepl("AI", df$Scenario1, ignore.case = TRUE) | 
                          grepl("chat", df$Scenario1, ignore.case = TRUE) |
                          grepl("gpt", df$Scenario1, ignore.case = TRUE) |
                          grepl("alex", df$Scenario1, ignore.case = TRUE) |
                          grepl("bot", df$Scenario1, ignore.case = TRUE)) %>% dplyr::select(Scenario1)


## Scenario-4 ####
google <- df %>% filter(grepl("google", Scenario4, ignore.case = TRUE) | 
                          grepl("computer", Scenario4, ignore.case = TRUE) |
                          grepl("internet", df$Scenario4, ignore.case = TRUE) |
                          grepl("online", df$Scenario4, ignore.case = TRUE) |
                          grepl("search", df$Scenario4, ignore.case = TRUE)) %>% dplyr::select(Scenario4)

family <- df %>% filter(grepl("famil", df$Scenario4, ignore.case = TRUE) |
                          grepl("friend", df$Scenario4, ignore.case = TRUE) | 
                          grepl("parent", df$Scenario4, ignore.case = TRUE) |
                          grepl("dad", df$Scenario4, ignore.case = TRUE) |
                          grepl("mum", df$Scenario4, ignore.case = TRUE) |
                          grepl("mom", df$Scenario4, ignore.case = TRUE) | 
                          grepl("father", df$Scenario4, ignore.case = TRUE) | 
                          grepl("mother", df$Scenario4, ignore.case = TRUE)) %>% dplyr::select(Scenario4)

book <- df %>% filter(grepl("book", df$Scenario4, ignore.case = TRUE) | 
                        grepl("lib", df$Scenario4, ignore.case = TRUE)) %>% dplyr::select(Scenario4)

AI <- df %>% filter(grepl("AI", df$Scenario4, ignore.case = TRUE) | 
                      grepl("chat", df$Scenario4, ignore.case = TRUE) |
                      grepl("gpt", df$Scenario4, ignore.case = TRUE) |
                      grepl("alex", df$Scenario4, ignore.case = TRUE) |
                      grepl("bot", df$Scenario4, ignore.case = TRUE)) %>% dplyr::select(Scenario4)

social_media <- df %>% filter(grepl("social", df$Scenario4, ignore.case = TRUE) | 
                        grepl("media", df$Scenario4, ignore.case = TRUE) | 
                          grepl("tube", df$Scenario4, ignore.case = TRUE) |
                          grepl("tik", df$Scenario4, ignore.case = TRUE) |        
                          grepl("faceb", df$Scenario4, ignore.case = TRUE) | 
                          grepl("insta", df$Scenario4, ignore.case = TRUE)) %>% dplyr::select(Scenario4)


## Scenario-2 ####
traditional <- df %>% filter(grepl("paper", df$Scenario2, ignore.case = TRUE) | 
                          grepl("pen", df$Scenario2, ignore.case = TRUE) |
                          grepl("marker", df$Scenario2, ignore.case = TRUE) |
                          grepl("cray", df$Scenario2, ignore.case = TRUE) |
                            grepl("paint", df$Scenario2, ignore.case = TRUE) | 
                            grepl("draw", df$Scenario2, ignore.case = TRUE) |
                            grepl("hand", df$Scenario2, ignore.case = TRUE) |
                            grepl("design", df$Scenario2, ignore.case = TRUE)) %>% dplyr::select(Scenario2)

family <- df %>% filter(grepl("famil", df$Scenario2, ignore.case = TRUE) |
                          grepl("friend", df$Scenario2, ignore.case = TRUE) | 
                          grepl("parent", df$Scenario2, ignore.case = TRUE) |
                          grepl("dad", df$Scenario2, ignore.case = TRUE) |
                          grepl("mum", df$Scenario2, ignore.case = TRUE) |
                          grepl("mom", df$Scenario2, ignore.case = TRUE) | 
                          grepl("father", df$Scenario2, ignore.case = TRUE) | 
                          grepl("mother", df$Scenario2, ignore.case = TRUE)) %>% dplyr::select(Scenario2)

AI <- df %>% filter(grepl("AI", df$Scenario2, ignore.case = TRUE) | 
                      grepl("chat", df$Scenario2, ignore.case = TRUE) |
                      grepl("gpt", df$Scenario2, ignore.case = TRUE) |
                      grepl("alex", df$Scenario2, ignore.case = TRUE) |
                      grepl("bot", df$Scenario2, ignore.case = TRUE)) %>% dplyr::select(Scenario2)

digital <- df %>% filter(grepl("digital", df$Scenario2, ignore.case = TRUE) |
                           grepl("slide", df$Scenario2, ignore.case = TRUE) | 
                           grepl("word", df$Scenario2, ignore.case = TRUE) |
                           grepl("photo", df$Scenario2, ignore.case = TRUE) | 
                           grepl("shop", df$Scenario2, ignore.case = TRUE) | 
                           grepl("microsoft", df$Scenario2, ignore.case = TRUE) |
                           grepl("device", df$Scenario2, ignore.case = TRUE) |
                      grepl("power", df$Scenario2, ignore.case = TRUE) |
                        grepl("point", df$Scenario2, ignore.case = TRUE) |
                      grepl("canva", df$Scenario2, ignore.case = TRUE) |
                        grepl("adobe", df$Scenario2, ignore.case = TRUE) |
                        grepl("doc", df$Scenario2, ignore.case = TRUE) |
                        grepl("app", df$Scenario2, ignore.case = TRUE)) %>% dplyr::select(Scenario2)



robots <- sum(grepl("bot", df$child1, ignore.case = TRUE)==TRUE)- sum(grepl("chatbot", df$child1, ignore.case = TRUE)==TRUE)
computer <- sum(grepl("computer", df$child1, ignore.case = TRUE)==TRUE)
chatbot_chatgpt <- sum(grepl("chat", df$child1, ignore.case = TRUE)==TRUE)
write.xlsx(df, '../../Data/free_text.xlsx')
