setwd("/Users/rodney_sturdivant/Library/CloudStorage/Box-Box/SCOREbaylorhub/baylorapugit/mma/old")
load("fights_stack.Rdata")
library(dplyr)
player1 <- fights_stack |> 
  filter(stack == 1) |>
  select(-c("first_name", "last_name",  "Birthdate", "stack")) |>
  rename(
    p1_result = "fighter_result",  
    p1_id = "fighter_id",      
    p1_name = "fighter",        
    p1_nickname = "Nickname",        
    p1_country = "fighter_country",
    p1_sex = "sex",             
    p1_height = "Height",          
    p1_weight = "Weight",          
    p1_age = "fighter_age",    
    p1_reach ="Reach",           
    p1_stance = "Stance",          
    p1_wtClass = "WT Class",        
    p1_wtClass2 = "weight_class",   
    p1_team = "Team"            
  )
  
player2 <- fights_stack |> 
  filter(stack == 2) |> 
  select(-c("first_name", "last_name",  "Birthdate", "stack")) |>
  rename(
    p2_result = "fighter_result",  
    p2_id = "fighter_id",      
    p2_name = "fighter",        
    p2_nickname = "Nickname",        
    p2_country = "fighter_country",
    p2_sex = "sex",             
    p2_height = "Height",          
    p2_weight = "Weight",          
    p2_age = "fighter_age",    
    p2_reach ="Reach",           
    p2_stance = "Stance",          
    p2_wtClass = "WT Class",        
    p2_wtClass2 = "weight_class",   
    p2_team = "Team"            
  )

newdata <- bind_cols(player1, player2)
check <- newdata$date...1 == newdata$date...27
check <- newdata$event...5 == newdata$event...31
table(check)

mma_data <- newdata |>
  select(-c("date...27", "event_day...28", "event_month...29",
            "event_year...30", "event...31", "champ...32",          
            "decision...33", "decision_group...34", "round...35",
            "time...36",  "time_minutes...37", "time_seconds...38")) |>
  rename(date = "date...1", 
         day = "event_day...2",
         month = "event_month...3",
         year = "event_year...4", 
         event = "event...5",
         championship = "champ...6",
         decision = "decision...7",
         decision_group = "decision_group...8",
         round = "round...9",         
         time = "time...10",
         time_min = "time_minutes...11",
         time_sec = "time_seconds...12")  
  
save(mma_data, file = "mma.Rdata")
write_excel_csv(mma_data, file = "mma.csv")

test <- mma |> na.omit()
table(test$decision_group, test$p1_wtClass2)
