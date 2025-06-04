library(tidyverse)
ninja <- read_csv("anw//kaplan_meier//anw_2021_stage1.csv")
ninja_obstacles <- distinct(ninja, obstacle_number, obstacle) |> 
  add_row(obstacle_number = 6, obstacle = "Warped Wall")
# 1               1 Slide Surfer   
# 2               2 Swinging Blades
# 3               3 Double Dipper  
# 4               4 Jumping Spider 
# 5               5 Tire Run       
# 6               6 Warped Wall
# 7               7 Dipping Birds  
# 8               8 The High Road  
# 9               8 Fly Hooks      
# 10               9 Cargo Net      
# 11              10 Complete 
ninja_new <- ninja |> 
  mutate(
    # obstacle_number = ifelse(cause %in% c("Time", "Complete"), 
    #                               obstacle_number - 1, obstacle_number), 
         obstacle = ifelse(cause %in% c("Time", "Complete"),
                           case_when(
                             obstacle_number == 1 ~ "Slide Surfer",
                             obstacle_number == 2 ~ "Swinging Blades",
                             obstacle_number == 3 ~ "Double Dipper",
                             obstacle_number == 4 ~ "Jumping Spider",
                             obstacle_number == 5 ~ "Tire Run",
                             obstacle_number == 6 ~ "Warped Wall",
                             obstacle_number == 7 ~ "Dipping Birds",
                             obstacle_number == 8 ~ "Fly Hooks",
                             obstacle_number == 9 ~ "Cargo Net",
                             TRUE ~ NA
                           ), obstacle))

flextable::flextable(ninja_new[c(1,50,42),])

write_csv(ninja_new, "anw/kaplan_meier/anw_2021_stage1.csv")

ninja2 <- read_csv("anw/kaplan_meier/anw_2023_stage1.csv")
ninja2_obstacles <- distinct(ninja2, obstacle_number, obstacle) |> 
  arrange(obstacle_number)
# obstacle_number obstacle           
# <dbl> <chr>              
# 1               2 Three Ring Circus  
# 2               3 Giant Rollercoaster
# 3               4 Jumping Spider     
# 4               5 Gambler            
# 5               6 Warped Wall        
# 6               7 Dipping Birds      
# 7               8 Thread the Needle  
# 8               9 Complete   
ninja2_new <- ninja2 |>
  mutate(obstacle_number = ifelse(cause %in% c("Time", "Complete"), 
                                  obstacle_number - 1, obstacle_number), 
         obstacle = ifelse(cause %in% c("Time", "Complete"),
                           case_when(
                             obstacle_number == 2 ~ "Three Ring Circus",
                             obstacle_number == 3 ~ "Giant Rollercoaster",
                             obstacle_number == 4 ~ "Jumping Spider",
                             obstacle_number == 5 ~ "Gambler",
                             obstacle_number == 6 ~ "Warped Wall",
                             obstacle_number == 7 ~ "Dipping Birds",
                             obstacle_number == 8 ~ "Thread the Needle",
                             T ~ NA),
                           obstacle))

write_csv(ninja2_new, "anw/kaplan_meier/anw_2023_stage1.csv")

                             