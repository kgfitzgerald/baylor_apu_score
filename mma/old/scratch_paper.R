# [1] "date"           "month"          "year"           "event"         
# [5] "championship"   "decision"       "decision_group" "round"         
# [9] "time"           "p1_result"      "p1_id"          "p1_name"       
# [13] "p1_country"     "p1_sex"         "p2_result"      "p2_id"         
# [17] "p2_name"        "p2_country"     "p2_sex"         "wtClass"  

test <- mma |> 
  filter(year == 2023) |>
  #filter(p1_sex == "F") |>
  select(year, p1_sex, decision_group) |> 
  # filter(wtClass == "Flyweight" | 
  #          wtClass == "Bantamweight" |
  #          wtClass == "Strawweight" |
  #          wtClass == "Atomweight") |> 
  # mutate(decision = if_else(decision_group == "Decision", "Decision",
                            # "KO/TKO/Submission"))

table(test$p1_sex, test$decision_group)
test.chisq <- chisq.test(test$p1_sex, test$decision_group)
test.chisq$expected
test.chisq
fisher.test(test$p1_sex, test$decision_group)


