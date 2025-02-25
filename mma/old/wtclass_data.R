# Atomweighth 105 lb
# Strawweight	115 lb (52.2 kg)
# Flyweight	125 lb (56.7 kg)
# Bantamweight	135 lb (61.2 kg)
# Featherweight	145 lb (65.8 kg)
# Lightweight	155 lb (70.3 kg)
# Super lightweight	165 lb (74.8 kg)
# Welterweight	170 lb (77.1 kg)
# Super welterweight	175 lb (79.4 kg)
# Middleweight	185 lb (83.9 kg)
# Super middleweight	195 lb (88.5 kg)
# Light heavyweight	205 lb (93.0 kg)
# Cruiserweight	225 lb (102.1 kg)
# Heavyweight	265 lb (120.2 kg)
# Super heavyweight	

mma <- readr::read_csv("mma.csv")

mma <- mma |>
  select(-c(p1_nickname, p2_nickname, p1_stance, p1_team,
            p2_stance, p2_team, p1_wtClass, p2_wtClass
  )) |> na.omit()

colSums(is.na(mma))

# Atomweight 1 Strawweight 2 Flyweight  3 
# Bantamweight 4   Featherweight 5 Lightweight 6
# Welterweight 8 Middleweight 10 Heavyweight 14   
#   Catchweight 15 Open Weight 16 - classes outside MMA or diff wts

diffs <- mma$p1_wtClass2 == mma$p2_wtClass2
table(diffs)

open <- mma |> filter(mma$p2_wtClass2 == "Open Weight" & 
                        mma$p1_wtClass2 == "Catchweight")

mma_new <- mma |> mutate(
  class1 = case_when(p1_wtClass2 == "Atomweight" ~ 1,
            p1_wtClass2 == "Strawweight" ~ 2,
            p1_wtClass2 == "Flyweight" ~ 3,
            p1_wtClass2 == "Bantamweight" ~ 4,
            p1_wtClass2 == "Featherweight" ~ 5,
            p1_wtClass2 == "Lightweight" ~ 6,
            p1_wtClass2 == "Welterweight" ~ 7,
            p1_wtClass2 == "Middleweight" ~ 8,
            p1_wtClass2 == "Heavyweight" ~ 9,
            p1_wtClass2 == "Catchweight" ~ 10,
            p1_wtClass2 == "Open Weight" ~ 11),
  class2 = case_when(p2_wtClass2 == "Atomweight" ~ 1,
                     p2_wtClass2 == "Strawweight" ~ 2,
                     p2_wtClass2 == "Flyweight" ~ 3,
                     p2_wtClass2 == "Bantamweight" ~ 4,
                     p2_wtClass2 == "Featherweight" ~ 5,
                     p2_wtClass2 == "Lightweight" ~ 6,
                     p2_wtClass2 == "Welterweight" ~ 7,
                     p2_wtClass2 == "Middleweight" ~ 8,
                     p2_wtClass2 == "Heavyweight" ~ 9,
                     p2_wtClass2 == "Catchweight" ~ 10,
                     p2_wtClass2 == "Open Weight" ~ 11)
  )

mma_new <- mma_new |>
  mutate(
    wtClass = case_when(
      class1 > class2 ~ p1_wtClass2,
      .default = p2_wtClass2
    )
  )

mma <- mma_new |> select(-c(p1_wtClass2, p2_wtClass2, class2, class1))

write_excel_csv(mma, file = "mma.csv")



