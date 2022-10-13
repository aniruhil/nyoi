library(tidyverse)

haven::read_sav(
  "~/Downloads/Ohio data request/NYOI 2019 Ohio.sav"
  ) -> nyoi

haven::read_sav(
  "~/Downloads/Ohio data request/yrbss2019.sav"
  ) -> yrbss19

nyoi %>%
  mutate(
    bcgroup = 1,
    dob = paste(YEAR, MONTH, DAY, sep = "-"),
    dob = lubridate::ymd(dob),
    age_in_years = trunc(
      as.numeric(
        difftime(
          as.Date("2022-01-01"),
          dob,
          units = "days"
        )
      ) / 365.25
    )
  ) -> nyoi

nyoi %>%
  relocate(dob, age_in_years) -> nyoi

nyoi %>%
  filter(
    age_in_years >= 13 & age_in_years <= 19
    ) %>%
  mutate(
    Q1 = case_when(
      age_in_years <= 12 ~ "12 years old or younger",
      age_in_years == 13 ~ "13 years old",
      age_in_years == 14 ~ "14 years old",
      age_in_years == 15 ~ "15 years old",
      age_in_years == 16 ~ "16 years old",
      age_in_years == 17 ~ "17 years old",
      age_in_years >= 18 ~ "18 years old or older"
    ),
    Q2 = case_when(
      GENDER == 1 ~ "Female",
      GENDER == 2 ~ "Male"
    ),
    QN17 = case_when(
      FIGHTING == 0 ~ "No Fights",
      FIGHTING == 1 ~ "One or More Fights"
    )
  ) -> nyoi

nyoi %>%
  count(age_in_years)

nyoi %>%
  count(GENDER, Q2)

yrbss19 %>%
  mutate(
    bcgroup = 0
  ) -> yrbss19

yrbss19 %>%
  count(Q5)

nyoi %>%
  select(Q1, Q2, bcgroup, QN17) -> df01

yrbss19 %>%
  filter(
    Q1 >= 1) %>%
  mutate(
    QN17 = factor(
      QN17,
      levels = c(1, 2),
      labels = c("One or More Fights", "No Fights")
    )
  ) %>%
  select(Q1, Q2, bcgroup, QN17) -> df02

df01 %>%
  mutate(
    Q1 = as.factor(Q1),
    Q2 = as.factor(Q2),
    QN17 = as.factor(QN17)
  ) -> df01

df02$Q1 = factor(df02$Q1,
                    levels = c(2:7),
                    labels = c(
                      "13 years old",
                      "14 years old",
                      "15 years old",
                      "16 years old",
                      "17 years old",
                      "18 years old or older"
                      )
                    )

df02$Q2 = factor(df02$Q2,
                 levels = c(1,2),
                 labels = c("Female", "Male")
                 )

bind_rows(df01, df02) -> df_mat

df_mat %>%
  mutate(
    bcgroup = factor(
      bcgroup,
      levels = c(0, 1),
      labels = c("Not in BC", "In BC")
      )
    ) -> df_mat

library(MatchIt)

# No matching; constructing a pre-match matchit object
matchit(
  bcgroup ~ Q1 + Q2,
  data = na.omit(df_mat),
  method = NULL,
  distance = "glm"
  ) -> mout0

summary(mout0)


matchit(
  bcgroup ~ Q1 + Q2,
  data = na.omit(df_mat),
  method = "nearest",
  distance = "glm"
  ) -> mout1

summary(mout1)

matchit(
  bcgroup ~ Q1 + Q2,
  data = na.omit(df_mat),
  method = "exact",
  distance = "glm"
) -> mout2

summary(mout2)

matchit(
  bcgroup ~ Q1 + Q2,
  data = na.omit(df_mat),
  method = "optimal",
  distance = "glm"
) -> mout3

summary(mout3)

matchit(
  bcgroup ~ Q1 + Q2,
  data = na.omit(df_mat),
  method = "full",
  distance = "glm"
) -> mout4

summary(mout4)

matchit(
  bcgroup ~ Q1 + Q2,
  data = na.omit(df_mat),
  method = "genetic",
  distance = "glm"
) -> mout5

summary(mout5)

matchit(
  bcgroup ~ Q1 + Q2,
  data = na.omit(df_mat),
  method = "cem",
  distance = "glm"
) -> mout6

summary(mout6)

plot(mout6)

match.data(mout6) -> mydf

glm(QN17 ~ Q1 + Q2 + bcgroup, data = mydf,
                family = binomial(link = "logit"),
                weights = weights) -> mout6.logit

summary(mout6.logit)



# 7, 35, 36, 37 (maybe),

# Outcome 1 will be PA_60M_T

haven::read_sav("~/Downloads/MergedFilesNYOIYRBSS.sav") -> sroi

sroi$tcgroup = sjmisc::to_value(sroi$NYOICase1)
sroi$age = sjmisc::to_value(sroi$AGENew)
sroi$gender = sjmisc::to_value(sroi$GENDER)
sroi$race = sjmisc::to_value(sroi$Racebinary)
sroi$fighting = sjmisc::to_value(sroi$FIGHT)
sroi$cigarettes = sjmisc::to_value(sroi$CIG)
sroi$vaping = sjmisc::to_value(sroi$VAPE)
sroi$drinking = sjmisc::to_value(sroi$ALCOHOL)
sroi$marijuana = sjmisc::to_value(sroi$MARIJ)
sroi$activity = sjmisc::to_value(sroi$ACTIVE)
sroi$presdrugs = sjmisc::to_value(sroi$RX)

sroi %>%
  mutate(
    tcgroup = factor(
      tcgroup,
      levels = c(0, 1),
      labels = c("YRBSS", "NYOI"))
    ) -> sroi

save(sroi, file = "~/Downloads/sroi.RData")

sroi %>%
  filter(age != 19) -> mydf01

mydf01 %>%
  select(14:24) -> mydf02

summary(mydf02)

na.omit(mydf02) -> brutedf

brutedf %>%
  mutate(
    age = factor(age),
    gender = factor(gender),
    race = factor(race, levels = c(0, 1), labels = c("White", "Non-White"))
  ) -> brutedf

brutedf %>%
  filter(!is.na(race)) -> brutedf2

brutedf2 %>%
  group_by(tcgroup) %>%
  count()

matchit(
  tcgroup ~ age + gender + race,
  data = brutedf2,
  method = NULL,
  distance = "glm"
  ) -> mout0

summary(mout0)

matchit(
  tcgroup ~ age + gender + race,
  data = na.omit(brutedf),
  method = "genetic"
  ) -> mout1

summary(mout1)

matchit(
  tcgroup ~ age + gender + race,
  data = brutedf2,
  method = "cem"
  ) -> mout2

summary(mout2)

plot(mout1, type = "qq")

plot(mout1, type = "density")

plot(mout2, type = "qq")

plot(mout2, type = "density")

match.data(mout2) -> mout2.df

glm(QN17 ~ Q1 + Q2 + bcgroup, data = mydf,
    family = binomial(link = "logit"),
    weights = weights) -> mout6.logit

summary(mout6.logit)


brutedf2 %>%
  group_by(fighting) %>%
  count()

brutedf2 %>%
  group_by(activity) %>%
  count()

brutedf2 %>%
  group_by(drinking) %>%
  count()

brutedf2 %>%
  group_by(presdrugs) %>%
  count()

brutedf2 %>%
  group_by(cigarettes) %>%
  count()

brutedf2 %>%
  group_by(marijuana) %>%
  count()

lm(
  activity ~ age + gender + race + tcgroup,
  data = mout2.df,
  weights = weights
  ) -> mout2.lm

summary(mout2.lm)

lm(
  fighting ~ tcgroup,
  data = mout2.df,
  weights = weights
) -> mout2.lm.bad

summary(mout2.lm.bad)

glm(
  fighting ~ age + gender + race + tcgroup,
  data = mout2.df,
  weights = weights,
  family = binomial(link = "logit")
  ) -> mout2.logit

summary(mout2.lm)
