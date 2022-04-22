library(dplyr)
library(readr)

allzips <- readRDS("data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

myvars <- read.csv("data/myvars.csv") |>
  mutate(NAME = gsub("ZCTA5 ", "", NAME))

str(myvars)

allzips2years <- full_join(allzips, myvars, by = c("zipcode" = "NAME"))

# cleantable <- allzips %>%
#   select(
#     City = city.x,
#     State = state.x,
#     Zipcode = zipcode,
#     Rank = rank,
#     Score = centile,
#     Superzip = superzip,
#     Population = adultpop,
#     College = college,
#     Income = income,
#     Lat = latitude,
#     Long = longitude
#   )

cleantable <- allzips2years %>%
  select(
    Year = year,
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
    Rank = rank,
    Score = centile,
    Superzip = superzip,
    Population = adultpop,
    College = college,
    Black = prc_NHBlack,
    Asian = prc_NHAsian,
    Hispanic = prc_HispanicAn,
    Income = income,
    Renter = prc_renterocc_hh,
    Transit = prc_trans_tran,
    Employed = prc_employed,
    Poverty = prc_pov,
    Low_education = prc_educ_ltHS,
    Lat = latitude,
    Long = longitude
  )


