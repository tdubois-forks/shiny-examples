library(dplyr)
library(readr)


not_sel <- "Not Selected"


allzips <- readRDS("U:/Projects/R package - Grant Idea/GenePattern/shiny-examples/063-superzip-example/data/superzip.rds")
allzips$latitude <- jitter(allzips$latitude)
allzips$longitude <- jitter(allzips$longitude)
allzips$college <- allzips$college * 100
allzips$zipcode <- formatC(allzips$zipcode, width=5, format="d", flag="0")
row.names(allzips) <- allzips$zipcode

myvars <- read.csv("U:/Projects/R package - Grant Idea/GenePattern/shiny-examples/063-superzip-example/data/myvars.csv") |>
  mutate(NAME = gsub("ZCTA5 ", "", NAME))

allzips2years <- full_join(allzips, myvars, by = c("zipcode" = "NAME"))
# allzips <- full_join(allzips, myvars[which(myvars$year == "2019"),], by = c("zipcode" = "NAME"))


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
    # Rank = rank,
    # Score = centile,
    # Superzip = superzip,
    Population = adultpop,
    Black = prc_NHBlack,
    Asian = prc_NHAsian,
    Hispanic = prc_HispanicAn,
    Income = income,
    Renter = prc_renterocc_hh,
    Transit = prc_trans_tran,
    Employed = prc_employed,
    Poverty = prc_pov,
    College = college,
    'Low education' = prc_educ_ltHS,
    Lat = latitude,
    Long = longitude
  )

str(cleantable)
