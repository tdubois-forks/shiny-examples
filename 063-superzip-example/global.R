library(dplyr)
library(readr)


# CALL IN WHAT WE NEED

allzips <- readRDS("U:/Projects/R package - Grant Idea/GenePattern/shiny-examples/063-superzip-example/data/superzip.rds") |>
  mutate(latitude = jitter(allzips$latitude),
         longitude = jitter(allzips$longitude),
         college = college * 100,
         zipcode = formatC(allzips$zipcode, width=5, format="d", flag="0"))

row.names(allzips) <- allzips$zipcode

myvars <- read.csv("U:/Projects/R package - Grant Idea/GenePattern/shiny-examples/063-superzip-example/data/myvars.csv") |>
  mutate(NAME = gsub("ZCTA5 ", "", NAME))

allzips2years <- full_join(allzips, myvars, by = c("zipcode" = "NAME"))

cleantable <- allzips2years %>%
  select(
    Year = year,
    City = city.x,
    State = state.x,
    Zipcode = zipcode,
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

cleantable19 <- cleantable |>
  filter(Year == 2019)

# THESE ARE FUNCTIONS PASTED FROM HERE: https://github.com/MatePocs/rshiny_apps/blob/main/data_analyser/app.R

not_sel <- "Not Selected"


create_state_table <- function(data_input, state){
  # data_input <- data_input |>
  #   mutate(ZIP = as.character(ZIP),
  #          ZIP = str_pad(ZIP, 5, "left", "0"))
  # # IF NOT, ATTACH IT HERE
  # if(state == not_sel){
  #   state_data <- left_join(data_input, crosswalk, by = c("ZIP" = "ZIP_CODE"))
  #
  #   state_data <- state_data |>
  #     group_by(STATE)|>
  #     summarize(Patients = sum(Pat_count))
  # }
  # else if(state != not_sel){
  state_data <- data_input |>
    group_by(state)|>
    summarize(Patients = sum(Pat_count))|>
    arrange(-Patients) |>
    setNames(c("State", "Count of patients"))
  # }
  return(state_data)
}

# create_state_table <- function(data_input){
#   data_input <- data_input |>
#     mutate(ZIP = as.character(ZIP),
#            ZIP = str_pad(ZIP, 5, "left", "0"))
#   # IF NOT, ATTACH IT HERE
#   if(state == not_sel){
#     # HOW MANY JOINS DO WE HAVE BASED ON ZIP_CODE?
#     state_data_ZC <- left_join(data_input, crosswalk, by = c("ZIP" = "ZIP_CODE"))
#     complete_Zip_Code <- nrow(state_data_ZC[complete.cases(state_data_ZC),])
#     # HOW MANY JOINS DO WE HAVE BASED ON ZCTA?
#     state_data_ZT <- left_join(data_input, crosswalk, by = c("ZIP" = "ZCTA"))
#     complete_ZCTA <- nrow(state_data_ZT[complete.cases(state_data_ZT),])
#     if (complete_Zip_Code > complete_ZCTA) {
#       state_data <- left_join(data_input, crosswalk, by = c("ZIP" = "ZIP_CODE"))
#     }
#       else if (complete_Zip_Code < complete_ZCTA){
#       state_data <- left_join(data_input, crosswalk, by = c("ZIP" = "ZCTA"))
#
#       state_data <- state_data |>
#         group_by(STATE)|>
#         summarize(Patients = sum(Pat_count))
#
#       state_table <- state_data|>
#         filter(Patients > 0) |>
#         arrange(-Patients)
#       }
#   }
#     # IF STATE IS INCLUDED, USE THAT
#     else if (state != not_sel){
#       state_data <- data_input |>
#       group_by(state)|>
#       summarize(Patients = sum(Pat_count))
#
#       state_table <- state_data|>
#         filter(Patients > 0) |>
#         arrange(-Patients)
#     }
#   return(state_table)
# }



draw_plot_1 <- function(data_input){
  # HERE I SAY ZIP, BUT IT SHOULD BE THE INPUT WE DESIGNATE AS ZIP
  data_input <- data_input |>
    mutate(ZIP = as.character(ZIP))
  # FILTER FOR THE YEAR TO USE
  cleantable19 <- cleantable |>
    filter(Year == 2019)
  # JOIN THE NEIGHBORHOOD VARS TO MY UPLOADED DATA, AND CALCULATE THE POVERTY CATEGORIES
  joined_data <- left_join(data_input, cleantable19, by = c("ZIP" = "Zipcode")) |>
    mutate(p_level_4 = cut(Poverty, breaks = c(0,10,20,30,100),
                           labels = c("Low (<10%)", "Medium (10-20%)", "High (20-30%)", "Very High (>30%)")))
  # COUNT OF PATIENTS PER POV LEVEL - PULL FROM OTHER CODE TO MAKE THIS FANCIER- THIS IS A PLACEHOLDER FOR NOW
  pov_groups <- joined_data |>
    group_by(p_level_4)|>
    summarize(n = sum(Pat_count))
  # SAME AS ABOVE- THIS IS A PLACEHOLDER
  ggplot(data = as.data.frame(pov_groups),
         aes(x = p_level_4, y = n)) +
    geom_bar(stat="identity")
}

create_num_var_table <- function(data_input, num_var){
  if(num_var != not_sel){
    col <- data_input[,get(num_var)]
    if (length(col)>5000) col_norm <- sample(col,5000) else col_norm <- col
    norm_test <- shapiro.test(col_norm)
    statistic <- c("mean", "median", "5th percentile", "95th percentile",
                   "Shapiro statistic", "Shapiro p-value")
    value <- c(round(mean(col),2), round(median(col),2),
               round(quantile(col, 0.05),2), round(quantile(col, 0.95),2),
               norm_test$statistic, norm_test$p.value)
    data.table(statistic, value)
  }
}

create_state_table <- function(data_input, state){
  if(state != not_sel){
    freq_tbl <- data_input[,.N, by = get(state)]
    freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
    freq_tbl
  }
}

create_combined_table <- function(data_input, num_var_1, num_var_2, state){
  if(state != not_sel){
    if(num_var_1 != not_sel & num_var_2 != not_sel){
      res_tbl <- data_input[,.(correlation = cor(get(num_var_1), get(num_var_2))), by = state]
    }
    else if(num_var_1 != not_sel & num_var_2 == not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_1))), by = state]
    }
    else if(num_var_1 == not_sel & num_var_2 != not_sel){
      res_tbl <- data_input[,.(mean = mean(get(num_var_2))), by = state]
    }
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel){
    res_tbl <- data.table(
      statistic = c("correlation"),
      value = c(cor(
        data_input[,get(num_var_1)],
        data_input[,get(num_var_2)])))
  }
  return(res_tbl)
}
