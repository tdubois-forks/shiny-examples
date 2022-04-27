library(readr)
library(stringr)

crosswalk <- read_csv("U:/Projects/R package - Grant Idea/GenePattern/ZiptoZcta_Crosswalk_2021.csv")
colnames(crosswalk)
data_input <- pat_count_zips_G1

# create_fact_var_table <- function(data_input){
#   data_input <- data_input |>
#     mutate(ZIP = as.character(ZIP),
#            ZIP = str_pad(ZIP, 5, "left", "0"))
#   # IF NOT, ATTACH IT HERE
#   if(fact_var != not_sel){
#     # HOW MANY JOINS DO WE HAVE BASED ON ZIP_CODE?
#     # state_data_ZC <- left_join(data_input, crosswalk, by = c("ZIP" = "ZIP_CODE"))
#     # complete_Zip_Code <- nrow(state_data_ZC[complete.cases(state_data_ZC),])
#     # sum(state_data_ZC$Pat_count) # good, correct number of pats
#     # HOW MANY JOINS DO WE HAVE BASED ON ZCTA?
#     # state_data_ZT <- left_join(data_input, crosswalk, by = c("ZIP" = "ZCTA"))
#     # complete_ZCTA <- nrow(state_data_ZT[complete.cases(state_data_ZT),])
#     # sum(state_data_ZT$Pat_count) # uh-oh. this resulted in too many pats
#
#     # if (complete_Zip_Code > complete_ZCTA) {
#       state_data <- left_join(data_input, crosswalk, by = c("ZIP" = "ZIP_CODE"))
#       # else
#       # state_data <- left_join(data_input, crosswalk, by = c("ZIP" = "ZCTA"))
#       state_data <- state_data |>
#         group_by(STATE)|>
#         summarize(Patients = sum(Pat_count))
#     }
#     # IF STATE IS INCLUDED, USE THAT
#   # else
#     state_data <- data_input |>
#       group_by(fact_var)|>
#       summarize(Patients = sum(Pat_count))
#
#   return(state_data)
# }

create_fact_var_table <- function(data_input){
  data_input <- data_input |>
    mutate(ZIP = as.character(ZIP),
           ZIP = str_pad(ZIP, 5, "left", "0"))
  # IF NOT, ATTACH IT HERE
  # if(fact_var == not_sel){
    state_data <- left_join(data_input, crosswalk, by = c("ZIP" = "ZIP_CODE"))

    state_data <- state_data |>
      group_by(STATE)|>
      summarize(Patients = sum(Pat_count))
  # }
  # else if(fact_var != not_sel){
  state_data <- data_input |>
    group_by(STATE)|>
    summarize(Patients = sum(Pat_count))
  # }
  state_data <- state_data|>
           arrange(-Patients)
state_data
sum(state_data$Patients)
return(state_data|>
           arrange(-Patients))
}

create_fact_var_table(data_input)



getwd()
write.csv(state_data_ZT, "U:/Projects/R package - Grant Idea/GenePattern/TestDataWState.csv")
create_fact_var_table(pat_count_zips_G1)


states <- pat_zips |>
  group_by(STATE) |>
  summarize(n = sum(Pat_count)) |>
  # distinct() |>
  filter(n > 0) |>
  arrange(-n)


sum(pat_count_zips_G1$Pat_count)
sum(state_table$Patients)

create_fact_var_table <- function(data_input, fact_var){
  if(fact_var != not_sel){
    freq_tbl <- data_input[,.N, by = get(fact_var)]
    freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
    freq_tbl
  }
}

create_fact_var_table(pat_count_zips_G1, )

fact_var_summary_table <- eventReactive(input$run_button,{
  create_fact_var_table(data_input(), fact_var())
})

output$fact_var_summary_table <- renderTable(fact_var_summary_table(),colnames = FALSE)
