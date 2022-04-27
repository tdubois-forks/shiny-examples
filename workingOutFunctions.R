library(readr)
library(stringr)

crosswalk <- read_csv("U:/Projects/R package - Grant Idea/GenePattern/ZiptoZcta_Crosswalk_2021.csv")
colnames(crosswalk)
data_input <- pat_count_zips_G1




# create_state_table <- function(data_input, state){
  data_input <- data_input |>
    mutate(ZIP = as.character(ZIP),
           ZIP = str_pad(ZIP, 5, "left", "0"))
  # IF NOT, ATTACH IT HERE
  # if(state == not_sel){
    state_data <- left_join(data_input, crosswalk, by = c("ZIP" = "ZIP_CODE"))|>
      group_by(STATE)|>
      summarize(Patients = sum(Pat_count))|>
      arrange(-Patients) |>
      setNames(c("State", "Count of patients"))
  # }
  # else if(state != not_sel){
    state_data <- data_input |>
      group_by(state)|>
      summarize(Patients = sum(Pat_count))|>
      arrange(-Patients) |>
      setNames(c("State", "Count of patients"))
  # }
  # return(state_data)
# }







TestDataWState <- read_csv("U:/Projects/R package - Grant Idea/GenePattern/TestDataWState.csv")
data_input <- TestDataWState
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
    group_by(STATE)|>
    summarize(Patients = sum(Pat_count))|>
    arrange(-Patients) |>
    setNames(c("State", "Count of patients"))
  # }
  return(state_data)
}

create_fact_var_table(data_input)



getwd()
write.csv(state_data_ZC, "U:/Projects/R package - Grant Idea/GenePattern/TestDataWState.csv")
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
