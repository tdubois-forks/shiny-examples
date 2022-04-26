library(readr)

crosswalk <- read_csv("U:/Projects/R package - Grant Idea/GenePattern/ZiptoZcta_Crosswalk_2021.csv")
data_input <- pat_count_zips_G1

create_fact_var_table <- function(data_input, fact_var){
  data_input <- data_input |>
    mutate(ZIP = as.character(ZIP))
  # IF NOT, ATTACH IT HERE
  if(fact_var != not_sel){
    state_data <- left_join(data_input, crosswalk, by = c("ZIP" = "ZCTA")) |>
      group_by(STATE)|>
      summarize(Patients = sum(Pat_count))
    # IF STATE IS INCLUDED, USE THAT
  else
    freq_tbl <- data_input[,.N, by = get(fact_var)]
    freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
    freq_tbl
  }
}




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
