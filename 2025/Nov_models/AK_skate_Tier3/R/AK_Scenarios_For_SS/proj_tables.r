proj_tables<-function(prof=profile, wid=1){
	library(dplyr)


	df_f <- data.frame(prof$Tables$F) %>%
  		dplyr::mutate(across(2:8, ~round(.x, 3)))

  	df_s <- data.frame(prof$Tables$SSB) %>%
  		dplyr::mutate(across(2:8, ~round(.x)))

  	df_c <- data.frame(prof$Tables$Catch) %>%
  		dplyr::mutate(across(2:8, ~round(.x)))


	proj_tables<-vector('list',length=3)

	proj_tables$F <- flextable::flextable(df_f) %>%
	                 flextable::bold(part='header') %>%
	                 flextable::colformat_num( j = 1, big.mark = "")%>%
	                 flextable::width(j = 1:8, width = wid)%>%
	                 flextable::set_header_labels(Yr='Year',scenario_1="Scenario 1",
	                 	scenario_2="Scenario 2",scenario_3="Scenario 3",scenario_4="Scenario 4",
	                 	scenario_5="Scenario 5", scenario_6="Scenario 6",scenario_7="Scenario 7")



	proj_tables$SSB <- flextable::flextable(df_s) %>%
	                 flextable::bold(part='header') %>%
	                 flextable::colformat_num( j = 1, big.mark = "")%>%
	                 flextable::colformat_num( j = 2:8, big.mark = ",")%>%
	                 flextable::width(j = 1:8, width = wid)%>%
	                 flextable::set_header_labels(Yr='Year',scenario_1="Scenario 1",
	                 	scenario_2="Scenario 2",scenario_3="Scenario 3",scenario_4="Scenario 4",
	                 	scenario_5="Scenario 5", scenario_6="Scenario 6",scenario_7="Scenario 7")

	proj_tables$catch <- flextable::flextable(df_c) %>%
	                 flextable::bold(part='header') %>%
	                 flextable::colformat_num( j = 1, big.mark = "")%>%
	                 flextable::colformat_num( j = 2:8, big.mark = ",")%>%
	                 flextable::width(j = 1:8, width = wid)%>%
	                 flextable::set_header_labels(Yr='Year',scenario_1="Scenario 1",
	                 	scenario_2="Scenario 2",scenario_3="Scenario 3",scenario_4="Scenario 4",
	                 	scenario_5="Scenario 5", scenario_6="Scenario 6",scenario_7="Scenario 7")

	proj_tables
}
