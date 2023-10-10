sql_read <- function(x) {
  if(file.exists(system.file("sql", x, package = "swo"))) {
    readLines(system.file("sql", x, package = "swo"))
  } else {
    stop("The sql file does not exist.")
  }
}

collapse_filters <- function(x) {
  sprintf("'%s'", paste(x, collapse = "','"))
  }

sql_add <- function(x, sql_code, flag = "-- insert table") {

  i = suppressWarnings(grep(flag, sql_code))
  sql_code[i] <- x
  sql_code
}


sql_filter <- function(sql_precode = "=", x, sql_code, flag = "-- insert species") {
  
  i = suppressWarnings(grep(flag, sql_code))
  sql_code[i] <- paste0(
    sql_precode, " (",
    collapse_filters(x), ")"
  )
  sql_code
}

sql_run <- function(database, query) {
  query = paste(query, collapse = "\n")
  DBI::dbGetQuery(database, query, as.is=TRUE, believeNRows=FALSE)
}

WED<-function(x)
   { y<-data.table(
     weekday=weekdays(x),
     wed=ceiling_date(x, "week"),  
     plus= ifelse(weekdays(x) %in% c("Sunday"), 6, -1),
     YR=year(x))
     y$next_saturday<-date(y$wed)+y$plus
     y[YR<1993]$next_saturday<-date(y[YR<1993]$wed)
     y$yr2<-year(y$next_saturday)
     y[YR!=yr2]$next_saturday<-date(paste0(y[YR!=yr2]$YR,"-12-31"))
     return(y$next_saturday)
}


fuzzy_dates<-function(length_data=NAPBCOMB,Fish_ticket=PBFTCKT3,ndays=7){
      x<-length_data[,c('DELIVERING_VESSEL','DELIVERY_DATE')]
      y <- Fish_ticket
      y$ID<-1:nrow(y)
      y2<-y[,c("ID","DELIVERING_VESSEL","DELIVERY_DATE")]
      colnames(y2)[which(names(y2) == "DELIVERY_DATE")] <- "DELIVERY_DATE2"

  # join all combos
      x2 = full_join(x, y2, by = "DELIVERING_VESSEL")
  # just resort the data
      x2 = x2 %>% 
      arrange(DELIVERING_VESSEL, DELIVERY_DATE, DELIVERY_DATE2)
  # get absolute difference in datNAPe
      x2 = x2 %>% 
      mutate(date_diff = abs(as.Date(DELIVERY_DATE)- as.Date(DELIVERY_DATE2)))
  # subset only those that have a match
      x3<-subset(x2,!is.na(DELIVERY_DATE)&!is.na(DELIVERY_DATE2))

  # only pull the matches with the lowest difference in date
      x4 <- x3 %>% 
      group_by(DELIVERING_VESSEL, DELIVERY_DATE) %>% 
      slice(which.min(date_diff))

  # get rid of any matches greater than ndays
      x4<-subset(x4,date_diff <= ndays)
  #merge back with original length data
      x5<-merge(x,x4,by=c('DELIVERING_VESSEL','DELIVERY_DATE'), all.x=T)
      x5.1 <- subset(x5,!is.na(ID))
      x5.1=unique(x5.1[,c("DELIVERING_VESSEL","DELIVERY_DATE","ID","date_diff")])
      x6 <- merge(x5.1,y,by='ID')
      names(x6)[2:3]<-c("DELIVERING_VESSEL","DELIVERY_DATE")
      x6<-subset(x6,select=-c(DELIVERY_DATE.y,DELIVERING_VESSEL.y))
      x7<-merge(length_data,x6,by=c("DELIVERING_VESSEL","DELIVERY_DATE"),all.x=T)
      colnames(x7)[which(names(x7) == "FISH_TICKET_NO.x")] <- "FISH_TICKET_NO"
      x7$DELIVERY_DATE.y<-x7$DELIVERY_DATE
      x7$DELIVERING_VESSEL.y<-x7$DELIVERING_VESSEL
      return(x7)
}

