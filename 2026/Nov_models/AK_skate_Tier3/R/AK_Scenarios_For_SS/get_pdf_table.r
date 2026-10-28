#' Function to pull the tables from a pdf document
#' @param url is the location of the assessment
#' @param page is the pages in the document you wish to scrape for tables
#' @export a list of tables


get_pdf_tables<-function(url="https://apps-afsc.fisheries.noaa.gov/Plan_Team/2022/EBSpcod.pdf",page=c(1:128)){
   download.file(url, destfile = "document.pdf", mode = "wb")
   tables <- tabulizer::extract_tables("document.pdf",pages=page)
   } 