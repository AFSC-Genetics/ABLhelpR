#' Northern Boundary Sockeye data sheet formatting
#'
#' Convert excel file produced by ADFG into a usable formatted intake sheet.
#' @param infile The file you want to convert
#' @examples
#' NBsockeye_fmt(infile = "./DNA0047.xlsx")
#' @export
nbsockeye_fmt <- function(infile,
                      outfile){

  if(file.exists(infile)){
    infile <- file.path(infile)
  } else {
    cat(paste("File ", infile, " not found. Please check path and try again",sep=""))
  }

  if(!exists("outfile")){
    cat(paste("No outfile name given. Printing results to: ./",
              gsub(pattern="\\./",
                   replacement= "",
                   infile) %>% gsub(pattern="\\.|\\s",
                   replacement= "_",
                   .), ".csv",sep=""))
    outfile <- paste("./",
                     gsub(pattern="\\./",
                          replacement= "",
                          infile) %>% gsub(pattern="\\.|\\s",
                                           replacement= "_",
                                           .), ".csv",sep="")
  } else {
    outfile <- file.path(outfile)
  }

  #read in data as character; we are only going to manipulate a few columns
  ADFGdat <- suppressMessages(readxl::read_excel(infile,
                                                 col_types=c("text")))

  WhtCrd <- ADFGdat$`Dna Specimen No` %>%
    {gsub('.{2}$', '', .)} %>% #remove the last two numbers and replace with nada
    {gsub('^0+|^10+', '', .)} #remove the start of the DNA specimen number

  SclCrd <- ADFGdat$`Scale Card No`

  WhtPos <- ADFGdat$`Dna Specimen No` %>%
    str_sub(.,-2,-1) %>%
    as.numeric()


  ABLdat <- data.frame(`ABL Sample ID` = "",
                       `DNA Plate ID` =  "",
                        Well = "",
                       `Printed Whatman card ID (leading zeros removed)` = WhtCrd,
                      `ADFG-designated scale card number` =    SclCrd,
                      `Position on Whatman card` = WhtPos,
                      `Sample ID for tissue plating` = paste(WhtCrd,
                                                             SclCrd,
                                                             WhtPos,
                                                             sep="_"))


  write.csv(file = paste("./",outfile,".csv"),
            ABLdat,
            row.names = F,
            quote=F)
}
