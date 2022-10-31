#' DNA quantification data formatting
#'
#' Convert excel file produced by Synergy H1 into a long format table.
#' @param infile The file you want to convert
#' @examples
#' quant_fmt(infile = "./DNA0047.xlsx")
#' @export
quant_fmt <- function(infile,
                      outfile){

if(file.exists(infile)){
infile <- file.path(infile)
} else {
  cat(paste("File ", infile, " not found. Please check path and try again",sep=""))
}

if(!exists("outfile")){
  cat(paste("No outfile name given. Printing results to: ",
            gsub(pattern="\\.",
                 replacement= "_",
                 infile), ".csv",sep=""))
  } else {
    outfile <- file.path(outfile)
  }

Wells<- paste(LETTERS[seq(from=1,to=8)],rep(1:12,each=8),sep="")

Samps<-suppressMessages(readxl::read_excel(infile,
                  range= "C28:N35",
                  col_names = F)) %>%
  unlist() %>%
  as.vector()


Concn <- suppressMessages(readxl::read_excel(infile,
                   range= "C41:N56",
                   col_names = F))%>%
  .[rep(c(T,F),8),] %>%
  unlist() %>%
  as.vector()


ExcEmss <- suppressMessages(readxl::read_excel(infile,
                         range= "C41:N56",
                         col_names = F))%>%
  .[rep(c(F,T),8),] %>%
  unlist() %>%
  as.vector()


BindDat <- data.frame(Well = Wells,
                      SampID = Samps,
                      Concentration = Concn,
                      Excitation_Emission = ExcEmss)

write.csv(file = paste("./",outfile,".csv"),
          BindDat,
          row.names = F,
          quote=F)
}
