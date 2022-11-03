#' Convert microsatellite genotype calls
#'
#' Convert ABL to DFO microsatellite sizes.
#' @param infile The file you want to convert
#' @examples
#' ABL2DFO(Infile = "./Chum2010genosABL.csv", GenoStartCol = 2, AlleleConversions = "./ChumAlleleConversion.csv", Outfile = "./Chum2010genosDFO.csv", rows2skip=0)
#' @export
ABL2DFO <- function(Infile,
                    GenoStartCol,
                    AlleleConversions,
                    Outfile,
                    DataSheet,
                    rows2skip=0){
library(doFuture)
  if(unlist(stringr::str_split(Infile,"\\."))[length(unlist(stringr::str_split(Infile,"\\.")))]=="csv"){
    dat <- read.csv(Infile,stringsAsFactors = F,header=T)
  } else if (unlist(stringr::str_split(Infile,"\\."))[length(unlist(stringr::str_split(Infile,"\\.")))]=="xlsx"){
    dat <- readxl::read_excel(Infile, sheet = DataSheet,skip = rows2skip)
  }

  Conversions<- read.csv(AlleleConversions,stringsAsFactors = F)
  loci<-colnames(dat)[GenoStartCol:ncol(dat)][c(T,F)]
  nloci<-length(loci)

  #Subset on just the GenoData
  dat2 <- dat %>%
    dplyr::select(all_of(GenoStartCol:((GenoStartCol-1)+nloci*2)))

  # now we want to convert alleles by locus
  registerDoFuture()
  plan(multisession)
  X <- 1:nloci

  AllelesConverted <- foreach(x = X) %dopar% {
    dattemp<-dat2 %>%
      dplyr::select(colnames(dat2)[grep(x=colnames(dat2),loci[x])])

    ConvertTemp <- Conversions[grep(x=Conversions[,1],pattern=loci[x]),]

    dattemp[,1] <- plyr::mapvalues(x=unlist(dattemp[,1]),
                                   from = ConvertTemp[,2],
                                   to = ConvertTemp[,3],
                                   warn_missing = F)
    dattemp[,2] <- plyr::mapvalues(x=unlist(dattemp[,2]),
                                   from = ConvertTemp[,2],
                                   to = ConvertTemp[,3],
                                   warn_missing = F)
    #drop any XXX genotypes
    RowIndex<-grep(x=sapply(1:nrow(dattemp),function(x) paste(dattemp[x,-1],
                                                              collapse="_")),
                   pattern="XXX") #ABL coded genotypes XXX that were not in the baseline

    if(length(RowIndex)!=0){dattemp[RowIndex,]<-c(0,0)}

    AllelesConverted <- dattemp[,]

    AllelesConverted
  }

  datnew <- cbind(dat[,1:(GenoStartCol-1)],do.call(cbind,AllelesConverted))

  colnames(datnew)<-colnames(dat)

  write.csv(file = Outfile,x = datnew, row.names = F)

}
