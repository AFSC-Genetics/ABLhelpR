#' Northern Boundary Sockeye data inventory
#'
#' Process multiple excel files from ADFG into a single metadata sheet and platemaps.
#' @param Path2SampSheets The directory where the ADFG ASL sample sheets live
#' @param CombinedInventory_OutFile The output file name for the combined ADFG inventory with additional ABL sample information
#' @param PlateMap_output_file name of the output file for the platemaps
#' @param ntc_well Well used for the NTC
#' @param species Species being plated
#' @param project Name of the project
#' @param plated_by Name of person plating the DNA, may be left blank 
#' @param extracted_by Name of person plating the DNA, may be left blank
#' @param extracted_on Name of person plating the DNA, may be left blank
#' @examples
#' Inventory_ADFG(Path2SampSheets = "./", CombinedInventory_OutFile = "./NB_One26.csv",PlateMap_output_file = "./NB_One26platemaps.xlsx" )
#' @export

Inventory_ADFG <- function(Path2SampSheets = "./",CombinedInventory_OutFile = "./NB_One26.csv", PlateMap_output_file, ntc_well = "H12",
                           species = "", project = "", plated_by = "", 
                           extracted_by = "", extracted_on = "") {

Files2Process <- list.files(Path2SampSheets,full.names=T) %>%
  grep(pattern="101 Gillnet - 104 Seine Detailed ASL Samples",value = T)

PlateWells <- c(outer(LETTERS[1:8], sprintf("%02d", 1:12), paste0)) %>%
  {.[.!="H12"]}

ADFG_inventory<-lapply(1:length(Files2Process), function(x)
  readxl::read_xlsx(Files2Process[x])) %>%
  bind_rows(.)%>%
  mutate(PosOnWhatman = `Dna Specimen No` %>% str_sub(.,-2,-1) %>% as.numeric() ,
         ADFG_ScaleCardNum = `Scale Card No`,
         WhatCardNum = `Dna Specimen No` %>% {gsub('.{2}$', '', .)} %>% {gsub('^0+|^10+', '', .)},
         Well = rep(PlateWells, length.out = n()),
         DNA_PlateID = paste0("One26_", sprintf("%02d", ceiling(row_number() / 95))),
         ABL_SampleID = paste('One26',WhatCardNum,ADFG_ScaleCardNum,PosOnWhatman,sep="_"),
         SampleID4Plating = paste(WhatCardNum,ADFG_ScaleCardNum,PosOnWhatman,sep="_"))%>%
  relocate(SampleID4Plating, .before=Year) %>%
  relocate(PosOnWhatman,.before = SampleID4Plating) %>%
  relocate(ADFG_ScaleCardNum,.before = PosOnWhatman) %>%
  relocate(WhatCardNum,.before = ADFG_ScaleCardNum) %>%
  relocate(Well,.before = WhatCardNum) %>%
  relocate(DNA_PlateID,.before = Well) %>%
  relocate(ABL_SampleID,.before = DNA_PlateID) 

export_plate_maps(data = ADFG_inventory,
                  sample_col = 'SampleID4Plating', 
                  plate_col = 'DNA_PlateID', 
                  well_col = 'Well',
                  PlateMap_output_file = PlateMap_output_file, 
                  ntc_well = ntc_well,
                  species = species, 
                  project = project, 
                  plated_by = plated_by, 
                  extracted_by = extracted_by, 
                  extracted_on = extracted_on)

write_delim(x=ADFG_inventory,
            file = CombinedInventory_OutFile,
            delim=",")

}

