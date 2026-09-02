#' Northern Boundary Sockeye data sheet formatting
#'
#' Convert excel file produced by ADFG into a usable formatted intake sheet.
#' @param data ADFG data to process.
#' @param sample_col name of the column in data that is used for the sample 
#' @param plate_col name of the column in data that is used for the plate
#' @param well_col of the column in data that is used for the well

#' @examples
#' export_plate_maps(data=ADFG_inventory,sample_col = 'SampleID4Plating', plate_col = 'DNA_PlateID')
#' @export

export_plate_maps <- function(data, sample_col, plate_col, well_col, 
                              PlateMap_output_file = PlateMap_output_file, ntc_well = ntc_well,
                              species = species, project = project, plated_by = plated_by, 
                              extracted_by = extracted_by, extracted_on = extracted_on) {
  
  wb <- openxlsx::createWorkbook()
  
  # Create styles 
  bold_style <- createStyle(textDecoration = "bold")
  # New grid style with borders included
  grid_style <- createStyle(
    halign = "center", 
    valign = "center", 
    wrapText = TRUE,
    border = "TopBottomLeftRight",
    borderStyle = "thin",
    borderColour = "black"
  )
  
  plates <- unique(data[[plate_col]])
  
  for (plate in plates) {
    # Filter for current plate
    plate_data <- data[data[[plate_col]] == plate, ]
    
    # --- GET FIRST AND LAST SAMPLES ---
    # omit NAs just in case, and grab the first and last sample names
    samples_only <- na.omit(plate_data[[sample_col]])
    first_sample <- first(samples_only)
    last_sample <- last(samples_only)
    
    # --- CREATE THE HEADER BLOCK ---
    header_df <- data.frame(
      Field = c("Species:", "Project:", "Tissue plated by:", "DNA extracted by:", 
                "Plate:", "Samples:", "DNA extracted on:"),
      Value = c(species, project, plated_by, extracted_by, 
                as.character(plate), paste(first_sample, "through", last_sample), extracted_on),
      stringsAsFactors = FALSE
    )
    
    # --- PROCESS THE GRID ---
    wells <- toupper(trimws(plate_data[[well_col]]))
    
    clean_data <- data.frame(
      Row = stringr::str_extract(wells, "[A-H]"),
      Col = as.numeric(stringr::str_extract(wells, "\\d+")),
      Sample = plate_data[[sample_col]],
      stringsAsFactors = FALSE
    )
    
    full_grid <- expand.grid(
      Row = factor(LETTERS[1:8], levels = LETTERS[1:8]),
      Col = as.numeric(1:12)
    )
    
    merged_data <- left_join(full_grid, clean_data, by = c("Row", "Col"))
    
    merged_data <- merged_data %>%
      mutate(
        Well_ID = paste0(Row, Col),
        Sample = case_when(
          Well_ID == ntc_well & is.na(Sample) ~ paste("NTC",stringr::str_sub(plate,-2,-1),sep="_"),
          is.na(Sample) ~ "", 
          TRUE ~ Sample
        )
      ) %>%
      select(-Well_ID)
    
    grid_wide <- merged_data %>%
      pivot_wider(names_from = Col, values_from = Sample) %>%
      arrange(Row)
    
    # Rename the first column to be blank so "Row" doesn't print in 9A
    colnames(grid_wide)[1] <- ""
    
    # --- WRITE TO EXCEL ---
    sheet_name <- substr(as.character(plate), 1, 31)
    addWorksheet(wb, sheet_name)
    
    # Write the header block
    writeData(wb, sheet_name, header_df, startRow = 1, startCol = 1, colNames = FALSE)
    addStyle(wb, sheet_name, style = bold_style, rows = 1:7, cols = 1, gridExpand = TRUE)
    
    # Write the Grid
    writeData(wb, sheet_name, grid_wide, startRow = 9, startCol = 1, colNames = TRUE)
    
    # --- FORMATTING ADJUSTMENTS ---
    # Apply centering, wrapping, and borders to the grid (Rows 9-17, Columns 1-13)
    addStyle(wb, sheet_name, style = grid_style, rows = 9:17, cols = 1:13, gridExpand = TRUE)
    
    # Set Column A width to 15 (for the header labels and row letters)
    setColWidths(wb, sheet_name, cols = 1, widths = 15)
    
    # Set Columns B-M (Cols 2-13) width to 10.17
    setColWidths(wb, sheet_name, cols = 2:13, widths = 10.17)
    
    # Set row heights for rows 10-17 to 40
    setRowHeights(wb, sheet_name, rows = 10:17, heights = 40)
  }
  
  saveWorkbook(wb, PlateMap_output_file, overwrite = TRUE)
  message("Successfully exported plate maps to: ", PlateMap_output_file)
}