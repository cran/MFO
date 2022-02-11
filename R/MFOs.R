#' Maximal Fat Oxidation calculation of multiple databases
#'
#' @param from select or folder (basal, MFO and graded databases of the same participant are store in
#'              different files but in the same folder) or files (basal, MFO and graded databases of the
#'              same participant are store in one file but in different sheets)
#' @param path path to the folder with the databases
#' @param db_basal_name name given to the basal database, eg: basal_df
#' @param db_MFO_name name given to the MFO database, eg: MFO_df
#' @param db_graded_name name given to the graded database, eg: VO2max_df
#' @param step_time how often the data was collected (in seconds).
#' @param cv_var variable to estimate coefficient of variation. Can be: VO2, VCO2 or RER.
#' @param author author to estimate MFO. Can be: Frayn or Jeukendrup.
#' @param VO2max VO2max can be passed directly using this argument instead of use db_graded argument. Default set to NULL.
#' @param remove_rows An integer (or a vector of integers) representing the position of the rows to delete
#' @param col_name_VO2 name given to the variable VO2 in the databases. Must be the same for all databases. Default set to "VO2"
#' @param col_name_VCO2 name given to the variable VCO2 in the databases. Must be the same for all databases. Default set to "VCO2"
#' @param col_name_RER name given to the variable RER in the databases. Must be the same for all databases. Default set to "RER"
#' @param col_name_HR name given to the variable HR in the databases. Must be the same for all databases. Default set to "HR"
#' @param save_plot to save the plot or not. Default set to True.
#' @param save_result to save the results in a .xlsx file or not. Default set to True.
#'
#'
#' @importFrom readxl read_xlsx
#' @importFrom tibble tibble
#' @importFrom stringr str_subset
#' @importFrom openxlsx write.xlsx
#'
#' @return {This function creates an .xlsx file in the working directory with the
#' following variables:
#' \itemize{
#' \item MFO_db: database used to create the MFO plot.
#' \item MFO_plot: ggplot object with the MFO plot.
#' \item MFO: Maximal fat oxidation.
#' \item FAT_MAX: Intensity that elicits MFO.
#' \item x_CHO: carbohydrates in basal metabolism.
#' \item x_FAT: fat in basal metabolism.
#' \item x_Kcal: Kcal in basal metabolism.
#' }}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Get old working directory
#' oldwd <- getwd()
#'
#' # Set temporary directory
#' setwd(tempdir())
#'
#' # Create path to store databases
#' dir.create(paste(getwd(),"/MFO_databases", sep = ""))
#' # Get path to databases
#' path <- paste(getwd(),"/MFO_databases", sep = "")
#'
#' # MFOs function
#' # "path" is the path to the databases
#' MFOs <- function(from = "folder",
#'                  path = path,
#'                  db_basal_name = "basal_df",
#'                  db_MFO_name = "MFO_df",
#'                  db_graded_name = "graded_df",
#'                  step_time = 20,
#'                  cv_var = "RER",
#'                  author = "Frayn",
#'                  VO2max = NULL,
#'                  remove_rows = NULL,
#'                  col_name_VO2 = "VO2",
#'                  col_name_VCO2 = "VCO2",
#'                  col_name_RER = "RER",
#'                  col_name_HR = "HR",
#'                  save_plot = TRUE,
#'                  save_result = TRUE)
#'
#' # set user working directory
#' setwd(oldwd)
#' }
#'
MFOs <- function(from = c("folder", "files"),
                 path,
                 db_basal_name,
                 db_MFO_name,
                 db_graded_name,
                 step_time,
                 cv_var,
                 author,
                 VO2max = NULL,
                 remove_rows = NULL,
                 col_name_VO2 = "VO2",
                 col_name_VCO2 = "VCO2",
                 col_name_RER = "RER",
                 col_name_HR = "HR",
                 save_plot = TRUE,
                 save_result = TRUE){

  participants <- list.files(path)

  #print(participants)

  for(i in 1:length(participants)) {

    # Get the participant ID
    # Need to be fixed
    ID_participant <- participants[i]

    print(paste("Working in ", participants[i], sep = ""))

    participant_dbs <- read_MFO_databases(from = from,
                                          path = paste(path,"/",participants[i], sep = ""),
                                          db_basal_name = db_basal_name,
                                          db_MFO_name = db_MFO_name,
                                          db_graded_name = db_graded_name,
                                          remove_rows = remove_rows,
                                          col_name_VO2 = col_name_VO2,
                                          col_name_VCO2 = col_name_VCO2,
                                          col_name_RER = col_name_RER,
                                          col_name_HR = col_name_HR)

    participant_result_MFO <- MFO(step_time = step_time,
                                  db_MFO = participant_dbs$participant_db_MFO,
                                  db_basal = participant_dbs$participant_db_basal,
                                  db_graded = participant_dbs$participant_db_graded,
                                  cv_var = cv_var,
                                  author = author,
                                  VO2max = VO2max)

    # MFO plot
    if(save_plot == TRUE){

      # Create a folder to store plots
      if(i == 1){
      dir.create(paste(getwd(),"/MFO_plots", sep = ""))
      }

      ggsave(paste(ID_participant,".png", sep = ""),
             plot = participant_result_MFO$MFO_plot,
             path = paste(getwd(),"/MFO_plots", sep = ""),
             height=5,
             width=7,
             units='in',
             dpi=600)
    }

    participant_result_MFO_kinetics <- MFO_kinetics(participant_result_MFO$MFO_db)

    # Kinetics plot
    if(save_plot == TRUE){
      ggsave(paste(ID_participant,"kinetics_.png", sep = ""),
             plot = participant_result_MFO_kinetics$MFO_kinetics_plot,
             path = paste(getwd(),"/MFO_plots", sep = ""),
             height=5,
             width=7,
             units='in',
             dpi=600)
    }

    # Data Frame
    if(i == 1){
      dF_result <- tibble(ID = ID_participant,
                          MFO_g_min = round(participant_result_MFO$MFO,2),
                          FAT_MAX_perVO2max = round(participant_result_MFO$FAT_MAX,2),
                          d = round(participant_result_MFO_kinetics$d, 2),
                          t = round(participant_result_MFO_kinetics$t, 2),
                          s = round(participant_result_MFO_kinetics$s, 2),
                          CHO_g_basal = round(participant_result_MFO$x_CHO, 2),
                          FAT_g_basal = round(participant_result_MFO$x_FAT, 2),
                          Kcal_g_basal = round(participant_result_MFO$x_Kcal, 2))
    } else {
      vector_result <- c(ID = ID_participant,
                         MFO_g_min = round(participant_result_MFO$MFO,2),
                         FAT_MAX_perVO2max = round(participant_result_MFO$FAT_MAX,2),
                         d = round(participant_result_MFO_kinetics$d, 2),
                         t = round(participant_result_MFO_kinetics$t, 2),
                         s = round(participant_result_MFO_kinetics$s, 2),
                         CHO_g_basal = round(participant_result_MFO$x_CHO, 2),
                         FAT_g_basal = round(participant_result_MFO$x_FAT, 2),
                         Kcal_g_basal = round(participant_result_MFO$x_Kcal, 2))

      dF_result <- rbind(dF_result, vector_result)
    }


  } # end loop

  if(save_result == TRUE){
  write.xlsx(dF_result, "MFO_result.xlsx")
  }

  return(dF_result)

}
