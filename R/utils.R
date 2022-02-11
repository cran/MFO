
#' Basal metabolic rate
#'
#' @param step_time how often the data was collected (in seconds).
#' @param db a database
#' @param cv_var variable to calculate coefficient of variation
#'
#' @importFrom dplyr pull mutate
#' @importFrom tibble add_column
#' @importFrom magrittr %>%
#'
met_basal <- function(step_time, db, cv_var) {

  n_row <- calculate_steps(step_time = step_time,
                           db = NULL,
                           db_type = "basal")

  # First rows
  cv_VO2_1 <- rep(NA, n_row)
  cv_VCO2_1 <- rep(NA, n_row)
  cv_RER_1 <- rep(NA, n_row)

  n_row <- n_row + 1

  # Empty vectors
  cv_VO2_2 <- rep(NA, length(pull(db["VO2"])) - n_row)
  cv_VCO2_2 <- rep(NA, length(pull(db["VCO2"])) - n_row)
  cv_RER_2 <- rep(NA, length(pull(db["RER"])) - n_row)

  # VO2
  ############ VO2
  j <- 1
  for (i in n_row:length(pull(db["VO2"]))) {
    cv_VO2_2[j] <- sd(unlist((db[j:i, "VO2"]))) / mean(unlist((db[j:i, "VO2"])))
    j <- j + 1
  }

  ############ VCO2
  j <- 1
  for (i in n_row:length(pull(db["VCO2"]))) {
    cv_VCO2_2[j] <- sd(unlist((db[j:i, "VCO2"]))) / mean(unlist((db[j:i, "VCO2"])))
    j <- j + 1
  }

  ############ RER
  j <- 1
  for (i in n_row:length(pull(db["RER"]))) {
    cv_RER_2[j] <- sd(unlist((db[j:i, "RER"]))) / mean(unlist((db[j:i, "RER"])))
    j <- j + 1
  }

  # Merge variables
  cv_VO2 <- round(c(cv_VO2_1, cv_VO2_2*100),2)
  cv_VCO2 <- round(c(cv_VCO2_1, cv_VCO2_2*100),2)
  cv_RER <- round(c(cv_RER_1, cv_RER_2*100),2)

  # Add columns to db
  db <- db %>%
    add_column(cv_VO2, cv_VCO2, cv_RER)

  # get 5 min db
  cv_var <- paste("cv_", cv_var, sep = "")
  db_5min <- get_5min(db, cv_var = cv_var, n_row = n_row)

  db_5min <- db_5min %>%
    mutate(CHO_Frayn = (4.55 * VCO2 - 3.21 * VO2) / 1000,
           FAT_Frayn = (1.67 * VO2 - 1.67 * VCO2) / 1000,
           Kcal_total_Frayn = CHO_Frayn * 4 + FAT_Frayn * 9)

  ## Return
  # mean VO2
  x_VO2 <- mean(pull(db_5min["VO2"]))
  # mean HR
  x_HR <- mean(pull(db_5min["HR"]))
  # mean RER
  x_RER <- mean(pull(db_5min["RER"]))
  # mean CHO
  x_CHO <- mean(pull(db_5min["CHO_Frayn"]))
  # mean FAT
  x_FAT <- mean(pull(db_5min["FAT_Frayn"]))
  # mean Kcal
  x_Kcal <- mean(pull(db_5min["Kcal_total_Frayn"]))

  return(list(db = db,
              db_5min = db_5min,
              x_VO2 = x_VO2,
              #x_HR = x_HR,
              x_RER = x_RER,
              x_CHO = x_CHO,
              x_FAT = x_FAT,
              x_Kcal = x_Kcal))
}


#
#' Calculation of CHO, FAT and Kcal
#'
#' @param step_time how often the data was collected (in seconds).
#' @param db_MFO dtabase with MFO test
#' @param VO2max maximum oxygen uptake
#' @param author eithe "Frayn" or "Jeukendrup"
#'
#' @importFrom dplyr pull mutate
#' @importFrom magrittr %>%
#'
calculate_vars <- function(step_time, db_MFO, VO2max, author) {

  steps <- calculate_steps(step_time = step_time,
                           db = db_MFO,
                           db_type = "MFO")

  db_vars <- data.frame(matrix(ncol = 5, nrow = steps$n_steps))
  colnames(db_vars) <- c("VO2", "HR", "CHO", "FAT", "Kcal")

  if(author == "Frayn") {
    # CHO_Frayn (4.55 * VCO2 - 3.21 * VO2) / 1000
    # FAT_Frayn (1.67 * VO2 - 1.67 * VCO2) / 1000
    # Kcal_total_Frayn - CHO_Frayn * 4 + FAT_Frayn * 9
    db_MFO <- db_MFO %>%
      mutate(CHO_Frayn = (4.55 * VCO2 - 3.21 * VO2) / 1000,
             FAT_Frayn = (1.67 * VO2 - 1.67 * VCO2) / 1000,
             Kcal_total_Frayn = CHO_Frayn * 4 + FAT_Frayn * 9)


    for (i in seq(1:steps$n_steps)) {

      # mean VO2
      db_vars[i, "VO2"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "VO2"]))
      # mean HR
      db_vars[i, "HR"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "HR"]))
      # mean CHO
      db_vars[i, "CHO"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "CHO_Frayn"]))
      # mean FAT
      db_vars[i, "FAT"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "FAT_Frayn"]))
      # mean Kcal
      db_vars[i, "Kcal"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "Kcal_total_Frayn"]))

      # next bounds
      if(step_time == 20){
        steps$upper_bound <- (9 * (i+1))
        steps$lower_bound <- steps$upper_bound - 3
      }
    }

  }  else if (author == "Jeukendrup") {

    # VO2 < 50% VO2max
    # CHO_Jeukendrup (4.344 * VCO2 - 3.061 * VO2) / 1000
    # FAT_Jeukendrup (1.695 * VO2 - 1.701 * VCO2) / 1000
    # Kcal_total_Jeukendrup - CHO_Jeukendrup * 4 + FAT_Jeukendrup * 9
    # VO2 > 50% VO2max
    # CHO_Jeukendrup (4.210 * VCO2 - 2.962 * VO2) / 1000
    # FAT_Jeukendrup (1.695 * VO2 - 1.701 * VCO2) / 1000
    # Kcal_total_Jeukendrup - CHO_Jeukendrup * 4 + FAT_Jeukendrup * 9

    db_MFO <- db_MFO %>%
      mutate(CHO_Jeukendrup_40_50_VO2 = (4.344 * VCO2 - 3.061 * VO2) / 1000,
             FAT_Jeukendrup_40_50_VO2 = (1.695 * VO2 - 1.701 * VCO2) / 1000,
             Kcal_total_Jeukendrup_40_50_VO2 = CHO_Jeukendrup_40_50_VO2 * 4 + FAT_Jeukendrup_40_50_VO2 * 9,
             CHO_Jeukendrup_50_75_VO2 = (4.210 * VCO2 - 2.962 * VO2) / 1000,
             FAT_Jeukendrup_50_75_VO2 = (1.695 * VO2 - 1.701 * VCO2) / 1000,
             Kcal_total_Jeukendrup_50_75_VO2 = CHO_Jeukendrup_50_75_VO2 * 4 + FAT_Jeukendrup_50_75_VO2 * 9)

    for (i in seq(1:steps$n_steps)) {

      # mean VO2
      db_vars[i, "VO2"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "VO2"]))
      # mean HR
      db_vars[i, "HR"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "HR"]))

      if((db_vars[i, "VO2"] / VO2max) < 50) {
        # mean CHO
        db_vars[i, "CHO"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "CHO_Jeukendrup_40_50_VO2"]))
        # mean FAT
        db_vars[i, "FAT"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "FAT_Jeukendrup_40_50_VO2"]))
        # mean Kcal
        db_vars[i, "Kcal"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "Kcal_total_Jeukendrup_40_50_VO2"]))
      } else {
        # mean CHO
        db_vars[i, "CHO"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "CHO_Jeukendrup_50_75_VO2"]))
        # mean FAT
        db_vars[i, "FAT"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "FAT_Jeukendrup_50_75_VO2"]))
        # mean Kcal
        db_vars[i, "Kcal"] <- mean((db_MFO[steps$lower_bound:steps$upper_bound, "Kcal_total_Jeukendrup_50_75_VO2"]))
      }

      # next bounds
      if(step_time == 20){
        steps$upper_bound <- (9 * (i+1))
        steps$lower_bound <- steps$upper_bound - 3
      }
    }

  }

  return(db_vars)

}

#' Calculate steps
#'
#' @param step_time how often the data was collected (in seconds).
#' @param db a database
#' @param db_type either "basal" or "MFO"
#'
calculate_steps <- function(step_time, db, db_type) {

  if(db_type == "basal") {
    if(step_time == 20){
      n_steps <- 14
    }
    return(n_steps)
  }

  if(db_type == "MFO") {
    if(step_time == 20){
      n_steps <- nrow(db)/9
      upper_bound <- 9
      lower_bound <- upper_bound - 3
    }
    return(list(n_steps = n_steps,
                upper_bound = upper_bound,
                lower_bound = lower_bound))
  }
}

#' Get a 5 minutes database
#'
#' @param db a database
#' @param cv_var variable to calculate coefficient of variation
#' @param n_row number of rows
#'
#' @importFrom dplyr pull
#'
get_5min <- function(db, cv_var, n_row) {

  pos_final <- which.min((db[,cv_var]))
  pos_ini <- which.min((db[,cv_var])) - n_row

  db_5min <- db[pos_ini:pos_final,]

  return(db_5min)

}

#' Read databases for MFO package
#'
#' @param from select either from folder or files
#' @param path path to the  the databases
#' @param db_basal_name name of the database with the basal metabolic rate test
#' @param db_MFO_name name of the database of MFO test
#' @param db_graded_name name of the database of the graded exercise test
#' @param remove_rows An integer (or a vector of integers) representing the position of the rows to delete
#' @param col_name_VO2 name given to the variable VO2 in the databases. Must be the same for all databases. Default set to "VO2"
#' @param col_name_VCO2 name given to the variable VCO2 in the databases. Must be the same for all databases. Default set to "VCO2"
#' @param col_name_RER name given to the variable RER in the databases. Must be the same for all databases. Default set to "RER"
#' @param col_name_HR name given to the variable HR in the databases. Must be the same for all databases. Default set to "HR"
#'
#'
#' @importFrom dplyr rename select
#' @importFrom magrittr %>%
#' @importFrom readxl read_xlsx
#' @importFrom openxlsx write.xlsx
#'
#' @return {Returns 3 databases:
#' \itemize{
#' \item participant_db_basal: database with basal metabolism.
#' \item participant_db_MFO: database with MFO test.
#' \item participant_db_graded: graded exercise test.
#' }}
#'
#'
read_MFO_databases <- function(from = c("folder", "files"),
                               path,
                               db_basal_name,
                               db_MFO_name,
                               db_graded_name,
                               col_name_VO2,
                               col_name_VCO2,
                               col_name_RER,
                               col_name_HR,
                               remove_rows = NULL) {


  if(from == "folder"){

    participant_db_graded <- read_xlsx(paste(path, "/", db_graded_name,".xlsx", sep = ""))
    participant_db_basal <- read_xlsx(paste(path, "/", db_basal_name,".xlsx", sep = ""))
    participant_db_MFO <- read_xlsx(paste(path, "/", db_MFO_name,".xlsx", sep = ""))

  } else if(from == "files"){

    participant_db_graded <- read_xlsx(path = path,
                                       sheet = db_graded_name)
    participant_db_basal <- read_xlsx(path = path,
                                      sheet = db_basal_name)
    participant_db_MFO <- read_xlsx(path = path,
                                    sheet = db_MFO_name)

  }

  # Variables to select from databases
  vars_to_select <- c(col_name_VO2, col_name_VCO2, col_name_RER, col_name_HR)

  # Change columns names to VO2 and VCO2
  if(col_name_VO2 != "VO2" | col_name_VCO2 != "VCO2" | col_name_RER != "RER") {

    # First - select variables and rename for next functions
    # Second - remove rows
    # Third - Convert columns form char to num


    # db_graded
    participant_db_graded <- participant_db_graded %>%
      select(all_of(vars_to_select)) %>%
      rename(VO2 = all_of(col_name_VO2),
             VCO2 = all_of(col_name_VCO2),
             RER = all_of(col_name_RER))


    if(!is.null(remove_rows)) {
      participant_db_graded <- participant_db_graded[-c(remove_rows),]
    }
    participant_db_graded <- apply(participant_db_graded, 2, as.numeric)

    participant_db_graded <- as.data.frame(na.omit(participant_db_graded))

    # db_basal
    participant_db_basal <- participant_db_basal %>%
      select(all_of(vars_to_select)) %>%
      rename(VO2 = all_of(col_name_VO2),
             VCO2 = all_of(col_name_VCO2),
             RER = all_of(col_name_RER),
             HR = all_of(col_name_HR))

    if(!is.null(remove_rows)) {
      participant_db_basal <- participant_db_basal[-c(remove_rows),]
    }

    participant_db_basal <- apply(participant_db_basal, 2, as.numeric)

    participant_db_basal <- as.data.frame(na.omit(participant_db_basal))

    # db MFO
    participant_db_MFO <- participant_db_MFO %>%
      select(all_of(vars_to_select)) %>%
      rename(VO2 = all_of(col_name_VO2),
             VCO2 = all_of(col_name_VCO2),
             RER = all_of(col_name_RER))

    if(!is.null(remove_rows)) {
      participant_db_MFO <- participant_db_MFO[-c(remove_rows),]
    }

    participant_db_MFO <- apply(participant_db_MFO, 2, as.numeric)

    participant_db_MFO <- as.data.frame(na.omit(participant_db_MFO))

  }

  return(list(participant_db_graded = participant_db_graded,
              participant_db_basal = participant_db_basal,
              participant_db_MFO = participant_db_MFO))
}

