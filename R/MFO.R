
#' Maximal Fat Oxidation & Fat Max Function
#'
#' @param step_time how often the data was collected (in seconds).
#' @param db_MFO database containing MFO test.
#' @param db_basal database containing basal test.
#' @param db_graded database containing incremental exercise test.
#' @param cv_var variable to estimate coefficient of variation. Can be: VO2, VCO2 or RER.
#' @param author author to estimate MFO. Can be: Frayn or Jeukendrup.
#' @param VO2max VO2max can be passed directly using this argument instead of use db_graded argument.
#'
#' @import ggplot2
#' @importFrom tibble tibble
#' @importFrom stats lm na.omit sd
#'
#'
#' @return {Returns a list which contains:
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
#' # Read dfs
#' data(list = c("basal_df", "MFO_df", "VO2max_df"), package = "MFO")
#'
#' # Convert to data.frame
#' basal_df <- data.frame(basal_df)
#' MFO_df <- data.frame(MFO_df)
#' VO2max_df <- data.frame(VO2max_df)
#'
#' # Calculate MFO and Fatmax
#' result_MFO <- MFO(step_time = 20,
#'                  db_MFO = MFO_df,
#'                  db_basal = basal_df,
#'                  db_graded = VO2max_df,
#'                  cv_var = "RER",
#'                  author = "Frayn",
#'                  VO2max = NULL)
#'
#' # set user working directory
#' setwd(oldwd)
#' }
#'
MFO <- function(step_time,
                db_MFO,
                db_basal,
                db_graded = NULL,
                cv_var,
                author,
                VO2max = NULL) {

  # Solve error: 'list' object cannot be coerced to type 'double'
  db_basal <- data.frame(db_basal)
  db_MFO <- data.frame(db_MFO)

  # Get VO2max
  if(!is.null(db_graded)) {
    VO2max <- max(db_graded["VO2"])
  } else if(is.null(db_graded)) {
    VO2max <- VO2max
  } else if(is.null(db_graded) & is.null(VO2max)) {
    stop("you must provide a VO2max value using db_graded or VO2max arguments")
  }

  # Get basal data
  basal <- met_basal(step_time = step_time,
                     db = db_basal,
                     cv_var = cv_var)

  # Calculate CHO, FAT and Kcal - Frayn or Jeukendrup
  # calculate_vars function
  MFO_test <- calculate_vars(step_time = step_time,
                             db_MFO = db_MFO,
                             VO2max = VO2max,
                             author = author)

  # Create db for graph
  # Load
  Load <- c(1:dim(MFO_test)[1] * 15)
  Load <- c("basal", Load)

  # VO2
  VO2 <- c()
  for (i in 1:dim(MFO_test)[1]) {
    VO2[i] <- MFO_test[i, "VO2"]
  }
  VO2 <- c(basal$x_VO2, VO2)

  # FAT
  FAT <- c()
  for (i in 1:dim(MFO_test)[1]) {
    FAT[i] <- MFO_test[i, "FAT"]
  }
  FAT <- c(basal$x_FAT, FAT)


  MFO_db <- tibble(Load = Load,
                   VO2 = VO2,
                   FAT = FAT) %>%
    mutate(porc_VO2 = (VO2 * 100) /  VO2max)

  # Remove NA´s
  MFO_db <- na.omit(MFO_db)
  # Convert negative number to 0
  MFO_db[MFO_db$FAT <= 0, 3] <- 0

  # Polynomial regression
  MFO_mod <- lm(MFO_db$FAT ~ MFO_db$porc_VO2 + I(MFO_db$porc_VO2^2))

  # FATmax
  VO2_MFO <- (MFO_mod$coefficients[[2]] / (2 * MFO_mod$coefficients[[3]]))*-1
  VO2_MFO_text <- paste("FAT[MAX] == ~",round(VO2_MFO, 2))
  # MFO
  VAR_MFO <- MFO_mod$coefficients[[3]] * (VO2_MFO^2) + (MFO_mod$coefficients[[2]] * VO2_MFO) + MFO_mod$coefficients[[1]]
  VAR_MFO_text <- paste("MFO == ~",round(VAR_MFO, 2))

  # MFO plot
  MFO_plot <- MFO_db %>%
    ggplot(aes(x = porc_VO2, y = FAT)) +
    geom_point(aes(size = 15)) +
    stat_smooth(method='lm', formula = y~x + I(x^2), se = FALSE) +
    ylab("FAT (gr)") +
    xlab(expression("Exercise " *"intensity " *"("*VO["2max"]*")")) +
    theme_bw() +
    theme(legend.position = "none",
          axis.text.x = element_text(size=11),
          axis.text.y = element_text(size=11),
          axis.title = element_text(size=12)) +
    annotate("label",
             #x = min(MFO_db$porc_VO2) + 5,
             #y = max(MFO_db$FAT),
             x=Inf,
             y = Inf,
             vjust=1.5,
             hjust=1.25,
             label = VO2_MFO_text,
             parse = TRUE) +
    annotate("label",
             #x = min(MFO_db$porc_VO2) + 5,
             #y = max(MFO_db$FAT) - 0.05,
             x=Inf,
             y = Inf,
             vjust=3.5,
             hjust=1.25,
             label = VAR_MFO_text,
             parse = TRUE)

  return(list(MFO_db = MFO_db,
              MFO_plot = MFO_plot,
              MFO = VAR_MFO,
              FAT_MAX = VO2_MFO,
              x_CHO = basal$x_CHO,
              x_FAT = basal$x_FAT,
              x_Kcal = basal$x_Kcal))

}
