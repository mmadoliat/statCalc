load_data <- function(folder_name) {
  files <- list.files(folder_name, full.names = TRUE, pattern = "\\.csv$")
  data_list <- lapply(files, read.csv)
  names(data_list) <- basename(files)
  return(data_list)
}


data_1700 <- load_data("1700")
data_4720 <- load_data("4720")

save(data_1700, file = "../statspackage/data/1700.rda")
save(data_4720, file = "../statspackage/data/4720.rda")
