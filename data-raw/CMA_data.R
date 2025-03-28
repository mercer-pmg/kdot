## code to prepare cma_return_risk and cma_cor_mat datasets

# cma_dir <- "C:/Users/DavidAllen/OneDrive - Mercer Advisors/Documents/Portfolio Monitoring/"
# cma_name <- "2024 CMAs - Mercer - 2024-05.xlsx"
# cma_dirty_name <- "2024 CMAs with AQR Tax Aware Delphi Plus.xlsx"

cma_dir <- "C:/Users/DavidAllen/OneDrive - Mercer Advisors/Documents/Data/CMAs/"
cma_name <- "2025 LTCMAs.xlsx"


cma_return_risk <- readxl::read_xlsx(
  path  = paste0(cma_dir, cma_name),
  skip  = 3,
  range = "A4:F87")

# cma_return_risk <- readxl::read_xlsx(
#   path = paste0(cma_dir,cma_dirty_name),
#   sheet = "2024 JPM CMAs w TA Delphi Plus",
#   skip =3,
#   range = "B4:F88"
# )

cma_cor_mat <- readxl::read_xlsx(
  path  = paste0(cma_dir, cma_name),
  col_names = TRUE,
  range = "G4:CK87")

# cma_cor_mat <- readxl::read_xlsx(
#   path  = paste0(cma_dir, cma_dirty_name),
#   sheet = "2024 JPM CMAs w TA Delphi Plus",
#   col_names = TRUE,
#   range = "G4:CL88")

# source("data-raw/sleeve_holdings.R")
# source("data-raw/model_portfolio_holdings.R")
source("data-raw/strategy_holdings.R")

usethis::use_data(
  cma_return_risk,
  cma_cor_mat,
  strategy_holdings,
#   sleeve_holdings,
#   model_portfolio_holdings,
#
  overwrite = TRUE,
  internal  = TRUE)
