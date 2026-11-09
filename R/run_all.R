# ============================================================
# run_all.R : 00 ~ 09 를 순서대로 전부 실행 (한 번에 돌리고 싶을 때)
# ============================================================
# ---- 필요한 패키지가 없으면 자동 설치 ------------------------------------
need <- c("dplyr", "tidyr", "purrr", "tibble", "readr", "readxl", "tseries", "plm",
          "censReg", "betareg", "Benchmarking", "lmtest", "sandwich", "writexl")
miss <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss) > 0) {
  cat("설치되지 않은 패키지를 설치합니다:", paste(miss, collapse = ", "), "\n")
  install.packages(miss, repos = "https://cloud.r-project.org")
}

# ---- 실행 전 점검: data.xlsx 가 작업 폴더에 있어야 합니다 ---------------------
if (!file.exists("data.xlsx")) {
  stop("\n작업 폴더에 data.xlsx 가 없습니다.\n",
       "  현재 작업 폴더: ", getwd(), "\n",
       "  이 폴더에 있는 파일: ", paste(head(list.files(), 12), collapse = ", "), "\n",
       "  -> data.xlsx 를 위 폴더로 복사하거나, Session > Set Working Directory 로 data.xlsx 가 있는 폴더를 지정하세요.",
       call. = FALSE)
}

scripts <- c("00_data_prep.R", "01_dea.R", "02_descriptive.R", "03_adf.R",
             "04_panel_unitroot_hadri.R", "05_tobit_level.R", "06_vif.R",
             "07_leave_one_out.R", "08_beta_fractional.R", "09_tobit_diff.R", "10_stationary_regression.R", "11_tobit_stationary_samples.R", "12_country_regressions.R", "13_subregion_tobit.R", "14_panel_tobit_level.R")
for (s in scripts) {
  cat("\n\n################ ", s, " ################\n")
  ok <- tryCatch({ source(s, encoding = "UTF-8"); TRUE },
                 error = function(e) { cat("!! 오류로 중단:", conditionMessage(e), "\n"); FALSE })
  if (!ok && s == "00_data_prep.R") {
    cat("\n00번(데이터 준비)이 실패하면 나머지가 모두 실패하므로 여기서 멈춥니다.\n")
    cat("위의 오류 문장을 확인하세요.\n")
    break
  }
}

library(readr); library(writexl)
files <- list.files("output", pattern="\\.csv$", full.names=TRUE)
sheets <- lapply(files, read_csv, show_col_types=FALSE)
names(sheets) <- substr(tools::file_path_sans_ext(basename(files)), 1, 31)
if (file.exists("dea_results_full.csv"))
  sheets[["dea_results_full"]] <- read_csv("dea_results_full.csv", show_col_types=FALSE)
write_xlsx(sheets, "output/all_results.xlsx")

##통제변수 제외하고 Tobit 회귀분석
library(readxl); library(dplyr)
library(censReg); library(sandwich); library(lmtest)

d <- read_excel("data.xlsx", sheet = "data") |>
  filter(year >= 2006, !is.na(dea_score), !is.na(wgi_lag1))

# (1) GDP 포함 (본문 표 7의 (1)과 같아야 함)
m1 <- censReg(dea_score ~ wgi_lag1 + nat_resource + mobile + ln_gdp_pc + factor(year),
              left = 0, right = 1, data = d)
# (2) GDP 제외
m2 <- censReg(dea_score ~ wgi_lag1 + nat_resource + mobile + factor(year),
              left = 0, right = 1, data = d)

coeftest(m1, vcov = vcovCL(m1, cluster = d$iso3c))
coeftest(m2, vcov = vcovCL(m2, cluster = d$iso3c))