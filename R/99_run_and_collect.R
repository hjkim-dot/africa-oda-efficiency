# ============================================================
# 99. 전체 실행 + 결과 모으기  (이 파일 하나만 실행하면 됩니다)
# ============================================================
# 하는 일
#  1) run_all.R(00~09번)을 실행하면서, 화면에 나오는 모든 내용을 run_log.txt 로 저장
#     (에러와 경고 메시지도 같이 저장됩니다)
#  2) output 폴더의 결과표(CSV)를 output/all_results.xlsx 한 파일로 묶기
#     (CSV 하나가 엑셀 시트 하나가 됩니다)
# 실행 후 Claude 에게 올릴 파일 2개:  run_log.txt  /  output/all_results.xlsx
# 실행 방법:  source("99_run_and_collect.R", encoding = "UTF-8")
# ============================================================
if (!requireNamespace("writexl", quietly = TRUE)) install.packages("writexl")
suppressPackageStartupMessages({ library(readr); library(writexl) })
dir.create("output", showWarnings = FALSE)

# ---- 1) 실행 + 기록 --------------------------------------------------
log_con <- file("run_log.txt", open = "wt", encoding = "UTF-8")
sink(log_con, split = TRUE)            # 화면 출력도 그대로 보이게 (split = TRUE)
sink(log_con, type = "message")        # 에러·경고 메시지도 기록
cat("실행 시각:", format(Sys.time()), "\n")
cat("R 버전   :", R.version.string, "\n")
pk <- c("censReg", "betareg", "Benchmarking", "plm", "tseries", "sandwich", "lmtest", "dplyr")
cat("패키지   :", paste(pk, sapply(pk, function(p) tryCatch(as.character(packageVersion(p)),
    error = function(e) "없음")), sep = " ", collapse = " | "), "\n")
cat("작업 폴더:", getwd(), "\n\n")
tryCatch(
  source("run_all.R", encoding = "UTF-8"),
  finally = { sink(type = "message"); sink(); close(log_con) }
)

# ---- 2) CSV 결과를 엑셀 한 파일로 묶기 -----------------------------------
files <- list.files("output", pattern = "\\.csv$", full.names = TRUE)
sheets <- lapply(files, function(f) read_csv(f, show_col_types = FALSE))
names(sheets) <- substr(tools::file_path_sans_ext(basename(files)), 1, 31)   # 시트 이름은 31자까지
if (file.exists("dea_results_full.csv"))
  sheets[["dea_results_full"]] <- read_csv("dea_results_full.csv", show_col_types = FALSE)
write_xlsx(sheets, "output/all_results.xlsx")

cat("\n=============================================\n")
cat("끝났습니다. 아래 2개 파일을 올려 주세요.\n")
cat("  1) run_log.txt\n")
cat("  2) output/all_results.xlsx  (시트", length(sheets), "개)\n")
cat("=============================================\n")
