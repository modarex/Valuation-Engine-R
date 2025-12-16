suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  library(lubridate)
  library(tibble)
})

num0 <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  ifelse(is.na(x), 0, x)
}

pick_first_col <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) return(NULL)
  hit[[1]]
}

coalesce_cols <- function(df, candidates) {
  col <- pick_first_col(df, candidates)
  if (is.null(col)) return(rep(NA_real_, nrow(df)))
  suppressWarnings(as.numeric(df[[col]]))
}

normalize_period_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (is.numeric(x)) return(as.Date(x, origin = "1970-01-01"))
  out <- suppressWarnings(as.Date(x))
  if (all(is.na(out))) {
    out <- suppressWarnings(as.Date(substr(as.character(x), 1, 10)))
  }
  out
}


fcf_from_is_bs <- function(is_df, bs_df, max_years = 10) {
  if (is.null(is_df) || is.null(bs_df) || nrow(is_df) == 0 || nrow(bs_df) == 0) {
    return(tibble(
      date = as.Date(character()),
      fcf = numeric(),
      net_income = numeric(),
      dep_amort = numeric(),
      capex_proxy = numeric(),
      d_wc = numeric(),
      flags = character()
    ))
  }

  is_date_col <- pick_first_col(is_df, c("date", "calendarYear", "fillingDate", "acceptedDate"))
  bs_date_col <- pick_first_col(bs_df, c("date", "calendarYear", "fillingDate", "acceptedDate"))

  if (is.null(is_date_col) || is.null(bs_date_col)) {
    return(tibble(
      date = as.Date(character()),
      fcf = numeric(),
      net_income = numeric(),
      dep_amort = numeric(),
      capex_proxy = numeric(),
      d_wc = numeric(),
      flags = "missing_date_column"
    ))
  }

  is2 <- is_df %>%
    mutate(.date_raw = .data[[is_date_col]],
           date = normalize_period_date(.date_raw)) %>%
    filter(!is.na(date)) %>%
    arrange(desc(date)) %>%
    distinct(date, .keep_all = TRUE) %>%
    slice_head(n = max_years)

  bs2 <- bs_df %>%
    mutate(.date_raw = .data[[bs_date_col]],
           date = normalize_period_date(.date_raw)) %>%
    filter(!is.na(date)) %>%
    arrange(desc(date)) %>%
    distinct(date, .keep_all = TRUE) %>%
    slice_head(n = max_years)


  is_net_income <- coalesce_cols(is2, c("netIncome", "NetIncomeLoss", "NetIncomeLossAvailableToCommonStockholdersBasic"))
  is_da <- coalesce_cols(is2, c("depreciationAndAmortization", "DepreciationDepletionAndAmortization"))

  bs_ca <- coalesce_cols(bs2, c("totalCurrentAssets", "TotalCurrentAssets"))
  bs_cl <- coalesce_cols(bs2, c("totalCurrentLiabilities", "TotalCurrentLiabilities"))

  bs_ppe <- coalesce_cols(bs2, c("propertyPlantEquipmentNet", "PropertyPlantAndEquipmentNet", "propertyPlantEquipmentNetOfAccumulatedDepreciation"))

  bs_total_assets <- coalesce_cols(bs2, c("totalAssets", "TotalAssets"))

  df <- is2 %>%
    select(date) %>%
    inner_join(
      tibble(
        date = is2$date,
        net_income = suppressWarnings(as.numeric(is_net_income)),
        dep_amort = suppressWarnings(as.numeric(is_da))
      ),
      by = "date"
    ) %>%
    inner_join(
      tibble(
        date = bs2$date,
        current_assets = suppressWarnings(as.numeric(bs_ca)),
        current_liabilities = suppressWarnings(as.numeric(bs_cl)),
        ppe_net = suppressWarnings(as.numeric(bs_ppe)),
        total_assets = suppressWarnings(as.numeric(bs_total_assets))
      ),
      by = "date"
    ) %>%
    arrange(desc(date))

  df <- df %>%
    mutate(
      flag_no_da = is.na(dep_amort),
      dep_amort = num0(dep_amort),

      flag_no_ppe = is.na(ppe_net),
      ppe_net = suppressWarnings(as.numeric(ppe_net)),

      flag_no_wc = is.na(current_assets) | is.na(current_liabilities),
      current_assets = num0(current_assets),
      current_liabilities = num0(current_liabilities),

      working_capital = current_assets - current_liabilities
    )

  
  df <- df %>%
    mutate(
      wc_prev = lag(working_capital),
      ppe_prev = lag(ppe_net),
      total_assets_prev = lag(total_assets),

      d_wc = ifelse(is.na(wc_prev), NA_real_, working_capital - wc_prev),

      
      capex_proxy =
        case_when(
          !flag_no_ppe & !is.na(ppe_prev) ~ (ppe_net - ppe_prev) + dep_amort,
          TRUE ~ NA_real_
        ),

      capex_proxy =
        ifelse(
          is.na(capex_proxy) & !is.na(total_assets) & !is.na(total_assets_prev),
          (total_assets - total_assets_prev) + dep_amort,
          capex_proxy
        ),

      flag_capex_from_total_assets = is.na(ppe_prev) | flag_no_ppe
    )

  df <- df %>%
    mutate(
      net_income = num0(net_income),
      d_wc = ifelse(is.na(d_wc), 0, d_wc),  
      capex_proxy = ifelse(is.na(capex_proxy), 0, capex_proxy),

      fcf = net_income + dep_amort - capex_proxy - d_wc,

      flags = paste(
        c(
          ifelse(flag_no_da, "DA_missing_used_0", NA_character_),
          ifelse(flag_no_wc, "WC_missing_used_0", NA_character_),
          ifelse(flag_capex_from_total_assets, "CAPEX_proxy_total_assets", NA_character_)
        ) %>% na.omit(),
        collapse = ";"
      ),
      flags = ifelse(flags == "", "OK", flags)
    ) %>%
    select(date, fcf, net_income, dep_amort, capex_proxy, d_wc, flags)

  df
}
