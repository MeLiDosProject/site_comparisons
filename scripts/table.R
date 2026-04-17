summary_table <- function(data, 
                          site, 
                          table, 
                          non_columns = NULL, 
                          other_arguments = list()
                          ) {
  
  site_gt <- melidos_sites$site_name[melidos_sites$site == site]
  
  if(data |> filter(site == .env$site) |> nrow() == 0) {
    cat("No data available for site: ", site, ". Skipping table generation", 
        sep = "")
    return()
  }
  # browser()
  inject(
  data |> 
    select(-any_of(c("Id", "comments", "comments_english", non_columns))) |> 
    site_conv_mutate() |> 
    mutate(site = 
             fct_other(site, 
                       keep = site_gt, 
                       other_level = "Other sites")
    ) |> 
    tbl_summary(by = site,
                missing_text = "NA",
                !!!other_arguments
                ) |> 
    add_difference() |>
    modify_table_body(\(x) dplyr::select(x, -any_of("p.value"))) |> 
    add_p(test.args = all_categorical() ~ list(simulate.p.value = TRUE)) |>
    bold_p() |> 
    bold_labels() |> 
    modify_abbreviation("NA = Not available / missing")|> 
    gtsummary::add_significance_stars() |> 
    gtsummary::modify_header(label = paste0("**", table, "**")) |> 
    as_gt() |> 
    tab_style(
      style = list(cell_text(color = melidosData::melidos_colors[site])
      ),
      locations = cells_column_labels(stat_1)
    ) |> 
    tab_style(
      style = list(
        cell_fill(color = melidosData::melidos_colors[site],
                  alpha = 0.05)),
      locations = cells_body(stat_1)
    ) |> 
    cols_width("p.value" ~ px(100))
  )
}


table_save <- function(table, site, table_name, width, type){
  gtsave(table, 
         paste0("../tables/", 
                site, 
                "/", 
                str_trunc(table_name, 10, ellipsis = "_"), 
                today(),
                ".",
                type), 
         vwidth = width)
}

table_one_site <- function(data, 
                           site, 
                           table, 
                           non_columns = NULL, 
                           width = 1000, 
                           type = c("png", "docx"), 
                           other_arguments = list()
                           ) {
  table_data <- summary_table(data, site, table, non_columns, other_arguments)
  if(is.null(table_data)) return()
  walk(type, table_save, 
       table = table_data, site = site, table_name = table, width = width
  )
}

table_all_sites <- function(data, 
                            table,
                            ...){
  melidos_sites$site |> 
    walk(table_one_site, data = data, table = table, ..., .progress = TRUE)
}



#---------------

light_summary_table <- function(data, site, table_name, skip = NULL) {
  site_gt <- melidos_sites$site_name[melidos_sites$site == site]
 
  if(!is.null(skip) & site == "MPI") {
    cat("No data available for site: ", site, ". Skipping table generation", 
        sep = "")
    return()
  }
  
  tbl2_data <- 
    data |> 
    mutate(
      data = map2(data, name, 
                  \(data, name) if(name != "dose") return(data) else {
                    data |> 
                      mutate(metric = metric/1000)
                  }),
      data = map(data, 
                 \(data){
                   data |> 
                     mutate(site = fct_other(site, 
                                             keep = .env$site, 
                                             other_level = "Other sites")
                     )
                 }),
      summary = map(data, 
                    \(x) {
                      test <- 
                        x |> 
                        summarize(
                          .by = site,
                          median = median(metric, na.rm = TRUE),
                          mean = mean(metric, na.rm = TRUE),
                          sd = sd(metric, na.rm = TRUE),
                          p25 = quantile(metric, na.rm = TRUE, p = 0.25),
                          p75 = quantile(metric, na.rm = TRUE, p = 0.75),
                          n = sum(!is.na(metric)),
                          var = var(metric, na.rm = TRUE)
                        ) 
                    }
      ),
      type2 = metric_type
    )
  
  tbl2_data_formatted <- 
    tbl2_data |>
    select(-data) |>
    unnest(summary) |> 
    pivot_wider(id_cols = c(name:metric_type, type2), values_from = median:var, names_from = site) |> 
    rename_with(\(y) str_remove(y, "median_")) |> 
    mutate(Difference = .data[[site]] - `Other sites`, .after = `Other sites`,
           SE_diff = 
             sqrt((.data[[paste0("var_", site)]] / .data[[paste0("n_", site)]]) +
                    (`var_Other sites` / `n_Other sites`))
    ) |> 
    relocate(all_of(site), .before = `Other sites`) |> 
    group_by(metric_type) |> 
    gt(rowname_col = "name") |> 
    gt_multiple(c(site, "Other sites"), merge_desc_columns) |> 
    cols_merge(c(Difference, SE_diff), pattern = "{1} (±{2})") |> 
    cols_hide(type2) |> 
    fmt_number(columns = !starts_with("n_"), rows = type2 %in% c("dynamics", "spectrum")) |> 
    fmt_number(columns = !starts_with("n_"), rows = type2 %in% c("exposure history"),
               decimals = 2) |> 
    fmt_number(columns = !starts_with("n_"), rows = type2 %in% c("level"),
               decimals = 1) |> 
    fmt(columns = !c(starts_with("n_"), name:metric_type, type2),
        rows = type2 %in% c("timing", "duration"), 
        fns = \(x) {
          ifelse(x >= 0, 
                 x |> hms::hms(hours = _) |> strptime(format = "%H:%M:%S") |> format(format = "%H:%M"),
                 {
                   absolute <- 
                     abs(x) |> hms::hms(hours = _) |> strptime(format = "%H:%M:%S") |> format(format = "%H:%M")
                   str_c("−", absolute)
                 })
        }
    ) |> 
    as.data.frame() |> 
    select(1:Difference)
  
  tbl2 <- 
    inject(
    tbl2_data_formatted |> 
    mutate(metric_type = metric_type |> str_to_title(),
           across(c(name),
                  \(x) x |> str_to_title() |>  str_replace_all("_", " ")),
           Unit = c(
             rep(NA, 2),
             rep("lx", 3),
             rep("HH:MM (time-of-day)", 9),
             rep("Hrs:Mins (duration)", 6),
             "klx·h",
             NA
           ),
           .after = name) |> 
    group_by(metric_type) |> 
    gt(rowname_col = "name") |> 
    cols_hide(c(type2, type)) |> 
    site_conv_gt(after = "Unit") |> 
    sub_values(values = "Mder", replacement = "MDER") |> 
    tab_header(paste0("Melanopic EDI derived metrics. Descriptive summary (", table_name, ")")) |> 
    gt_multiple(site, style_tab) |> 
    tab_footnote(
      md("**Median** (25% percentile, 75% percentile), *Mean ± standard deviation* , <span style = 'color:grey'>n=observations</span>")
    )|> 
    fmt_markdown() |> 
    cols_width(
      c(all_of(c(!!site, "Unit", "Other sites"))) ~ px(190),
      everything() ~ px(100)) |> 
    sub_missing() |> 
    tab_style(style = cell_text(weight = "bold"),
              locations = list(
                cells_column_labels(),
                cells_row_groups(),
                cells_title()
              )))
  tbl2
}

light_table_one <- function(data, 
                           site, 
                           table_name, 
                           width = 1000, 
                           type = c("png", "docx"),
                           ...
) {
  table_data <- light_summary_table(data, site, table_name, ...)
  if(is.null(table_data)) return()
  walk(type, table_save, 
       table = table_data, site = site, table_name = table_name, width = width
  )
}


light_table_all <- function(data, 
                            table_name,
                            ...){
  melidos_sites$site |> 
    walk(light_table_one, data = data, table_name = table_name, ..., .progress = TRUE)
}
