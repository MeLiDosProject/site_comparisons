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
  # browser()
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
                        x |> 
                        summarize(
                          .by = site,
                          median = median(metric, na.rm = TRUE),
                          mean = mean(metric, na.rm = TRUE),
                          sd = sd(metric, na.rm = TRUE),
                          p25 = quantile(metric, na.rm = TRUE, p = 0.25),
                          p75 = quantile(metric, na.rm = TRUE, p = 0.75),
                          n = sum(!is.na(metric)),
                          var = var(metric, na.rm = TRUE),
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
                    (`var_Other sites` / `n_Other sites`)),
           t_value = 
             (.data[[paste0("mean_", site)]] - `mean_Other sites`) /
             sqrt(
               (.data[[paste0("sd_", site)]]^2)/.data[[paste0("n_", site)]] + (`sd_Other sites`^2)/`n_Other sites`
             ),
           df = 
             ((((.data[[paste0("sd_", site)]]^2)/.data[[paste0("n_", site)]] + (`sd_Other sites`^2)/`n_Other sites`)^2) /
             (
               (((.data[[paste0("sd_", site)]]^2)/.data[[paste0("n_", site)]])^2) / (.data[[paste0("n_", site)]] - 1) +
                 (((`sd_Other sites`^2)/`n_Other sites`)^2) / (`n_Other sites` - 1)
             )) |> round(),
           p_value = (2*pt(-abs(t_value), df))
           
    ) |> 
    relocate(all_of(site), .before = `Other sites`) |> 
    group_by(metric_type) |> 
    gt(rowname_col = "name") |> 
    gt_multiple(c(site, "Other sites"), merge_desc_columns) |> 
    cols_merge(c(Difference, SE_diff), pattern = "{1} (±{2})") |> 
    cols_hide(type2) |> 
    fmt(p_value, fns = style_p_md) |> 
    cols_merge(c(p_value, t_value, df), pattern = "{1}; t({3})={2}") |> 
    fmt_number(columns = t_value, decimals = 2) |> 
    fmt_number(columns = -c(starts_with("n_"), p_value, t_value, df), rows = type2 %in% c("dynamics", "spectrum")) |> 
    fmt_number(columns = -c(starts_with("n_"), p_value, t_value, df), rows = type2 %in% c("exposure history"),
               decimals = 2) |> 
    fmt_number(columns = -c(starts_with("n_"), p_value, t_value, df), rows = type2 %in% c("level"),
               decimals = 1) |> 
    fmt(columns = -c(starts_with("n_"), p_value, t_value, df, name:metric_type, type2),
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
    select(1:Difference, p_value)
  
  tbl2 <- 
    inject(
    tbl2_data_formatted |> 
    mutate(metric_type = metric_type |> str_to_title(),
           across(c(name),
                  \(x) x |> str_to_title() |>  str_replace_all("_", " ")),
           Unit = c(
             rep(NA, 2),
             rep("lx (rescaled from geometric mean)", 3),
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
    tab_footnote(
      md("Test for significance with two-sided Welch's t-test. Degrees of freedom (df) are calculated based on weighted df's for both groups"),
      locations = cells_column_labels(p_value)
    ) |> 
    fmt_markdown() |> 
    cols_label(p_value = "Significance") |> 
    cols_width(
      c(all_of(c(!!site, "Unit", "Other sites"))) ~ px(190),
      p_value ~ px(120),
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

general_summary_table <- function(data, site_choice) {
  tbl1_nosite <-
    tbl1_data |> 
    filter_out(site %in% c(site_choice, "Overall")) |> 
    summarize(
      site = "Other sites",
      across(c(partN:validdaysN_chest, starts_with("daytype_")),
             \(x) {
               x |> mean(na.rm = TRUE) |> round()
             }), 
      across(nonwear:total, as.duration),
      nonwear_pct = nonwear/total
    )
  
  tbl_general <- 
    tbl1_data |> 
    filter(site %in% site_choice) |> 
    select(site, c(partN:validdaysN_chest, starts_with("daytype_"))) |> 
    bind_rows(tbl1_nosite)
  
  tbl_general_formatted <- 
    tbl_general |> 
    gt() |> 
    cols_merge(starts_with("partN"), pattern = "{1}<br>(**g:{2}**, c:{3})") |> 
    cols_merge(starts_with("daysN"), pattern = "{1}d<br>(**g:{2}d**, c:{3}d)") |> 
    cols_merge(starts_with("validdaysN"), pattern = "{1}d<br>(**g:{2}d**, c:{3}d)") |> 
    cols_merge(starts_with("nonwear"), pattern = "{1} ({2})") |> 
    cols_merge(c(weekdayN, weekendN), pattern = "{1}wd / {2}we") |> 
    cols_merge(c(daytype_free, daytype_work), pattern = "{2}work / {1}free") |> 
    cols_hide(c(starts_with("weekdayN_"), starts_with("weekendN_"), daytype_NA)) |> 
    fmt_percent(nonwear_pct, decimals = 1) |> 
    fmt_duration(all_of(c("nonwear", "total")),
                 input_units = "secs", max_output_units = 2
    ) |> 
    cols_move(nonwear, after = total) |> 
    cols_move(daytype_free, after = weekdayN) |> 
    cols_label_with(everything(), fn= str_to_sentence) |> 
    as.data.frame() |> 
    select(site:partN, daysN, weekdayN, daytype_free, total, nonwear, validdaysN)
  
  tbl_general_transposed <- 
    tbl_general_formatted |> 
    t()
  
  colnames(tbl_general_transposed) <- tbl_general_transposed[1,]
  
  tbl_general_transposed <- 
    tbl_general_transposed |> 
    as_tibble(rownames = "description") |> 
    slice(-1) |> 
    mutate(
      description = replace_values(
        description,
        "site" ~ "Institution",
        "partN" ~ "Participants",
        "daysN" ~ "Participant-days",
        "weekdayN" ~ "Weekday / weekend",
        "daytype_free" ~ "Workday / free",
        "date_median" ~ "Collection dates",
        "total" ~ "Participant time",
        "nonwear" ~ "Nonwear time (%)",
        "validdaysN" ~ "Complete days (>80% data)",
      ),
    )
  
  tbl_general <-
    tbl_general_transposed |> 
    gt(rowname_col = "description") |> 
    # cols_move(Overall, 1) |> 
    sub_missing() |> 
    gt_multiple(names(melidos_cities), style_tab) |> 
    tab_style(style = cell_text(align = "center"), 
              locations = list(cells_body(),
                               cells_column_labels())) |> 
    tab_style(style = cell_text(weight = "bold", ), 
              locations = list(cells_row_groups(),
                               cells_column_labels())) |> 
    fmt_markdown() |> 
    tab_footnote(
      "g: glasses-mounted measurement device; c: chest-level measurement device",
      locations = cells_stub(c("Participants", "Participant-days", "Complete days (>80% data)"))
    ) |> 
    tab_footnote(
      "w: weeks, d: days, h: hours, m: minutes, s: seconds",
      locations = cells_stub(c("Participant time", "Participant-days", "Nonwear time (%)", 
                               "Complete days (>80% data)"))
    ) |> 
    site_conv_gt(rev = FALSE, "Other sites") |> 
    cols_move_to_start(site_choice) |> 
    tab_footnote(
      "On average",
      locations = cells_column_labels("Other sites")
    )
  
  tbl_general
}

general_table_one <- function(data, 
                              site, 
                              width = 1000, 
                              type = c("png", "docx"),
                              ...
) {
  table_data <- general_summary_table(data, site, ...)
  if(is.null(table_data)) return()
  walk(type, table_save, 
       table = table_data, site = site, table_name = "General", width = width
  )
}

general_table_all <- function(data, 
                              ...){
  melidos_sites$site |> 
    walk(general_table_one, data = data, ..., .progress = TRUE)
}

