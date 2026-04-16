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