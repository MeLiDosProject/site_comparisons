melidos_sites <- 
  tibble(
    site = names(melidos_countries),
    site_name =
      paste(melidos_cities, 
            c("(SE)", "(ES)", "(DE)", "(DE)", "(DE)", "(NL)", "(TR)", "(GH)", "(CR)")
      ) |> 
      replace_values("San Pedro, San José (CR)" ~ "San José (CR)")
  )

melidos_order <- c("RISE", "THUAS", "BAUA", "MPI", "TUM", "FUSPCEU", 
                   "IZTECH", "UCR", "KNUST")

site_conversion <- function(x){
  x |> 
    replace_values(
      from = melidos_sites$site,
      to = melidos_sites$site_name
    )
}

site_conv_mutate <- function(data, site = site, rev = TRUE, other.levels = NULL){
  
  if(rev) melidos_order <- rev(melidos_order)
  
  if(!is.null(other.levels)) {
    melidos_order <- c(other.levels, melidos_order)
  }
  
  factor_conv <- function(x) {
    if(!inherits(x, "factor")) {
      x <- fct(x, levels = melidos_order)
    }
    inject(fct_relevel(x, !!!melidos_order)) |> 
      fct_relabel(site_conversion)
  }
  # browser()
  data |> 
    mutate({{ site }} := {{ site }} |> factor_conv())
}

melidos_colors <- 
  melidos_colors |> set_names(melidos_sites$site_name)

site_conv_gt <- function(table, after = "Overall", rev = FALSE){
  
  if(rev) melidos_order <- rev(melidos_order)
  
  table |> 
    cols_label_with(
      columns = any_of(names(melidos_cities)),
      fn = site_conversion
    ) |> 
    cols_move(any_of(melidos_order), after = after)
}

gt_multiple <- function(table, names, fun){
  names |> purrr::reduce(\(tab, name) tab |> fun(name), .init = table)
}

style_tab <- function(table, column) {
  table |> 
    tab_style(
      style = list(cell_text(color = melidosData::melidos_colors[column])
      ),
      locations = cells_column_labels(any_of(column))
    ) |> 
    tab_style(
      style = list(
        cell_fill(color = melidosData::melidos_colors[column],
                  alpha = 0.05)),
      locations = cells_body(any_of(column))
    )
}

merge_desc_columns <- function(table, column){
  table |> 
    cols_merge(any_of(ends_with(column)),
               pattern = "**{1}** ({4}, {5})<br>*{2} ±{3}*<br><span style = 'color:grey'>n={6}</span>")
}
