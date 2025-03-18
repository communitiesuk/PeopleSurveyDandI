#' Summarize by group
#'
#' @param data
#' @param group_var
#' @param group_name
#'
#' @returns
#' @export
#'
#' @examples
summarize_by_group <-function(data, group_var, group_name) {
  data |>
    dplyr::mutate({{group_var}} := if (is.double({{group_var}})) as.factor(haven::as_factor({{group_var}})) else as.factor({{group_var}})) |>  # Ensure consistent factor type
    dplyr::group_by({{group_var}}) |>
    dplyr::summarise(dplyr::across(c(ees, if_p, pnE01, pnE03),
                     list(count = ~sum(!is.na(.)), mean = ~mean(., na.rm = TRUE), sd = ~sd(., na.rm = TRUE)),
                     .names = "{col}_{fn}")) |>
    dplyr::left_join(dplyr::count(data |> dplyr::mutate({{group_var}} := as.factor(haven::as_factor({{group_var}}))), {{group_var}}, E01_grouped) |>
                tidyr::pivot_wider(names_from = E01_grouped, values_from = n, values_fill = 0),
              by = as.character(rlang::enexpr(group_var))) |>
    dplyr::left_join(dplyr::count(data |> dplyr::mutate({{group_var}} := as.factor(haven::as_factor({{group_var}}))), {{group_var}}, E03_grouped) |>
                tidyr::pivot_wider(names_from = E03_grouped, values_from = n, values_fill = 0),
              by = as.character(rlang::enexpr(group_var)), suffix = c("E01", "E03")) |>
    dplyr::mutate(Char = dplyr::case_when(
      is.na(as.character({{group_var}})) ~ paste0(as.character(rlang::enexpr(group_var)), "NA"),
      as.character({{group_var}}) == 'Prefer not to say' ~ paste0(as.character(rlang::enexpr(group_var)), "PNTS"),
      TRUE ~ as.character({{group_var}})
    )) |>
    dplyr::mutate(Group = group_name) |>
    dplyr::select(Group, Char, everything(), -{{group_var}})
}
