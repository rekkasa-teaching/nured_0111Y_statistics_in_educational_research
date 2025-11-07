readr::read_csv("exercises/exercise_1/data/ToolsK_archive.csv") |>
  dplyr::filter(SchoolID %in% c(320000, 180000)) |>
  dplyr::select(SchoolID, ap_ws) |>
  tidyr::drop_na() |>
  dplyr::group_by(SchoolID) |>
  dplyr::summarise(
    n = dplyr::n(),
    m = mean(ap_ws, na.rm = TRUE),
    med = median(ap_ws),
    sd = sd(ap_ws, na.rm = TRUE),
    iqr = IQR(ap_ws, na.rm = TRUE),
    range = max(ap_ws, na.rm = TRUE) - min(ap_ws, na.rm = TRUE),
    .groups = "drop"
  )

readr::read_csv("exercises/exercise_1/data/ToolsK_archive.csv") |>
  dplyr::filter(SchoolID %in% c(320000, 180000)) |>
  dplyr::select(SchoolID, ap_ws) |>
  data.frame()

pp <- readr::read_csv("exercises/exercise_1/data/ToolsK_archive.csv") |>
  dplyr::filter(SchoolID %in% c(320000, 180000)) |>
  dplyr::select(SchoolID, ap_ws) |>
  dplyr::mutate(ap_ws = as.numeric(ap_ws)) |>
  tidyr::drop_na() |>
  dplyr::mutate(SchoolID = factor(SchoolID)) |>
  ggplot2::ggplot(ggplot2::aes(x = ap_ws, fill = SchoolID)) +
  ggplot2::geom_histogram(position = "identity", alpha = 0.6, binwidth = 10) +
  ggplot2::labs(
    x = "Woodcock-Johnson III Applied Problems Score",
    y = "Συχνότητα",
    fill = "Σχολείο"
  ) +
  ggplot2::theme_minimal()
ggplot2::ggsave("test.png", pp)

pp <- readr::read_csv("exercises/exercise_1/data/ToolsK_archive.csv") |>
  dplyr::filter(SchoolID %in% c(320000, 180000)) |>
  dplyr::select(SchoolID, ap_ws) |>
  dplyr::mutate(ap_ws = as.numeric(ap_ws)) |>
  tidyr::drop_na() |>
  dplyr::mutate(SchoolID = factor(SchoolID)) |>
  ggplot2::ggplot(ggplot2::aes(x = SchoolID, y = ap_ws, fill = SchoolID)) +
  ggplot2::geom_boxplot() +
  ggplot2::labs(
    x = "Σχολείο",
    y = "Woodcock-Johnson III Applied Problems Score",
    fill = "Σχολείο"
  ) +
  ggplot2::theme_minimal()
ggplot2::ggsave("test_boxplot.png", pp)
