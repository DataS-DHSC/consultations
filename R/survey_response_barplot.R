#' Visualise responses
#'
#' Take a consultation response spreadsheet (as transformed by Hmisc::describe), and turn it into a barplot.
#'
#' @param survey_response list
#'
#' @return A ggplot2 object
#' @export
#'
#' @examples survey_response_barplot(survey_response)
survey_response_barplot <- purrr::safely(function(survey_response){
  survey_barplot <- survey_response[[1]]$result %>%
    ggplot2::ggplot(ggplot2::aes(x = Response, y = Frequency, fill = Response))+
    ggplot2::geom_col()+
    ggplot2::theme(axis.title.x = ggplot2::element_blank())+
    ggplot2::scale_fill_viridis_d()+
    ggplot2::labs(x = "", y = "Number of responses")+
    ggthemes::theme_calc()+
    ggplot2::theme(
      legend.position = "none",
      plot.background = ggplot2::element_blank(),
      legend.direction = "horizontal",
      legend.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )+
    ggplot2::ggtitle(names(survey_response))

  return(survey_barplot)
})
