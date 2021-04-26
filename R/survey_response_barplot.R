#' Visualise responses
#'
#' Take a consultation response spreadsheet, and turn it into a barplot for each column.
#' This uses the question_types() function to clean data first, including aggregating small categories and detecting free text columns.
#' It then makes barplots on the basis of the frequency tables produced by response_tables().
#'
#' @param dummy_response dataframe
#'
#' @return list of ggplot2 objects
#' @export
#'
#' @examples survey_response_barplot(dummy_response)
survey_response_barplot <- function(dummy_response){
  response_t <- survey_response_tables(dummy_response, survey_question_types(dummy_response))
  plots <- list()
  for (i in names(response_t)){
    plots[[i]] <- response_t[[i]] %>%
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
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))+
      ggplot2::ggtitle(stringr::str_wrap(i, 60))
  }
  return(plots)
}
