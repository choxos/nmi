#' Format treatment effects table
#'
#' Formats posterior summaries into a nice table with credible intervals.
#'
#' @param summary_tab A data frame. The output of \code{\link{NMA_NMI_summary}},
#' \code{\link{NMA_Metareg_summary_2D}}, or \code{\link{ML_NMR_summary_2D}}.
#' 
#' @return A single row data frame containing posterior medians and 95% CrIs for
#' all treatment effects.
#'
#' @export
#' @importFrom dplyr mutate select
#' @importFrom reshape2 dcast
result_table <- function(summary_tab) {
  out <- summary_tab %>% 
    mutate(Est = paste0(
      sprintf('%.2f', `50%`), ' [',
      sprintf('%.2f', `2.5%`), ' ;',
      sprintf('%.2f', `97.5%`), ']')
    ) %>% 
    mutate(Parameter = dplyr::recode(Parameter,
                                     "D[1,2]" = "$d_{\\mathrm{AB}}$",
                                     "D[1,3]" = "$d_{\\mathrm{AC}}$",
                                     "D[1,4]" = "$d_{\\mathrm{AD}}$",
                                     "D[2,3]" = "$d_{\\mathrm{BC}}$",
                                     "D[2,4]" = "$d_{\\mathrm{BD}}$",
                                     "D[3,4]" = "$d_{\\mathrm{CD}}$"
    )) %>% 
    select(Parameter, Est) %>% 
    dcast(NULL ~ Parameter, value.var = 'Est') %>% 
    select(-1)
  
  return(out)
}

#' NMI interpolation diagnostic plot
#'
#' Creates a diagnostic plot showing observed vs predicted treatment effects
#' to assess goodness of interpolation.
#'
#' @param NMI_object The output of \code{\link{NMI_interpolation}}.
#' 
#' @return A ggplot object displaying the predicted TE estimates from the 
#' NMI algorithm vs. the observed TE estimates.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_segment theme element_blank
#' @importFrom ggplot2 element_line element_text xlab ylab ggtitle scale_fill_brewer
NMI_diagnostic_plot <- function(NMI_object) {
  df <- NMI_object$Diagnostics
  df$Study <- factor(df$Study, levels = sort(unique(df$Study)))
  
  p <- ggplot(df, aes(TE_orig, TE_pred, fill = Study)) + 
    geom_segment(x = min(min(df$TE_orig), min(df$TE_pred)), 
                 y = min(min(df$TE_orig), min(df$TE_pred)),
                 xend = max(max(df$TE_orig), max(df$TE_pred)),
                 yend = max(max(df$TE_orig), max(df$TE_pred)),
                 linetype = "dashed",
                 color = "red") + 
    geom_point(col = 'royal blue', size = 4,
               shape = 21, stroke = 1) + 
    theme(panel.background = element_blank(),
          legend.position = 'top',
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = .5)) + 
    xlab('Observed treatment effect estimate') + 
    ylab('Predicted treatment effect estimate') + 
    ggtitle('NMI goodness of fit') + 
    scale_fill_brewer(palette = "Set1")
  
  return(p)
}

#' Interactive NMI interpolation diagnostic plot
#'
#' Creates an interactive diagnostic plot showing observed vs predicted treatment effects
#' to assess goodness of interpolation.
#'
#' @param NMI_object The output of \code{\link{NMI_interpolation}}.
#' 
#' @return An interactive plot displaying the predicted TE estimates from the 
#' NMI algorithm vs. the observed TE estimates.
#'
#' @export
#' @importFrom plotly ggplotly
#' @importFrom ggplot2 ggplot aes geom_point geom_segment theme element_blank
#' @importFrom ggplot2 element_line element_text xlab ylab ggtitle scale_fill_brewer
NMI_diagnostic_plotly <- function(NMI_object) {
  df <- NMI_object$Diagnostics
  df$Text <- apply(df, 1,
                   function(u) {
                     v <- u[2:(length(u) - 2)]
                     paste0('Study ', u[1], '\n',
                            paste0(names(v), ' = ', 
                                   sprintf('%.1f', 100 * as.numeric(v)), '%', '\n', 
                                   collapse = ""))
                   })
  
  p <- ggplot(df, aes(TE_orig, TE_pred, text = Text,
                      fill = as.factor(Study))) + 
    geom_segment(x = min(min(df$TE_orig), min(df$TE_pred)), 
                 y = min(min(df$TE_orig), min(df$TE_pred)),
                 xend = max(max(df$TE_orig), max(df$TE_pred)),
                 yend = max(max(df$TE_orig), max(df$TE_pred)),
                 linetype = "dashed", color = "red") + 
    geom_point(size = 3, shape = 21, col = 'royal blue') + 
    theme(panel.background = element_blank(),
          legend.position = 'none',
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size = 13, face = "bold"),
          axis.title.y = element_text(size = 13, face = "bold"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 14, face = "bold", hjust = .5)) + 
    xlab('Observed treatment effect estimate') + 
    ylab('Predicted treatment effect estimate') + 
    ggtitle('NMI goodness of fit') + 
    scale_fill_brewer(palette = "Set1")
  
  return(ggplotly(p, tooltip = "text"))
}

#' Forest plot for treatment effects comparison
#'
#' Creates a forest plot comparing treatment effects from different methods.
#'
#' @param NMA_summary The output of \code{\link{NMA_NMI_summary}} for an NMA run.
#' @param NMR_summary The output of \code{\link{NMA_Metareg_summary_2D}}.
#' @param ML_NMR_summ The output of \code{\link{ML_NMR_summary_2D}}.
#' @param NMI_summary The output of \code{\link{NMA_NMI_summary}} for an NMI run.
#' @param trt_effs A vector of the true treatment effect used to generate the 
#' data. Default is NULL.
#' 
#' @return A ggplot2 object.
#'
#' @export
#' @importFrom ggplot2 ggplot aes geom_segment geom_point geom_hline coord_flip
#' @importFrom ggplot2 theme element_blank element_line element_text ylab
#' @importFrom lemon facet_rep_wrap
#' @importFrom dplyr mutate
result_forest_plot <- function(NMA_summary, NMR_summary, 
                               ML_NMR_summ, NMI_summary,
                               trt_effs = NULL) {
  df <- rbind(NMA_summary, NMR_summary, ML_NMR_summ, NMI_summary)
  if (!is.null(trt_effs)) df <- cbind(df, trt_eff = as.numeric(rep(trt_effs, 4)))
  
  df <- df %>% 
    mutate(Parameter = dplyr::recode(Parameter,
                                     "D[1,2]" = "hat(d)[AB]",
                                     "D[1,3]" = "hat(d)[AC]",
                                     "D[1,4]" = "hat(d)[AD]",
                                     "D[2,3]" = "hat(d)[BC]",
                                     "D[2,4]" = "hat(d)[BD]",
                                     "D[3,4]" = "hat(d)[CD]"))
  
  df$Method <- factor(rep(c('NMA', 'NMR', 'ML-NMR', 'NMI'), each = 6), 
                      levels = c('NMA', 'NMR', 'ML-NMR', 'NMI'))
  
  p <- ggplot(df, aes(Method, `50%`)) + 
    geom_segment(mapping = aes(x = Method, xend = Method,
                               y = `2.5%`, yend = `97.5%`),
                 col = 'royal blue', size = 1) + 
    geom_point(col = 'red', size = 2)
  
  if (!is.null(trt_effs)) {
    p <- p + 
      geom_hline(df, mapping = aes(yintercept = trt_eff),
                 col = 'dark green', linetype = 'dashed', size = 1)
  }
  
  p <- p + 
    facet_rep_wrap(. ~ Parameter, ncol = 1,
                   strip.position = 'left',
                   labeller = label_parsed,
                   repeat.tick.labels = 'all') + 
    coord_flip() + 
    theme(panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          panel.grid.major = element_line(colour = "grey92"),
          axis.title.x = element_text(size = 12, face = "bold"),
          axis.title.y = element_text(size = 12, face = "bold"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          strip.text = element_text(size = 11, face = 'bold'),
          strip.text.y.left = element_text(angle = 0)) + 
    ylab("Parameter estimate (95% CrI)")
  
  return(p)
}

#' Display results table with formatting
#'
#' Creates a formatted table for displaying results from different methods.
#'
#' @param NMA_results The output of \code{\link{result_table}} for an NMA run.
#' @param NMR_results The output of \code{\link{result_table}} for an NMR run.
#' @param ML_NMR_results The output of \code{\link{result_table}} for an ML-NMR run.
#' @param NMI_results The output of \code{\link{result_table}} for an NMI run.
#' 
#' @return A formatted kableExtra table.
#'
#' @export
#' @importFrom kableExtra kable kable_styling row_spec
#' @importFrom tibble add_column
display_result_table <- function(NMA_results, NMR_results, 
                                 ML_NMR_results, NMI_results) {
  result_df <- rbind(NMA_results, NMR_results, ML_NMR_results, NMI_results)
  
  if (ncol(result_df) >= 1) {
    result_df <- result_df %>%
      add_column(Method = c('NMA', 'NMR', 'ML-NMR', 'NMI'), .before = 1)
  } else {
    result_df$Method <- c('NMA', 'NMR', 'ML-NMR', 'NMI')
  }
  
  result_df %>%
    kable(align = 'c') %>%
    kable_styling(full_width = FALSE, font_size = 14,
                  bootstrap_options = c("striped")) %>%
    row_spec(0, font_size = 14)
}