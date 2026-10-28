# adapted from plot_osa in the afscOSA package
# only difference is that that plots can be labelled by the model name when they are saved by ggsave


plot_osa_model <- function(input, plot=TRUE, add_agg_CI=TRUE,
                     add_sdnr_CI = TRUE, add_QQ_quantiles=TRUE,
                     use_agg_proportions=TRUE,
                     outpath = NULL, figheight = 8, figwidth = NULL,
                     hjust = -.1, vjust = 1.1, model = NULL) {
  
  # create output filepath if it doesn't already exist
  if(!is.null(outpath)) dir.create(file.path(outpath), showWarnings = FALSE)
  ## helper function so the order of input stays the same when plotted
  fleets <- sapply(input, function(x) x[[1]]$fleet[1])
  fleetf <- function(x) factor(x, levels=fleets)
  # ensure osa inputs are structured properly:
  res <- lapply(input, `[[`, 1) # extracts each element of the list of lists
  if(all(unlist(lapply(res, is.data.frame)))) {
    res <- do.call("rbind", res)
    res$fleet <- fleetf(res$fleet)
  } else {
    stop("The input argument should be a list() of output objects from run_osa. The $res element in one of these lists was not a dataframe.")
  }
  
  pears <- lapply(input, `[[`, 2) # extracts each element of the list of lists
  if(all(unlist(lapply(pears, is.data.frame)))) {
    pears <- do.call("rbind", pears)
    pears$fleet <- fleetf(pears$fleet)
  } else {
    stop("The input argument should be a list() of output objects from run_osa. The $pearson element in one of these lists was not a dataframe.")
  }
  
  # ensure user is only plotting either ages or lengths at one time:
  if(length(unique(res$index_label))>1) stop("you are mixing age and length compositions. please input these separately for plotting purposes.")
  
  # ensure aggregated fit inputs are structured properly:
  agg <- lapply(input, `[[`, 3)
  if(all(unlist(lapply(agg, is.data.frame)))) {
    agg <- do.call("rbind", agg)
    agg$fleet <- fleetf(agg$fleet)
  } else {
    stop("The input argument should be a list() of output objects from run_osa. The $agg element in one of these lists was not a dataframe.")
  }
  # bubble plots
  res <- res |>
    dplyr::mutate(sign = ifelse(resid < 0, "Neg", "Pos"),
                  Outlier = ifelse(abs(resid) > 3, "Yes", "No"))
  bad <- which(abs(res$resid)>6)
  if(length(bad)>0){
    warning("The following OSA residuals set to 6 for plotting: ",
            paste(round(res$resid[bad],2), collapse=' '))
    res$resid[bad] <- 6*sign(res$resid[bad])
  }
  
  bubble_plot <- ggplot(data = res, aes(x = year, y = index,
                                        color = sign, size = abs(resid),
  )) +
    geom_point(alpha=.5) +
    scale_color_manual(values=c("blue","red")) +
    scale_size_continuous(breaks=c(0,2,4,6),
                          limits = c(0, 6),
                          range = c(.1, 3) ) +
    labs(x = NULL, y = 'OSA',#unique(res$index_label),
         color = "Sign", #sign = "abs(Resid)",
         size = "|Resid|")+ #alpha = "abs(Resid)") +
    facet_wrap(~fleet, nrow = 1) +
    theme_bw(base_size=10) +
    # try to reduce white space around legend
    theme(
      legend.title = element_text(size = 9),
      legend.box.spacing = unit(0, "pt"),
      legend.margin=margin(t = 0, b = 0, unit = "pt"),
      legend.box.margin=margin(t = -5, r = 0, b = 0, l = 0, unit = "pt"
      )) +
    
    theme(legend.position = "top")
  if(length(unique(res$index)) < 20){
    bubble_plot <- bubble_plot +
      scale_y_continuous(breaks = unique(agg$index), labels = unique(agg$index),
                         limits = c(min(agg$index), max(agg$index)))
  } else {
    bubble_plot <- bubble_plot +
      scale_y_continuous(limits = c(min(agg$index), max(agg$index)))
  }
  
  
  pears <- pears  |>
    dplyr::mutate(sign = ifelse(resid < 0, "Neg", "Pos"),
                  Outlier = ifelse(abs(resid) > 3, "Yes", "No"))
  bad <- which(abs(pears$resid)>6)
  if(length(bad)>0){
    warning("The following Pearson residuals were set to 6 for plotting: ",
            paste(round(pears$resid[bad],2), collapse=' '))
    pears$resid[bad] <- 6*sign(pears$resid[bad])
    
  }
  bubble_pearson <- ggplot(data = pears, aes(x = year, y = index,
                                             color = sign, size = abs(resid),
                                             #shape = Outlier,
                                             #alpha = abs(resid)
  )) +
    geom_point(alpha=.5) +
    scale_color_manual(values=c("blue","red")) +
    scale_size_continuous(breaks=c(0,2,4,6),
                          limits = c(0, 6),
                          range = c(.1, 3) )+
    facet_wrap(~fleet, nrow = 1) +
    theme_bw(base_size = 10) +
    labs(y='Pearson', x=NULL)+
    theme(legend.position='none')
  if(length(unique(res$index)) < 20){
    bubble_pearson <- bubble_pearson +
      scale_y_continuous(breaks = unique(agg$index), labels = unique(agg$index),
                         limits = c(min(agg$index), max(agg$index)))
  } else {
    bubble_pearson <- bubble_pearson +
      scale_y_continuous(limits = c(min(agg$index), max(agg$index)))
  }
  
  
  # QQ plots
  
  sdnr <- res |>
    dplyr::group_by(fleet) |>
    dplyr::summarise(
      df=dplyr::n()-1,
      HCI = sqrt(qchisq(.975,df)/df),
      LCI = sqrt(qchisq(.025,df)/df),
      est= sd(resid))  |>
    dplyr::mutate(
      sdnr=paste0('SDNR=',sprintf('%.2f', est))
    )
  if(add_sdnr_CI)
    sdnr <- dplyr::mutate(sdnr,
                          sdnr=paste0(sdnr,'\n(', sprintf('%.2f', LCI), '-', sprintf('%.2f', HCI),')'))
  
  # calculate 95% interval for the lower and upper tail probabilities
  get_quantile_limit <- function(q,N, lower=TRUE, alpha=.05){
    r <- pmax(1, round(q * (N + 1))) # which point corresponds to the qth order statistic
    # The exact distribution of the CDF at the r-th order statistic is Beta(r, N - r + 1)
    if(lower) x <-qbeta(alpha / 2, r, N - r + 1) else
      x<-qbeta(1 - alpha / 2, r, N - r + 1)
    return(qnorm(x))
  }
  tails <- res |>
    dplyr::group_by(fleet) |>
    dplyr::summarise(
      lower.min=get_quantile_limit(q=0.025, N=dplyr::n(), lower=TRUE),
      lower.max=get_quantile_limit(q=0.025, N=dplyr::n(), lower=FALSE),
      upper.min=get_quantile_limit(q=0.975, N=dplyr::n(), lower=TRUE),
      upper.max=get_quantile_limit(q=0.975, N=dplyr::n(), lower=FALSE),
      text=paste0('2.5% quantiles    \nLow= ',round(quantile(resid, probs=c(0.025)),2),
                  ' (', sprintf('%.2f', lower.min), ' \u2013 ', sprintf('%.2f', lower.max),')\n',
                  'High= ', round(quantile(resid, probs=c(0.975)),2),
                  ' (', sprintf('%.2f', upper.min), ' \u2013 ', sprintf('%.2f', upper.max),')'))
  
  qq_plot <- ggplot() +
    stat_qq(data = res, aes(sample = resid), col = "blue") +
    geom_abline(slope = 1, intercept = 0) +
    labs(x = NULL, y = 'OSA Q-Q Plot') +
    facet_wrap(~fleet, nrow = 1) +
    theme_bw(base_size = 10) +
    geom_text(data = sdnr, size=3,
              aes(x = -Inf, y = Inf, label = sdnr),
              hjust = hjust, vjust = vjust)
  if(add_QQ_quantiles){
    qq_plot <- qq_plot +
      geom_text(data = tails, size=3,
                aes(x = Inf, y = -Inf, label = text),
                hjust = vjust, vjust = hjust)
  }
  
  if(use_agg_proportions){
    agg$obs <- agg$obs_prop
    agg$exp <- agg$exp_prop
    agg$lwr <- agg$lwr_prop
    agg$upr <- agg$upr_prop
    ylab <- 'Aggregated Proportions'
  } else {
    ylab <- 'Aggregated Counts'
  }
  agg.N <- dplyr::slice_head(agg, n=1, by='fleet')
  agg_plot <- ggplot(data = agg) +
    geom_bar(aes(x = index, y = obs), stat = 'identity',
             color = "blue", fill = 'blue', alpha=0.4) +
    geom_point(aes(x = index, y = exp), color = 'red') +
    geom_line(aes(x = index, y = exp), color = 'red') +
    facet_wrap(~fleet, nrow = 1) +
    labs(x = NULL, y = ylab) +
    theme_bw(base_size = 10) +
    geom_text(data = agg.N, size=3,
              aes(x = Inf, y = Inf, label = paste("ISS=",ISS,'\n','ESS=', ESS)),
              hjust =1 -hjust, vjust = vjust)
  if(length(unique(agg$index)) < 20){
    agg_plot <- agg_plot +
      scale_x_continuous(breaks = unique(agg$index), labels = unique(agg$index))
  }
  if(add_agg_CI)
    agg_plot <- agg_plot +
    geom_linerange(mapping=aes(x=index, y=exp, ymin=lwr, ymax=upr),
                   color='red', alpha=.5)
  # for lots of bins we need the bubble plots to have more space
  if(length(unique(res$index)) < 20) {
    myrelht <- c(5,5, 4.5,4)
  } else {
    myrelht <- c(5,5, 7.5,7)
  }
  
  p <- cowplot::plot_grid(agg_plot, qq_plot, bubble_plot, bubble_pearson,
                          nrow = 4, rel_heights = myrelht)
  
  # create file name and file path
  fn <- paste0("osa_", model, tolower(unique(res$index_label)), "_diagnostics.png")
  if(is.null(outpath)) {
    fp <- fn
  } else {
    fp <- here::here(outpath, fn)
  }
  
  # use the fleet number to scale figure dimensions
  nflt <- length(unique(res$fleet))
  if(is.null(figwidth)) {
    if(nflt <= 2 | length(unique(res$index)) > 60 | max(abs(res$resid)) >= 5) {
      figwidth <- nflt * 5
    } else {
      figwidth <- nflt * 3
    }
  }
  
  # save and print figure
  if(!is.null(outpath))
    ggsave(plot = p, filename = fp, units = 'in', bg = 'white', height = figheight,
           width = figwidth, dpi = 300)
  if(plot){
    print(p)
    return(p)
  }
  return(list(bubble = bubble_plot,
              bubble_pearson=bubble_pearson,
              qq = qq_plot,
              aggcomp = agg_plot))
}
