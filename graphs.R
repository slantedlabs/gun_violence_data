library(tidyverse)
library(ggthemes)
library(cowplot)

### Visual setup

bad.col <- "#ff0b3a"
good.col <- "black"

fill.col <- "gray80"
text.col <- "gray48"
title.col <- "gray26"
fontfam <- "sans"
text.fontfam <- "Palatino"

theme_sl <- function() {
  # theme colors
  plot.bg <- "white"
  panel.bg <- plot.bg
  text.col <- text.col
  plot.title.col <- text.col
  axis.title.col <- text.col
  axis.text.col <- text.col
  axis.tick.col <- text.col
  grid.col <- "gray80"

  theme_wsj() +
  theme(
    plot.margin = margin(0.75, 0.75, 0.5, 0.75, "cm"),    
    text = element_text(family=fontfam),
    plot.background = element_rect(fill = plot.bg, colour = plot.bg),
    panel.background = element_rect(fill = panel.bg),
    axis.text = element_text(colour = axis.text.col, size=18),
    plot.title = element_text(colour = plot.title.col,
                              face = "bold", size = 18, vjust = 1),
    axis.title = element_text(colour = axis.title.col,
                              face = "bold", size = 18),
    panel.grid.major.y = element_line(colour = grid.col),
    strip.text = element_text(size=18),
    axis.ticks = element_line(colour = axis.tick.col)
  )
}


### Graph definitions
state.plot <- function(data, state.name, law.year) {
  data <- subset(data, state==state.name)
  p <- ggplot(data,
              aes(x=year, y=crude.rate)) +
      geom_ribbon(data=subset(data, year <= law.year),
                  aes(ymax=crude.rate.upper, ymin=crude.rate.lower),
                  fill=good.col, alpha=0.1) +
      geom_ribbon(data=subset(data, year >= law.year),
                  aes(ymax=crude.rate.upper, ymin=crude.rate.lower),
                  fill=bad.col, alpha=0.1) +
      geom_line(data=subset(data, year <= law.year),
                col=good.col) +
      geom_line(data=subset(data, year >= law.year),
                col=bad.col) +
      scale_y_continuous(breaks=c(12, 16)) +
      scale_x_continuous(breaks=c(2000, law.year, 2015)) +
      labs(x='', y='') +
      geom_segment(x=law.year, y=9, xend=law.year, yend=17,
                   linetype=5, col=text.col) +
      theme_sl()

  return(p)
}


national.plot <- function(data, hlt.year=0) {
  p <- ggplot(data,
              aes(x=year, y=crude.rate,
                  col=bg.check, alpha=state)) +
    geom_point() +
    scale_alpha_manual(values=rep(0.2,50)) +
    scale_colour_manual(values=c(bad.col, good.col)) +
    scale_fill_manual(values=c(bad.col, good.col)) +
    stat_smooth(inherit.aes=F,
                aes(x=year, y=crude.rate,
                    group=bg.check,
                    fill=bg.check),
                alpha=0.2,
                method='loess',
                geom='ribbon') +
    stat_smooth(inherit.aes=F,
                aes(x=year, y=crude.rate,
                    group=bg.check,
                    col=bg.check),
                method='loess',
                geom='line') +
    scale_y_continuous(breaks=c(10, 20), lim=c(0,20)) +
    guides(colour=F, alpha=F, fill=F) +
    labs(x='', y='') +
    theme_sl()

  if (hlt.year > 0) {
    p <- p + geom_point(data=subset(data, year==hlt.year),
                        alpha=0.8)
  }
  
  return(p)
}


national.plot.density <- function(data, hlt.year=0) {
  if (hlt.year > 0) {
    data <- subset(data, year==hlt.year)
  }
  p <- ggplot(data,
              aes(x=crude.rate)) +
    geom_density(aes(fill=bg.check, col=bg.check),
                 alpha=0.2) +
    scale_colour_manual(values=c(bad.col, good.col)) +
    scale_fill_manual(values=c(bad.col, good.col)) +
    labs(x='', y='') +
    coord_flip() +
    guides(colour=F, fill=F, alpha=F) +
    scale_x_continuous(breaks=c(10, 20), lim=c(0,20)) +
    scale_y_continuous(breaks=c(0.1), lim=c(0,0.14)) +
    theme_sl() +
    theme(axis.text.y=element_blank())

  return(p)
}

national_plot_with_density <- function(nat.p, nat.p.density) {
  aligned.plots <- align_plots(nat.p, nat.p.density, align="v")
  aligned_p <- ggdraw() +
    draw_plot(aligned.plots[[2]], 0.75, 0, 0.25, 1) +
    draw_plot(aligned.plots[[1]], 0, 0, 0.8, 1)

  return(aligned_grids)
}
