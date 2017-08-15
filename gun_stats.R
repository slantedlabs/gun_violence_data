library(tidyverse)
library(cowplot)
library(grid)
library(extrafont)
library(Cairo)

loadfonts()

source("graphs.R")


### Data stuff

gun.stats.all <- function(deaths, cr, cr.upper, cr.lower) {
  deaths <- sum(deaths)
  crude.rate <- sum(cr, na.rm=T)
  crude.rate.upper <- sum(cr.upper, na.rm=T)
  crude.rate.lower <- sum(cr.lower, na.rm=T)
  return(data.frame(deaths, crude.rate, crude.rate.upper, crude.rate.lower))
}

background.check.stats <- function(bg.check) {
  return(data.frame(bg.check))
}

nation.deaths.bg.stats <- function(cr, cr.upper, cr.lower) {
  crude.rate <- sum(cr, na.rm=T)
  crude.rate.upper <- sum(cr.upper, na.rm=T)
  crude.rate.lower <- sum(cr.lower, na.rm=T)
  return(data.frame(crude.rate, crude.rate.upper, crude.rate.lower))
}

death.data <- read.csv("gun_deaths_year_state_clean.csv")
death.data$state <- state.abb[match(death.data$state, state.name)]
law.data <- read.csv("Everytown_research_master.csv")

gun.stats.year.state <- death.data %>%
  group_by(year, state) %>%
  do(gun.stats.all(.$deaths, .$crude.rate, .$crude.rate.upper.95.ci, .$crude.rate.lower.95.ci))

bg.checks.year.state <- filter(law.data, year >= 1999) %>%
  group_by(year, state) %>%
  do(background.check.stats(.$response))

all.data.year.state <- merge(gun.stats.year.state, bg.checks.year.state, by=c("state", "year"))
all.data.year <- all.data.year.state %>%
  group_by(year, bg.check) %>%
  do(nation.deaths.bg.stats(.$crude.rate, .$crude.rate.upper, .$crude.rate.lower))


### Grid layouts
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

grid_mo <- function(dataset) {

  mo.p <- state.plot(dataset, "MO", 2007)

  ### Grid layout stuff
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(9, 12)))

  # Graphs
  print(mo.p, vp = vplayout(1:9, 1:12))

  # Title
  grid.text("Gun Deaths in Missouri",
            vp=vplayout(1, 1:12),
            y=unit(0.4, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=title.col, cex=2))


  ## MAIN TEXT
  xval <- 0.95
  yval <- 0.5
  line.gap <- 0.33
  newline <- function(y, line, gap=line.gap) {
    y - (line * gap)
  }
  block.cex <- 1.3
  block.vp <- vplayout(2, 3)
  block.hjust <- 0

  grid.text(expression("In 2007, Missouri repealed laws requiring criminal background"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 0), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("checks for gun sales by unlicensed sellers, and "),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 1), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

  grid.text(expression("requiring record of the sale to be kept."),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))

}


grid_national <- function(dataset, yr) {
  nat.p <- national.plot(dataset, hlt.year=yr)
  nat.p.density <- national.plot.density(dataset, hlt.year=yr)
  natp_with_den <- national_plot_with_density(nat.p, nat.p.density)

  ### Grid layout stuff
  grid.newpage() 
  pushViewport(viewport(layout = grid.layout(9, 12)))

  # Graphs
  print(natp_with_den, vp = vplayout(3:9, 1:12))

  # Title
  grid.text("Gun Deaths by State",
            vp=vplayout(1, 1:12),
            y=unit(0.4, "npc"),
            gp=gpar(fontfamily=text.fontfam, fontface="bold", col=title.col, cex=2))

  grid.text(yr,
            vp=vplayout(3, 11),
            x=unit(0.7, "npc"),
            y=unit(1.3, "npc"),
            gp=gpar(fontfamily=fontfam, fontface="bold", col=title.col, cex=2))


  ## MAIN TEXT
  xval <- 0
  yval <- 0.65
  line.gap <- 0.33
  newline <- function(y, line, gap=line.gap) {
    y - (line * gap)
  }
  block.cex <- 1.3
  block.vp <- vplayout(2, 2)
  block.hjust <- 0

  grid.text(expression("Over time, gun death rates have stayed the same in states " *
                       phantom("with") *
                       " and " *
                       phantom("without") *
                       " laws"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 0), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
  grid.text(expression(phantom("Over time, gun death rates have stayed the same in states ") *
                       "with" *
                       phantom(" and ") *
                       phantom("without") *
                       phantom(" laws")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 0), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=good.col, cex=block.cex))
  grid.text(expression(phantom("Over time, gun death rates have stayed the same in states ") *
                       phantom("with") *
                       phantom(" and ") *
                       "without" *
                       phantom(" laws")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 0), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=bad.col, cex=block.cex))

  grid.text(expression("requiring background checks for gun sales by unlicensed sellers, but states" *
                       phantom("without")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 1), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
  grid.text(expression(phantom("requiring background checks for gun sales by unlicensed sellers, but states ") *
                       "without"),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 1), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=bad.col, cex=block.cex))

  grid.text(expression("those laws have always had significantly more deaths than those " *
                       phantom("with") *
                       "."),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=text.col, cex=block.cex))
  grid.text(expression(phantom("those laws have always had significantly more deaths than those ") *
                       "with" *
                       phantom(".")),
            vp=block.vp,
            x=unit(xval, "npc"),
            y=unit(newline(yval, 2), "npc"),
            hjust=block.hjust,
            gp=gpar(fontfamily=text.fontfam, col=good.col, cex=block.cex))


}

make_mo_svg <- function(dataset) {
  svg("missouri_gun_deaths.svg", family=fontfam, width = 12, height = 8)
  grid_mo(gun.stats.year.state)
  dev.off()
}

make_national_svg <- function(year=0) {
  if (year > 0) {
    fname <- paste(c("national_gun_deaths_bg_checks_",
                     year,
                     ".svg"),
                   sep="",
                   collapse="")
  } else {
    fname <- paste(c("national_gun_deaths_bg_checks",
                     ".svg"),
                   sep="",
                   collapse="")
  }
  svg(fname, family=fontfam, width = 12, height = 8)
  grid_hectic(all.data.year.state, year)
  dev.off()
}

make_all_national_svgs <- function() {
  for (i in 1999:2015) {
    make_national_svg(year=i)
  }
}
