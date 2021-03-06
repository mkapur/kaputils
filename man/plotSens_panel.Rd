% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotSens_panel.R
\name{plotSens_panel}
\alias{plotSens_panel}
\title{plotSens_panel
a four-panel transparency shade plot for sensitivities}
\usage{
plotSens_panel(
  rootdir,
  spr_csv = NA,
  b.name = NA,
  f.name = NA,
  pattern = NA,
  subpattern = NA,
  plotloc = NA,
  doLegend = T
)
}
\arguments{
\item{b.name}{the character column name of the biomass ref point; can rename to change y lab}

\item{f.name}{the character column name of the fishing ref point}

\item{pattern}{1st order directory character}

\item{subpattern}{2nd order directory character}

\item{plotloc}{where to save plot}

\item{doLegend}{logical. do you want to write a legend (not advised for huge rep sets)}

\item{mq_csv}{optional; if you already generated a MQ csv using extractResults it will read from that, otherwise need pattern/subpattern}
}
\description{
plotSens_panel
a four-panel transparency shade plot for sensitivities
}
