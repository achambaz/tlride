
---
knit: "bookdown::render_book"
title: "A Ride in Targeted Learning Territory"
author: ["David Benkeser (Emory University)", "Antoine Chambaz (Université Paris Descartes)"]
date: "2018-10-18"
description: "To do..."
encoding: "UTF-8"
github-repo: achambaz/tlride
site: bookdown::bookdown_site
documentclass: book
geometry: "textwidth=6in,textheight=8in"
classoption: "openright,twoside"
fontsize: 11pt
bibliography: "bibliography.bib"
link-citation: true
cover-image: "cover.jpg"
favicon: "favicon.png"
---


# Welcome {-} 

This is either the  website or the text called __"A  Ride in Targeted Learning
Territory"__. In  the former case, the  text can be downloaded  by clicking on
the dedicated button in  the top part of the webpage. In  the latter case, the
website can be browsed [here](http://achambaz.github.io/tlride "TLRIDE").


<img src="cover.jpg" width="70%" style="display: block; margin: auto;" />

<div class="center">
<p>Long Mendocino Drive (detail, <a href="https://www.lianasteinmetz.com/">Liana Steinmetz</a>)</p>
</div>

__Organization__ 

The text  takes the  form of  a series  of brief  sections. The  main sections
combine theoretical and computational developments. The code is written in the
programming  language [`R`](https://www.r-project.org).   `R`  is widely  used
among statisticians  and data scientists  to develop statistical  software and
data analysis.

Regularly, a section  is inserted that proposes exercizes.   Each such section
is indicated by the  <span style="color:red">&#9881;</span> \gear symbol.  The
symbol  <span  style="color:red">&#9761;</span> \stixdanger{}  also  indicates
those sections whose content is more involved.

__Overview__

After a short introduction, we present the reproducible experiment that will
play a central role throughout the text. Then, we introduce the main parameter
of interest.  We  comment upon some of  its properties that are  useful from a
statistical perspective.  This paves  the way to  the presentation  of several
estimators that  are increasingly more powerful  statistically. The discussion
takes us into *targeted learning territory*.


__Audience__

The  text  might  be  of  interest  to  students  in  statistics  and  machine
learning. It might also serve as a gentle introduction to targeted learning in
both its theoretical and computational  aspects before delving deeper into the
literature [@TMLEbook11], [@TMLEbook18].

__Technical details__

The   text   is    written   in   [`RMarkdown`](https://rmarkdown.rstudio.com)
with         [`bookdown`](https://bookdown.org).         Assuming         that
the  [`knitr`](https://yihui.name/knitr/)   package  is  installed,   you  can
retrieve all the `R` code by running


```r
knitr::purl("abcd.Rmd")
```

<!--It is automatically rebuilt from
[source](https://github.com/achambaz/tlride)                                by
[travis](http://travis-ci.org/).-->
