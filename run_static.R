# ---------------------------------------------------------------------------
# Static perceptual map. In RStudio: source() this file.
# The plot appears in the Plots pane and is written to outputs/.
# ---------------------------------------------------------------------------
suppressPackageStartupMessages({
  library(ggplot2); library(grid); library(patchwork); library(scales)
})
# ---------------------------------------------------------------------------
# Find the project, wherever this was started from. This has to sit inline: it
# runs before any other file can be sourced. It tries the path R was given on
# the command line, then the file being source()d, then the working directory,
# and from whichever it finds walks up to the folder holding R/ and
# data/cars.csv. Windows drive roots and Unix "/" both terminate the walk.
# ---------------------------------------------------------------------------
PM_ROOT <- local({
  looks_right <- function(d)
    dir.exists(file.path(d, "R")) && file.exists(file.path(d, "data", "cars.csv"))
  start <- NULL
  a <- commandArgs(FALSE)
  i <- grep("^--file=", a)
  if (length(i)) start <- sub("^--file=", "", a[i[length(i)]])
  if (is.null(start)) {
    for (k in rev(seq_len(sys.nframe()))) {
      fr <- sys.frame(k)
      if (!is.null(fr$ofile)) { start <- fr$ofile; break }          # source()
      if (identical(sys.function(k), base::sys.source) &&           # sys.source()
          !is.null(fr$file)) { start <- fr$file; break }
    }
  }
  from <- if (is.null(start)) getwd()
          else dirname(normalizePath(start, winslash = "/", mustWork = FALSE))
  for (cand in unique(c(from, getwd()))) {
    d <- normalizePath(cand, winslash = "/", mustWork = FALSE)
    repeat {
      if (looks_right(d)) return(d)
      up <- dirname(d)
      if (identical(up, d)) break          # "/" on Unix, "C:/" on Windows
      d <- up
    }
  }
  stop("Could not find the project: no folder above '", getwd(),
       "' contains both R/ and data/cars.csv. Open perceptual_map.Rproj, or ",
       "setwd() to the project folder.", call. = FALSE)
})

#' Path to something inside the project, whatever the working directory is.
pm_path <- function(...) file.path(PM_ROOT, ...)

for (f in c("silhouettes", "pca", "layout", "plot")) source(pm_path("R", paste0(f, ".R")), encoding = "UTF-8")

cars <- read.csv(pm_path("data", "cars.csv"), stringsAsFactors = FALSE)
m    <- fit_map(cars)

cat(sprintf("%d cars | %d attributes | PC1 %.1f%%  PC2 %.1f%%  (%.1f%% total)\n",
            nrow(cars), length(m$vars), m$ve[1], m$ve[2], m$ve[1] + m$ve[2]))
cat("  PC1 =", axis_name(m$loadings, "PC1"), "\n")
cat("  PC2 =", axis_name(m$loadings, "PC2"), "\n")

DEV <- c(12.5, 9.5)
p <- compose_map(m, car_pt = 40, dev_in = DEV)

dir.create(pm_path("outputs"), showWarnings = FALSE)
ggsave(pm_path("outputs", "map.png"), p, width = DEV[1], height = DEV[2], dpi = 200,
       device = pm_png(), limitsize = FALSE, bg = pm_bg(FALSE))
#' What to tell the user when cairo is unavailable. Only macOS needs XQuartz;
#' Windows and Linux R builds normally ship with cairo already.
pdf_hint <- function(sysname = Sys.info()[["sysname"]]) {
  base <- paste("note: cairo unavailable - PDF written with the base device, which",
                "substitutes ASCII for the em-dash and arrow glyphs.")
  paste(base, switch(sysname,
    Darwin  = "Install XQuartz (https://www.xquartz.org) if you need exact PDF typography.",
    Windows = "This is unusual on Windows; the PNG exports are unaffected.",
    "Install the cairo development libraries if you need exact PDF typography."))
}

# cairo_pdf gives proper UTF-8 text but needs XQuartz on macOS. capabilities()
# can report TRUE while the DLL still fails to load at draw time, and that
# failure is a WARNING, not an error -- so verify the file actually landed
# rather than trusting the call to have worked.
save_pdf <- function(file, plot) {
  try_dev <- function(dev) {
    unlink(file)
    # A cairo device that cannot open falls back to R's DEFAULT device, which
    # quietly writes an Rplots.pdf into the working directory -- litter now
    # that the project can be run from anywhere. Point the default at a null
    # device for the duration, close anything the attempt leaves open, and
    # sweep up an Rplots.pdf if one still appears.
    had_rplots <- file.exists("Rplots.pdf")
    keep_dev <- grDevices::dev.cur()
    old_opt <- getOption("device")
    options(device = function(...) grDevices::pdf(NULL))
    on.exit({
      options(device = old_opt)
      while (grDevices::dev.cur() != keep_dev && grDevices::dev.cur() > 1) grDevices::dev.off()
      if (!had_rplots && file.exists("Rplots.pdf")) unlink("Rplots.pdf")
    }, add = TRUE)
    suppressWarnings(try(
      ggsave(file, plot, width = DEV[1], height = DEV[2], device = dev,
             limitsize = FALSE, bg = pm_bg(FALSE)),
      silent = TRUE))
    file.exists(file) && file.size(file) > 1000
  }
  if (try_dev(grDevices::cairo_pdf)) return(invisible(TRUE))
  if (try_dev(NULL)) {
    message(pdf_hint())
    return(invisible(TRUE))
  }
  warning("could not write ", file)
  invisible(FALSE)
}
save_pdf(pm_path("outputs", "map.pdf"), p)

p_dark <- compose_map(m, car_pt = 40, dev_in = DEV, dark = TRUE)
ggsave(pm_path("outputs", "map-dark.png"), p_dark, width = DEV[1], height = DEV[2], dpi = 200,
       device = pm_png(), limitsize = FALSE, bg = pm_bg(TRUE))

cat("\nWrote outputs/map.png, outputs/map.pdf, outputs/map-dark.png\n")
if (interactive()) print(p)   # <- Plots pane
