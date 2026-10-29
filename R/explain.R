# ---------------------------------------------------------------------------
# The "How it works" tab.
#
# Numbers and the attribute table are derived from CARS / ATTRS at build time,
# so the prose cannot drift out of sync with the data or the transforms.
# Styling uses Bootstrap 5.3 semantic classes only (text-body-secondary,
# bg-body-tertiary, border, table) so the whole tab follows the dark-mode
# toggle without a second palette.
# ---------------------------------------------------------------------------

xp_css <- htmltools::HTML("
.xp { max-width: 980px; }
.xp h2 { font-size: 1.05rem; font-weight: 700; margin: 0; }
.xp p  { margin-bottom: .6rem; line-height: 1.6; }
.xp .lead-in { font-size: 1.05rem; line-height: 1.65; }
.xp code { font-size: .86em; }
.xp .num {
  display: inline-flex; align-items: center; justify-content: center;
  width: 1.6rem; height: 1.6rem; flex: 0 0 1.6rem; border-radius: 50%;
  background: var(--bs-primary); color: #fff; font-size: .8rem; font-weight: 700;
}
.xp .note {
  border-left: 3px solid var(--bs-primary); padding: .5rem .75rem;
  background: var(--bs-body-bg); border-radius: .25rem; font-size: .87rem;
}
.xp .note .h { font-weight: 650; }
.xp table { font-size: .85rem; }
.xp .mono { font-family: var(--bs-font-monospace); font-size: .82rem; }
.xp .compass {
  display: grid; gap: .6rem; grid-template-columns: repeat(3, minmax(0, 1fr));
  grid-template-areas: '. up .' 'left mid right' '. down .';
}
.xp .compass .up { grid-area: up; }     .xp .compass .down  { grid-area: down; }
.xp .compass .left { grid-area: left; } .xp .compass .right { grid-area: right; }
.xp .compass .mid {
  grid-area: mid; display: flex; flex-direction: column; justify-content: center;
  align-items: center; text-align: center; font-size: .78rem; color: var(--bs-secondary-color);
}
.xp .dir { border: 1px solid var(--bs-border-color); border-radius: .4rem;
  padding: .55rem .7rem; background: var(--bs-body-bg); }
.xp .dir .k { font-size: .7rem; font-weight: 700; letter-spacing: .05em;
  text-transform: uppercase; color: var(--bs-secondary-color); }
.xp .dir .w { font-weight: 700; font-size: .95rem; margin: .15rem 0 .3rem; }
.xp .dir .r { font-family: var(--bs-font-monospace); font-size: .72rem; color: var(--bs-secondary-color); }
.xp .dir .c { font-size: .8rem; margin-top: .35rem; }
.xp .quad td { width: 42%; vertical-align: top; }
.xp .pcs td, .xp .pcs th { vertical-align: top; }
.xp .pcs tr.faint td { color: var(--bs-secondary-color); }
.xp .pcs .onscreen { font-size: .66rem; font-weight: 700; margin-left: .35rem; }
.xp .pcs .bar { height: .35rem; border-radius: .2rem; background: var(--bs-primary); opacity: .75; }
.xp .pcs td:nth-child(2) { min-width: 7rem; }
.xp .quad th { font-size: .8rem; }
@media (max-width: 800px) {
  .xp .compass { grid-template-columns: 1fr; grid-template-areas: 'up' 'left' 'mid' 'right' 'down'; }
}
")

xp_sec <- function(n, title, ...) {
  bslib::card(
    class = "mb-3",
    bslib::card_header(
      tags$div(class = "d-flex align-items-center gap-2",
               tags$span(class = "num", n), tags$h2(title))
    ),
    bslib::card_body(...)
  )
}

xp_note <- function(head, ...) {
  tags$div(class = "note my-2", tags$span(class = "h", head), " ", ...)
}

#' The attribute table, generated from ATTRS so it always matches the engine.
xp_attr_table <- function(attrs) {
  why <- c(
    price_k = "Sticker price, $000s.",
    hp      = "Peak engine or motor output.",
    mpg     = "Logged first. EV MPGe (122-132) would otherwise dwarf petrol MPG (18-36) and collapse this into an \"is it an EV\" flag.",
    weight  = "Kerb weight, lb.",
    seats   = "Belted seating positions.",
    sec060  = "Negated, so a larger number means quicker and the arrow points the intuitive way.",
    cargo   = "Boot or bed volume, cu ft.",
    length  = "Overall length, inches."
  )
  tf <- c(none = "as measured", log = "log10", negate = "sign flipped")
  tags$div(class = "table-responsive",
    tags$table(class = "table table-sm align-middle mb-0",
      tags$thead(tags$tr(
        tags$th("Shown as"), tags$th("Column"), tags$th("Transform"), tags$th("Notes"))),
      tags$tbody(lapply(seq_len(nrow(attrs)), function(i) {
        v <- attrs$var[i]
        tags$tr(
          tags$td(tags$span(class = "fw-semibold", attrs$label[i])),
          tags$td(tags$code(v)),
          tags$td(if (attrs$transform[i] == "none")
                    tags$span(class = "text-body-secondary", tf[["none"]])
                  else tags$span(class = "badge text-bg-warning", tf[[attrs$transform[i]]])),
          tags$td(class = "text-body-secondary small", why[[v]] %||% "")
        )
      }))
    )
  )
}

`%||%` <- function(a, b) if (is.null(a)) b else a

#' The table of every component for the current fit, plus the notes that
#' depend on it. A plain function so it can be checked from a script.
pcs_explainer <- function(m, xpc, ypc, thresh = 0.45, max_axis = 4) {
  L <- m$loadings; k <- m$npc; pcs <- paste0("PC", seq_len(k))
  cum <- cumsum(m$ve)

  row <- function(i) {
    pc <- pcs[i]; r <- L[[pc]]; o <- order(abs(r), decreasing = TRUE)[seq_len(min(3, nrow(L)))]
    th <- axis_theme(L, pc, thresh)
    e  <- axis_ends(L, pc, thresh = thresh)
    badge <- c(if (pc == xpc) "across", if (pc == ypc) "up")
    tags$tr(class = if (is.na(th)) "faint",
      tags$td(tags$strong(pc),
              if (length(badge)) tags$span(class = "badge text-bg-primary onscreen",
                                           paste("on map:", paste(badge, collapse = " & ")))),
      tags$td(sprintf("%.1f%%", m$ve[i]),
              tags$div(class = "bar mt-1", style = sprintf("width: %.0f%%;", m$ve[i]))),
      tags$td(sprintf("%.1f%%", cum[i])),
      tags$td(if (is.na(th)) tags$em("no clear pattern") else tags$strong(th)),
      tags$td(if (is.na(th)) "\u2014" else paste(e$pos, collapse = ", ")),
      tags$td(class = "mono", paste(sprintf("%s %+.2f", L$var[o], r[o]), collapse = ", ")))
  }
  tbl <- tags$div(class = "table-responsive",
    tags$table(class = "table table-sm pcs mb-2",
      tags$thead(tags$tr(tags$th("Component"), tags$th("Share"), tags$th("Running total"),
                         tags$th("Mostly"), tags$th("Higher score means"), tags$th("Strongest specs (r)"))),
      tags$tbody(lapply(seq_len(k), row))))

  # Attributes the default pair shows poorly, and where they went instead.
  first2 <- intersect(c("PC1", "PC2"), pcs)
  hidden <- lapply(seq_len(nrow(L)), function(j) {
    if (max(abs(unlist(L[j, first2]))) >= thresh) return(NULL)
    rr <- abs(unlist(L[j, pcs])); best <- which.max(rr)
    if (rr[best] < thresh) return(NULL)
    tags$li(tags$strong(L$var[j]), sprintf(" barely registers on PC1 or PC2, but lines up with %s (r %.2f). ",
                                           pcs[best], rr[best]),
            if (best <= max_axis) sprintf("Set an axis to %s to see it.", pcs[best])
            else sprintf("%s is beyond the axis dropdowns, so this map cannot show it.", pcs[best]))
  })
  hidden <- Filter(Negate(is.null), hidden)
  tail_share <- if (k > max_axis) sum(m$ve[(max_axis + 1):k]) else 0

  tagList(
    tbl,
    tags$p(class = "small text-body-secondary mb-2",
           "r is the correlation between a spec and the component's score: + means the spec rises as ",
           "the score rises, \u2212 means it falls. Greyed-out rows have no spec at 0.45 or above."),
    tags$ul(class = "mb-0",
      if (k >= 2) tags$li(sprintf(paste0("The map opens on PC1 against PC2 because together they carry %.1f%% ",
                                         "of the differences between cars \u2014 more than any other pair."), cum[2])),
      hidden,
      if (tail_share > 0) tags$li(sprintf(paste0("The axis dropdowns stop at PC%d. Everything after it carries ",
                                                 "%.1f%% between them, so plotting it would mostly show noise."),
                                          max_axis, tail_share)),
      tags$li("Untick specs in the sidebar and PCA reruns: the components can change meaning, ",
              "and this table changes with them."))
  )
}

#' A plain-English line for each component above the loadings table, so the
#' columns are not just PC1..PC4. Same axis_theme()/axis_ends() as the map and
#' the How it works tab, so the three cannot drift apart.
loadings_key <- function(m, xpc, ypc, max_pc = 4, thresh = 0.45) {
  pcs <- intersect(paste0("PC", seq_len(max_pc)), names(m$loadings))
  if (!length(pcs)) return(NULL)
  item <- function(pc) {
    ve <- m$ve[as.integer(sub("PC", "", pc))]
    th <- axis_theme(m$loadings, pc, thresh)
    e  <- axis_ends(m$loadings, pc, thresh = thresh)
    on <- c(if (pc == xpc) "across", if (pc == ypc) "up")
    tags$div(class = "border rounded p-2 flex-fill", style = "min-width: 11rem;",
      tags$div(class = "d-flex align-items-baseline gap-2 flex-wrap",
        tags$strong(pc),
        tags$span(class = "text-body-secondary small", sprintf("%.1f%% of the variance", ve)),
        if (length(on)) tags$span(class = "badge text-bg-primary",
                                  paste("on map:", paste(on, collapse = " & ")))),
      tags$div(class = "fw-semibold mt-1",
               if (is.na(th)) tags$em("no clear pattern") else th),
      tags$div(class = "small text-body-secondary",
               if (e$weak) "no spec reaches 0.45" else paste("higher =", paste(e$pos, collapse = ", "))))
  }
  tagList(
    tags$div(class = "d-flex flex-wrap gap-2 mb-3", lapply(pcs, item)),
    tags$p(class = "small text-body-secondary mb-2",
           "Each number is the correlation between a spec and that component: ",
           "+1 means they rise together, \u22121 means one rises as the other falls, 0 means unrelated. ",
           "Cells are shaded past \u00b10.5 \u2014 blue for a strong positive, red for a strong negative.")
  )
}

#' The Loadings tab: what each component is, how each spec loads on it, and
#' where each car sits.
#'
#' Plain HTML tables rather than DT: this copy has no DT dependency, the tables
#' are small, and Bootstrap classes follow the dark-mode toggle on their own.
#'
#' The two halves are NOT on the same scale and the note says so -- attribute
#' values are correlations capped at 1, car values are scores in standard
#' deviations -- which is the same caveat the map's arrows carry.
loadings_panel <- function(m, d, xpc, ypc, max_pc = 4, thresh = 0.5) {
  pcs <- intersect(paste0("PC", seq_len(max_pc)), names(m$loadings))
  num <- function(x) sprintf("%.2f", x)
  cell <- function(v, shade) {
    cls <- "text-end"
    if (shade && is.finite(v) && abs(v) >= thresh) cls <- paste(cls, if (v > 0) "pm-pos" else "pm-neg")
    tags$td(class = cls, num(v))
  }
  tbl <- function(rows, head1, first, shade) tags$div(
    class = "table-responsive", style = "max-height: 26rem; overflow-y: auto;",
    tags$table(class = "table table-sm align-middle mb-0",
      tags$thead(class = "position-sticky top-0",
        tags$tr(tags$th(head1), lapply(pcs, function(p) tags$th(class = "text-end", p)))),
      tags$tbody(lapply(seq_len(nrow(rows)), function(i)
        tags$tr(tags$td(first(rows, i)), lapply(pcs, function(p) cell(rows[[p]][i], shade)))))))

  L <- m$loadings
  cars <- if (nrow(d)) d[order(-d[[xpc]]), , drop = FALSE] else d

  tagList(
    # Bootstrap paints every cell via ".table > :not(caption) > * > *", which
    # outranks a bare .pm-pos, so the tint is set through its own variable.
    # Hues match the map; as tints they read on either theme.
    tags$style(HTML(paste0(
      ".pm-load td.pm-pos { --bs-table-bg: rgba(76,114,176,.22); }",
      ".pm-load td.pm-neg { --bs-table-bg: rgba(196,78,82,.20); }",
      ".pm-load thead th { background: var(--bs-body-bg); }"))),
    loadings_key(m, xpc, ypc, max_pc = max_pc),

    tags$h6(class = "fw-bold mt-3 mb-2", "Attributes: how each spec lines up with each component"),
    tags$div(class = "pm-load", tbl(L, "attribute", function(r, i) r$var[i], TRUE)),
    tags$h6(class = "fw-bold mt-4 mb-2",
            sprintf("Cars: where each one sits, strongest %s first", xpc)),
    if (!nrow(cars)) tags$p(class = "text-body-secondary", "No cars match the current body-type filter.")
    else tags$div(class = "pm-load",
      tbl(cars, "model", function(r, i) tagList(r$model[i],
            tags$span(class = "text-body-secondary small ms-2", r$body[i])), FALSE)),
    tags$p(class = "small text-body-secondary mt-2 mb-0",
           "Scores in standard deviations from the average car, not correlations, so these ",
           "are not comparable with the numbers above. 0 is the crosshair on the map; ",
           sprintf("a car at +2 on %s is two standard deviations along it.", xpc))
  )
}

#' Build the whole tab.
explain_ui <- function(cars, attrs) {
  n_car   <- nrow(cars)
  n_brand <- length(unique(cars$brand))
  n_body  <- length(unique(cars$body))
  n_attr  <- nrow(attrs)

  tags$div(class = "xp",
    tags$style(xp_css),

    tags$p(class = "lead-in",
      "A perceptual map answers one question: ",
      tags$strong("which cars does the market treat as substitutes for each other?"),
      " Two cars sit close together here because their ", n_attr, " specifications are similar overall ",
      "\u2014 not because of any single number. Everything below is computed live from ",
      tags$code("data/cars.csv"), "; nothing is hard-coded."),

    xp_sec(1, "The data",
      tags$p(n_car, " current models from ", n_brand, " brands, across ", n_body,
             " body types, each measured on ", n_attr, " attributes."),
      xp_attr_table(attrs),
      xp_note("Hand-compiled.",
        "These are approximate real-world figures assembled for illustration, not a licensed ",
        "manufacturer dataset. Swap in your own rows and everything downstream adapts \u2014 ",
        "the only requirement is the same column names.")
    ),

    xp_sec(2, "Putting the attributes on a level playing field",
      tags$p("Price runs into the tens of thousands, seat count runs from 2 to 8. Fed in raw, ",
             "price would decide the entire map purely because its numbers are bigger. So each ",
             "column is converted to a ", tags$strong("z-score"), " \u2014 subtract the mean, divide by ",
             "the standard deviation \u2014 via ", tags$code("prcomp(scale. = TRUE)"),
             ". Afterwards every attribute has mean 0 and SD 1, and contributes on equal terms."),
      tags$p("Two columns are adjusted first, for the reasons in the table above: ",
             tags$code("mpg"), " is logged, and ", tags$code("sec060"),
             " is negated so that the axis reads as ", tags$em("quickness"),
             " rather than ", tags$em("seconds taken"), "."),
      xp_note("Edge case.",
        "If a filter leaves a column with zero variance it is dropped before fitting \u2014 ",
        "a constant column has no standard deviation to divide by and would make ",
        tags$code("prcomp"), " fail.")
    ),

    xp_sec(3, "What PC1, PC2 and the rest are",
      tags$p(tags$strong("PC stands for principal component."), " Each one is a new axis that ",
             "PCA builds by blending all the selected specs into a single score per car. They are ",
             "numbered by how much of the difference between cars they capture \u2014 PC1 the most, ",
             "PC2 the next most \u2014 and none of them overlap, so each picks up something the earlier ",
             "ones missed."),
      tags$p("With ", n_attr, " attributes each car is a point in ", n_attr,
             "-dimensional space. Principal Component Analysis finds the direction through that ",
             "cloud along which the cars are most spread out \u2014 that becomes ", tags$strong("PC1"),
             ". It then finds the direction of greatest remaining spread that is at right angles ",
             "to the first \u2014 ", tags$strong("PC2"), " \u2014 and so on."),
      tags$p("Because each component is perpendicular to the others, they carry non-overlapping ",
             "information. The percentages in the boxes at the top are each component's share of ",
             "total variance: ", tags$span(class = "mono", "100 \u00d7 sdev\u00b2 / \u03a3 sdev\u00b2"),
             ". The ", tags$strong("Variance"), " tab plots all of them."),
      tags$h6(class = "mt-3 mb-2 fw-bold", "Every component, for the specs you have ticked"),
      uiOutput("xp_pcs"),
      xp_note("On the map.",
        "The key under the map repeats this for the two axes on screen: what PC means, what the ",
        "percentage is, and what each axis mostly measures.")
    )
  )
}
