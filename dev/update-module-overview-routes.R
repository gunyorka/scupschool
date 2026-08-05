manifest_path <- "dev/course-manifest.csv"

route_begin <- "<!-- BEGIN GENERATED MODULE ROUTE -->"
route_end   <- "<!-- END GENERATED MODULE ROUTE -->"

manifest <- read.csv(
  manifest_path,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

manifest <- manifest[
  order(manifest$module_number, manifest$page_order),
]

if (anyDuplicated(manifest$relative_path)) {
  stop("Duplicate page paths found in the course manifest.")
}

overview_rows <- manifest[
  manifest$page_type == "overview",
  ,
  drop = FALSE
]

missing_overviews <- overview_rows$relative_path[
  !file.exists(overview_rows$relative_path)
]

if (length(missing_overviews) > 0) {
  stop(
    "Missing module overview pages:\n",
    paste(missing_overviews, collapse = "\n")
  )
}


# Helpers -----------------------------------------------------------------

read_utf8 <- function(path) {
  readLines(
    path,
    warn = FALSE,
    encoding = "UTF-8"
  )
}

write_utf8 <- function(lines, path) {
  writeLines(
    lines,
    con = path,
    useBytes = TRUE
  )
}

escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

qmd_to_html <- function(path) {
  sub("\\.qmd$", ".html", path)
}

build_route <- function(module_rows) {
  route_rows <- module_rows[
    module_rows$page_type != "overview",
    ,
    drop = FALSE
  ]
  
  route_rows <- route_rows[
    order(route_rows$page_order),
  ]
  
  cards <- vapply(
    seq_len(nrow(route_rows)),
    function(i) {
      row <- route_rows[i, , drop = FALSE]
      
      paste0(
        '<a class="module-route-card" href="',
        escape_html(qmd_to_html(row$file_name[[1]])),
        '">',
        '<span class="module-route-card__title">',
        escape_html(row$page_title[[1]]),
        "</span>",
        "</a>"
      )
    },
    character(1)
  )
  
  c(
    "```{=html}",
    '<div class="module-route-list">',
    cards,
    "</div>",
    "```"
  )
}

replace_route_block <- function(
    lines,
    replacement,
    path
) {
  begin_position <- which(lines == route_begin)
  end_position <- which(lines == route_end)
  
  has_begin <- length(begin_position) > 0
  has_end <- length(end_position) > 0
  
  if (has_begin || has_end) {
    if (
      length(begin_position) != 1 ||
      length(end_position) != 1 ||
      begin_position >= end_position
    ) {
      stop(
        "Invalid generated route markers in: ",
        path
      )
    }
    
    before <- if (begin_position > 1) {
      lines[seq_len(begin_position - 1)]
    } else {
      character()
    }
    
    after <- if (end_position < length(lines)) {
      lines[(end_position + 1):length(lines)]
    } else {
      character()
    }
    
    return(
      c(
        before,
        route_begin,
        replacement,
        route_end,
        after
      )
    )
  }
  
  heading_position <- which(
    trimws(lines) == "## A modul oldalai"
  )
  
  if (length(heading_position) != 1) {
    stop(
      "Expected exactly one '## A modul oldalai' heading in: ",
      path
    )
  }
  
  c(
    lines[seq_len(heading_position)],
    "",
    route_begin,
    replacement,
    route_end,
    ""
  )
}


# Update overview pages ----------------------------------------------------

updated_pages <- character()

for (
  module_number in
  sort(unique(manifest$module_number))
) {
  module_rows <- manifest[
    manifest$module_number == module_number,
    ,
    drop = FALSE
  ]
  
  overview <- module_rows[
    module_rows$page_type == "overview",
    ,
    drop = FALSE
  ]
  
  stopifnot(nrow(overview) == 1)
  
  path <- overview$relative_path[[1]]
  
  lines <- read_utf8(path)
  
  updated <- replace_route_block(
    lines = lines,
    replacement = build_route(module_rows),
    path = path
  )
  
  write_utf8(
    updated,
    path
  )
  
  updated_pages <- c(
    updated_pages,
    path
  )
}


# Validation ---------------------------------------------------------------

for (path in updated_pages) {
  lines <- read_utf8(path)
  
  if (
    sum(lines == route_begin) != 1 ||
    sum(lines == route_end) != 1
  ) {
    stop(
      "Route marker validation failed in: ",
      path
    )
  }
}

message(
  "Module overview routes updated successfully on ",
  length(updated_pages),
  " pages."
)