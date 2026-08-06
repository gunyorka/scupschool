manifest_path <- "dev/course-manifest.csv"

navigation_end <-
  "<!-- END GENERATED MODULE NAVIGATION -->"

next_link_begin <-
  "<!-- BEGIN GENERATED MILESTONE NEXT LINK -->"

next_link_end <-
  "<!-- END GENERATED MILESTONE NEXT LINK -->"


# Read and validate --------------------------------------------------------

manifest <- read.csv(
  manifest_path,
  fileEncoding = "UTF-8",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

pages <- manifest[
  manifest$page_type == "milestone",
  ,
  drop = FALSE
]

pages <- pages[
  order(pages$module_number, pages$page_order),
]

stopifnot(nrow(pages) == 7)

missing_pages <- pages$relative_path[
  !file.exists(pages$relative_path)
]

if (length(missing_pages) > 0) {
  stop(
    "Missing milestone pages:\n",
    paste(missing_pages, collapse = "\n")
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

split_front_matter <- function(lines, path) {
  delimiters <- which(trimws(lines) == "---")
  
  if (
    length(delimiters) < 2 ||
    delimiters[[1]] != 1
  ) {
    stop(
      "Valid YAML front matter not found in: ",
      path
    )
  }
  
  yaml_end <- delimiters[[2]]
  
  list(
    yaml = lines[2:(yaml_end - 1)],
    
    body = if (yaml_end < length(lines)) {
      lines[(yaml_end + 1):length(lines)]
    } else {
      character()
    }
  )
}

set_yaml_value <- function(
    yaml,
    key,
    value
) {
  pattern <- paste0(
    "^",
    key,
    "\\s*:"
  )
  
  position <- which(
    grepl(pattern, yaml)
  )
  
  if (length(position) > 1) {
    stop(
      "Multiple YAML fields found for: ",
      key
    )
  }
  
  new_line <- paste0(
    key,
    ": ",
    value
  )
  
  if (length(position) == 1) {
    yaml[position] <- new_line
  } else {
    yaml <- c(
      yaml,
      "",
      new_line
    )
  }
  
  yaml
}

remove_yaml_value <- function(
    yaml,
    key
) {
  pattern <- paste0(
    "^",
    key,
    "\\s*:"
  )
  
  yaml[
    !grepl(pattern, yaml)
  ]
}

set_body_width <- function(
    yaml,
    width = "1000px"
) {
  grid_position <- which(
    grepl(
      "^grid\\s*:\\s*$",
      yaml
    )
  )
  
  if (length(grid_position) > 1) {
    stop("Multiple grid blocks found.")
  }
  
  if (length(grid_position) == 0) {
    return(
      c(
        yaml,
        "",
        "grid:",
        paste0(
          "  body-width: ",
          width
        )
      )
    )
  }
  
  grid_start <- grid_position[[1]]
  
  following <- seq.int(
    grid_start + 1,
    length(yaml)
  )
  
  if (length(following) == 0) {
    grid_end <- length(yaml)
  } else {
    next_key <- following[
      grepl(
        "^[A-Za-z0-9_-]+:",
        yaml[following]
      )
    ]
    
    grid_end <- if (length(next_key) > 0) {
      next_key[[1]] - 1
    } else {
      length(yaml)
    }
  }
  
  grid_positions <- seq.int(
    grid_start,
    grid_end
  )
  
  width_position <- grid_positions[
    grepl(
      "^\\s+body-width\\s*:",
      yaml[grid_positions]
    )
  ]
  
  if (length(width_position) > 1) {
    stop("Multiple body-width values found.")
  }
  
  if (length(width_position) == 1) {
    yaml[width_position] <- paste0(
      "  body-width: ",
      width
    )
  } else {
    yaml <- append(
      yaml,
      paste0(
        "  body-width: ",
        width
      ),
      after = grid_start
    )
  }
  
  yaml
}

trim_trailing_blank_lines <- function(lines) {
  while (
    length(lines) > 0 &&
    !nzchar(trimws(lines[[length(lines)]]))
  ) {
    lines <- lines[-length(lines)]
  }
  
  lines
}


# Generated next-module transition ----------------------------------------

next_link_block <- function(
    module_number,
    next_module_folder = NULL
) {
  if (module_number < 7) {
    href <- paste0(
      "../",
      next_module_folder,
      "/index.html"
    )
    
    text <-
      "Nézzük, mi vár ránk a következő modulban"
  } else {
    href <- "../../index.html"
    text <- "Vissza a mérföldkőtérképhez"
  }
  
  c(
    next_link_begin,
    '<div class="milestone-decision__action">',
    paste0(
      '<a class="milestone-next-link" href="',
      href,
      '">'
    ),
    text,
    "</a>",
    "</div>",
    next_link_end
  )
}


# Generic milestone placeholder -------------------------------------------

milestone_placeholder <- function(
    module_number,
    next_module_folder = NULL
) {
  ready_summary <- if (module_number < 7) {
    paste0(
      "A közös döntés alapján továbbléphettek ",
      "a következő modulra."
    )
  } else {
    paste0(
      "A közös döntés alapján lezárhatjátok ",
      "a projektfolyamatot."
    )
  }
  
  ready_intro <- if (module_number < 7) {
    paste0(
      "Mielőtt lezárjátok a meetinget, érdemes még ",
      "néhány gyakorlati lépést megtennetek."
    )
  } else {
    paste0(
      "Mielőtt lezárjátok a meetinget, érdemes még ",
      "néhány utolsó gyakorlati lépést megtennetek."
    )
  }
  
  ready_steps <- if (module_number < 7) {
    c(
      "<ul>",
      paste0(
        "<li>Rögzítsétek a meeting legfontosabb ",
        "döntéseit és eredményeit.</li>"
      ),
      paste0(
        "<li>Jegyezzétek fel azokat a nyitott kérdéseket, ",
        "amelyekkel később még foglalkoznotok kell.</li>"
      ),
      paste0(
        "<li>Egyeztessétek a következő személyes meeting ",
        "várható időpontját.</li>"
      ),
      paste0(
        "<li>Győződjetek meg róla, hogy mindenki tudja, ",
        "melyik modullal folytatódik a közös munka.</li>"
      ),
      "</ul>"
    )
  } else {
    c(
      "<ul>",
      paste0(
        "<li>Rögzítsétek a meeting legfontosabb ",
        "döntéseit és eredményeit.</li>"
      ),
      paste0(
        "<li>Ellenőrizzétek, hogy minden fájl, link és ",
        "hozzáférés megfelelően működik.</li>"
      ),
      paste0(
        "<li>Beszéljétek meg, maradt-e még közös ",
        "lezárási vagy beadási teendőtök.</li>"
      ),
      paste0(
        "<li>Győződjetek meg róla, hogy mindenki érti, ",
        "mi történik a beadás után.</li>"
      ),
      "</ul>"
    )
  }
  
  c(
    paste0(
      "A meeting vége előtt nyissátok meg ezt az oldalt, ",
      "és közösen gondoljátok át, hogy a projektetek ",
      "készen áll-e a következő fázisra."
    ),
    "",
    paste0(
      "Az alábbi szempontok nem kötelező teljesítési ",
      "feltételek. Abban segítenek, hogy a csapat közösen ",
      "felmérje, hol tart most a projekt, és eldöntse, ",
      "érdemes-e továbblépni."
    ),
    "",
    "## Gondoljátok át, hol tartotok",
    "",
    "::: {.milestone-criteria}",
    "",
    "- *[Az első modulspecifikus értékelési szempont helye.]*",
    "- *[A második modulspecifikus értékelési szempont helye.]*",
    "- *[A harmadik modulspecifikus értékelési szempont helye.]*",
    "- *[A negyedik modulspecifikus értékelési szempont helye.]*",
    "",
    ":::",
    "",
    paste0(
      "A döntést ne egyetlen hiányzó részlet alapján ",
      "hozzátok meg. Beszéljétek át, hogy a bizonytalanságok ",
      "akadályozzák-e a továbblépést, vagy később is ",
      "tisztázhatók."
    ),
    "",
    "## Hogyan tovább?",
    "",
    '<div class="milestone-decision-grid">',
    "",
    paste0(
      '<details class="milestone-decision ',
      'milestone-decision--not-ready">'
    ),
    "",
    "<summary>",
    paste0(
      '<span class="milestone-decision__title">',
      "Még nem állunk készen",
      "</span>"
    ),
    paste0(
      '<span class="milestone-decision__summary">',
      "Még van néhány kérdés, amit érdemes közösen tisztáznotok.",
      "</span>"
    ),
    "</summary>",
    "",
    '<div class="milestone-decision__content">',
    "",
    paste0(
      "<p>Azonosítsátok közösen, mi akadályozza ",
      "a továbblépést.</p>"
    ),
    "",
    "<ul>",
    "<li>Melyik szempontban vagytok még bizonytalanok?</li>",
    paste0(
      "<li>Információ, közös döntés vagy pontosabb ",
      "megfogalmazás hiányzik?</li>"
    ),
    paste0(
      "<li>A hiányzó munka elvégezhető külön-külön, ",
      "vagy újabb közös beszélgetésre van szükség?</li>"
    ),
    "<li>Ki mit vállal, és mikorra készül el vele?</li>",
    paste0(
      "<li>Mikor tértek vissza együtt a mérföldkő ",
      "értékeléséhez?</li>"
    ),
    "</ul>",
    "",
    paste0(
      "<p>Nem feltétlenül kell újabb teljes meetinget ",
      "szerveznetek. A lényeg, hogy egyértelmű legyen, ",
      "mi hiányzik és hogyan fogjátok pótolni.</p>"
    ),
    "",
    "</div>",
    "",
    "</details>",
    "",
    paste0(
      '<details class="milestone-decision ',
      'milestone-decision--ready">'
    ),
    "",
    "<summary>",
    paste0(
      '<span class="milestone-decision__title">',
      "Készen állunk a következő modulra",
      "</span>"
    ),
    paste0(
      '<span class="milestone-decision__summary">',
      ready_summary,
      "</span>"
    ),
    "</summary>",
    "",
    '<div class="milestone-decision__content">',
    "",
    paste0(
      "<p>",
      ready_intro,
      "</p>"
    ),
    "",
    ready_steps,
    "",
    next_link_block(
      module_number = module_number,
      next_module_folder = next_module_folder
    ),
    "",
    "</div>",
    "",
    "</details>",
    "",
    "</div>",
    ""
  )
}


# Replace only the original placeholder body -------------------------------

replace_known_placeholder <- function(
    body,
    module_number,
    next_module_folder,
    path
) {
  navigation_position <- which(
    body == navigation_end
  )
  
  if (length(navigation_position) != 1) {
    stop(
      "Generated module navigation not found in: ",
      path
    )
  }
  
  navigation_position <-
    navigation_position[[1]]
  
  content <- if (
    navigation_position < length(body)
  ) {
    body[
      (navigation_position + 1):length(body)
    ]
  } else {
    character()
  }
  
  has_known_placeholder <- any(
    grepl(
      "Ez az oldal a navigációs prototípus része",
      content,
      fixed = TRUE
    )
  )
  
  if (!has_known_placeholder) {
    return(body)
  }
  
  c(
    body[
      seq_len(navigation_position)
    ],
    "",
    milestone_placeholder(
      module_number = module_number,
      next_module_folder = next_module_folder
    )
  )
}


# Update only the generated transition link --------------------------------

set_next_link_block <- function(
    body,
    module_number,
    next_module_folder,
    path
) {
  replacement <- next_link_block(
    module_number = module_number,
    next_module_folder = next_module_folder
  )
  
  begin_position <- which(
    body == next_link_begin
  )
  
  end_position <- which(
    body == next_link_end
  )
  
  if (
    length(begin_position) > 0 ||
    length(end_position) > 0
  ) {
    if (
      length(begin_position) != 1 ||
      length(end_position) != 1 ||
      begin_position >= end_position
    ) {
      stop(
        "Invalid milestone next-link markers in: ",
        path
      )
    }
    
    before <- if (begin_position > 1) {
      body[
        seq_len(begin_position - 1)
      ]
    } else {
      character()
    }
    
    after <- if (end_position < length(body)) {
      body[
        (end_position + 1):length(body)
      ]
    } else {
      character()
    }
    
    return(
      c(
        before,
        replacement,
        after
      )
    )
  }
  
  action_position <- which(
    grepl(
      '<div class="milestone-decision__action">',
      body,
      fixed = TRUE
    )
  )
  
  if (length(action_position) != 1) {
    stop(
      "Milestone transition action not found in: ",
      path
    )
  }
  
  action_start <- action_position[[1]]
  
  closing_candidates <- which(
    seq_along(body) > action_start &
      trimws(body) == "</div>"
  )
  
  if (length(closing_candidates) == 0) {
    stop(
      "Milestone transition action is not closed in: ",
      path
    )
  }
  
  action_end <- closing_candidates[[1]]
  
  before <- if (action_start > 1) {
    body[
      seq_len(action_start - 1)
    ]
  } else {
    character()
  }
  
  after <- if (action_end < length(body)) {
    body[
      (action_end + 1):length(body)
    ]
  } else {
    character()
  }
  
  c(
    before,
    replacement,
    after
  )
}


# Apply the shell ----------------------------------------------------------

updated_pages <- character()
generated_pages <- character()
preserved_pages <- character()

for (i in seq_len(nrow(pages))) {
  row <- pages[
    i,
    ,
    drop = FALSE
  ]
  
  module_number <-
    row$module_number[[1]]
  
  path <- row$relative_path[[1]]
  
  next_module_folder <- if (module_number < 7) {
    next_overview <- manifest[
      manifest$module_number == module_number + 1 &
        manifest$page_type == "overview",
      ,
      drop = FALSE
    ]
    
    stopifnot(nrow(next_overview) == 1)
    
    next_overview$module_folder[[1]]
  } else {
    NULL
  }
  
  lines <- read_utf8(path)
  
  parts <- split_front_matter(
    lines,
    path
  )
  
  yaml <- remove_yaml_value(
    parts$yaml,
    key = "subtitle"
  )
  
  yaml <- set_body_width(
    yaml,
    width = "1000px"
  )
  
  yaml <- set_yaml_value(
    yaml,
    key = "body-classes",
    value = "module-content-page milestone-page"
  )
  
  had_placeholder <- any(
    grepl(
      "Ez az oldal a navigációs prototípus része",
      parts$body,
      fixed = TRUE
    )
  )
  
  body <- replace_known_placeholder(
    body = parts$body,
    module_number = module_number,
    next_module_folder = next_module_folder,
    path = path
  )
  
  body <- set_next_link_block(
    body = body,
    module_number = module_number,
    next_module_folder = next_module_folder,
    path = path
  )
  
  write_utf8(
    c(
      "---",
      yaml,
      "---",
      body
    ),
    path
  )
  
  updated_pages <- c(
    updated_pages,
    path
  )
  
  if (had_placeholder) {
    generated_pages <- c(
      generated_pages,
      path
    )
  } else {
    preserved_pages <- c(
      preserved_pages,
      path
    )
  }
}


# Validation ---------------------------------------------------------------

for (path in updated_pages) {
  lines <- read_utf8(path)
  
  parts <- split_front_matter(
    lines,
    path
  )
  
  stopifnot(
    !any(
      grepl(
        "^subtitle\\s*:",
        parts$yaml
      )
    ),
    
    any(
      grepl(
        "^\\s+body-width\\s*:\\s*1000px\\s*$",
        parts$yaml
      )
    ),
    
    any(
      grepl(
        paste0(
          "^body-classes\\s*:\\s*",
          "module-content-page milestone-page\\s*$"
        ),
        parts$yaml
      )
    ),
    
    sum(parts$body == next_link_begin) == 1,
    sum(parts$body == next_link_end) == 1,
    
    any(
      trimws(parts$body) ==
        "## Gondoljátok át, hol tartotok"
    ),
    
    any(
      trimws(parts$body) ==
        "## Hogyan tovább?"
    ),
    
    !any(
      grepl(
        "Ez az oldal a navigációs prototípus része",
        parts$body,
        fixed = TRUE
      )
    )
  )
}

message(
  "Milestone shell applied successfully to ",
  length(updated_pages),
  " pages."
)

message(
  "Generated placeholder structure on ",
  length(generated_pages),
  " pages."
)

message(
  "Preserved existing authored content on ",
  length(preserved_pages),
  " pages."
)