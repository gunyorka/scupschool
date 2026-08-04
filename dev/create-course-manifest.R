# Scup School canonical course manifest
# Run from the RStudio project root:
#   source("dev/create-course-manifest.R")

make_module <- function(module_number, module_title, module_folder, pages) {
  stopifnot(all(c("page_type", "page_title", "slug") %in% names(pages)))

  page_order <- seq.int(0L, nrow(pages) - 1L)
  file_name <- ifelse(
    pages$page_type == "overview",
    "index.qmd",
    sprintf("%02d-%s.qmd", page_order, pages$slug)
  )

  data.frame(
    module_number = module_number,
    module_title = module_title,
    module_folder = module_folder,
    page_order = page_order,
    page_id = sprintf("m%02d-p%02d", module_number, page_order),
    page_type = pages$page_type,
    page_title = pages$page_title,
    file_name = file_name,
    relative_path = paste("modules", module_folder, file_name, sep = "/"),
    content_status = "placeholder",
    notes = "",
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

module_1 <- make_module(
  module_number = 1L,
  module_title = "Témaválasztás és csapatindítás",
  module_folder = "01-temavalasztas-es-csapatinditas",
  pages = data.frame(
    page_type = c(
      "overview",
      "core",
      "core",
      "core",
      "core",
      "core",
      "readiness",
      "meeting_pack",
      "milestone"
    ),
    page_title = c(
      "Kezdődik a projektetek",
      "Hogyan épül fel a Scup School?",
      "Hogyan tartsátok a kapcsolatot?",
      "Közös workspace és dokumentáció",
      "Melyik booklettémát választanád?",
      "Hogyan hozzátok meg az első közös döntést?",
      "Készen állsz az első meetingre?",
      "Meeting pack",
      "Az első milestone teljesítve"
    ),
    slug = c(
      "index",
      "hogyan-epul-fel-a-scup-school",
      "hogyan-tartsatok-a-kapcsolatot",
      "kozos-workspace-es-dokumentacio",
      "melyik-booklettemat-valasztanad",
      "hogyan-hozzatok-meg-az-elso-kozos-dontest",
      "meeting-readiness",
      "meeting-pack",
      "milestone"
    ),
    stringsAsFactors = FALSE
  )
)

module_2 <- make_module(
  module_number = 2L,
  module_title = "Problémaazonosítás",
  module_folder = "02-problemaazonositas",
  pages = data.frame(
    page_type = c(
      "overview",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "readiness",
      "meeting_pack",
      "milestone"
    ),
    page_title = c(
      "A nagy témától egy konkrét problémáig",
      "Mire való a booklet?",
      "Hogyan dolgozd fel a bookletet?",
      "Mit értünk probléma alatt?",
      "Hogyan írj használható problémamondatot?",
      "Mely problémákat vidd a meetingre?",
      "Egymás gondolataiból építkezni",
      "Hogyan értékeljetek egy problémát?",
      "Készen állsz a második meetingre?",
      "Meeting pack",
      "A második milestone teljesítve"
    ),
    slug = c(
      "index",
      "mire-valo-a-booklet",
      "hogyan-dolgozd-fel-a-bookletet",
      "mit-ertunk-problema-alatt",
      "hogyan-irj-hasznalhato-problemamondatot",
      "mely-problemakat-vidd-a-meetingre",
      "egymas-gondolataibol-epitkezni",
      "hogyan-ertekeljetek-egy-problemat",
      "meeting-readiness",
      "meeting-pack",
      "milestone"
    ),
    stringsAsFactors = FALSE
  )
)

module_3 <- make_module(
  module_number = 3L,
  module_title = "Projektkoncepció",
  module_folder = "03-projektkoncepcio",
  pages = data.frame(
    page_type = c(
      "overview",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "readiness",
      "meeting_pack",
      "milestone"
    ),
    page_title = c(
      "Egy problémából több projekt",
      "A kommunikációs iránytól a projektkoncepcióig",
      "Hat projektalkotási lencse",
      "Edukációs kampány",
      "Oktatási innováció",
      "Gerillakampány",
      "Online kommunikációs kampány",
      "Offline kommunikációs kampány",
      "Kulturális és művészeti tevékenységek",
      "Készíts két projektkoncepció-magot",
      "Hogyan válasszatok sok jó ötlet közül?",
      "Készen állsz a harmadik meetingre?",
      "Meeting pack",
      "Megszületett a projektkoncepciótok"
    ),
    slug = c(
      "index",
      "a-kommunikacios-iranytol-a-projektkoncepcioig",
      "hat-projektalkotasi-lencse",
      "edukacios-kampany",
      "oktatasi-innovacio",
      "gerillakampany",
      "online-kommunikacios-kampany",
      "offline-kommunikacios-kampany",
      "kulturalis-es-muveszeti-tevekenysegek",
      "keszits-ket-projektkoncepcio-magot",
      "hogyan-valasszatok-sok-jo-otlet-kozul",
      "meeting-readiness",
      "meeting-pack",
      "milestone"
    ),
    stringsAsFactors = FALSE
  )
)

module_4 <- make_module(
  module_number = 4L,
  module_title = "Projekttervezés I.",
  module_folder = "04-projekttervezes-1",
  pages = data.frame(
    page_type = c(
      "overview",
      "core",
      "core",
      "core",
      "core",
      "core",
      "readiness",
      "meeting_pack",
      "milestone"
    ),
    page_title = c(
      "A projektötlettől a megvalósítás felé",
      "Először a projektet tervezzétek meg",
      "Nézzétek meg a projektet egy résztvevő szemével",
      "Mi történik a résztvevővel a projekt során?",
      "Írd meg a résztvevő történetét",
      "Hol vannak még hiányok a projektben?",
      "Készen állsz a negyedik meetingre?",
      "Meeting pack",
      "Elindult a projekt részletes kidolgozása"
    ),
    slug = c(
      "index",
      "eloszor-a-projektet-tervezzetek-meg",
      "nezzetek-meg-a-projektet-egy-resztvevo-szemevel",
      "mi-tortenik-a-resztvevovel-a-projekt-soran",
      "ird-meg-a-resztvevo-tortenetet",
      "hol-vannak-meg-hianyok-a-projektben",
      "meeting-readiness",
      "meeting-pack",
      "milestone"
    ),
    stringsAsFactors = FALSE
  )
)

module_5 <- make_module(
  module_number = 5L,
  module_title = "Projekttervezés II. és pályázatírás",
  module_folder = "05-projekttervezes-2-es-palyazatiras",
  pages = data.frame(
    page_type = c(
      "overview",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "readiness",
      "meeting_pack",
      "milestone"
    ),
    page_title = c(
      "A projekttervtől a pályázati anyagig",
      "A pályázati adatlap eszköz, nem cél",
      "Először vázlat, utána végleges szöveg",
      "A probléma és a tudományos háttér bemutatása",
      "Foglaljátok össze röviden a projektet",
      "Tevékenység, eredmény és hatás",
      "Készítsetek reális ütemtervet",
      "Honnan fogjátok tudni, hogy működött?",
      "Mire és kikre lesz szükségetek?",
      "Fenntarthatóság és skálázhatóság",
      "Hogyan írjatok világos pályázati szöveget?",
      "Adjatok nevet a projektnek",
      "Alakítsátok ki a projekt kisarculatát",
      "Tervezzétek meg a logót",
      "Készítsétek el a grafikus onepagert",
      "Egyszerű grafikai tervezés Canvában",
      "Készen álltok az ötödik meetingre?",
      "Meeting pack",
      "Elkészült a pályázati anyagotok"
    ),
    slug = c(
      "index",
      "a-palyazati-adatlap-eszkoz-nem-cel",
      "eloszor-vazlat-utana-vegleges-szoveg",
      "a-problema-es-a-tudomanyos-hatter-bemutatasa",
      "foglaljatok-ossze-roviden-a-projektet",
      "tevekenyseg-eredmeny-es-hatas",
      "keszitsetek-realis-utemtervet",
      "honnan-fogjatok-tudni-hogy-mukodott",
      "mire-es-kikre-lesz-szuksegetek",
      "fenntarthatosag-es-skalazhatosag",
      "hogyan-irjatok-vilagos-palyazati-szoveget",
      "adjatok-nevet-a-projektnek",
      "alakitsatok-ki-a-projekt-kisarculatat",
      "tervezzetek-meg-a-logot",
      "keszitsetek-el-a-grafikus-onepagert",
      "egyszeru-grafikai-tervezes-canvaban",
      "meeting-readiness",
      "meeting-pack",
      "milestone"
    ),
    stringsAsFactors = FALSE
  )
)

module_6 <- make_module(
  module_number = 6L,
  module_title = "Projektkommunikáció és pitch",
  module_folder = "06-projektkommunikacio-es-pitch",
  pages = data.frame(
    page_type = c(
      "overview",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "core",
      "readiness",
      "meeting_pack",
      "milestone"
    ),
    page_title = c(
      "Mutassátok be a projekteteket",
      "Mitől működik egy jó pitch?",
      "A háromperces pitch felépítése",
      "Story first: először a történetet találjátok meg",
      "Készíts 5–6 pontos pitchvázlatot",
      "Hogyan alkossatok közös történetet?",
      "Ki és hogyan adja elő?",
      "Hogyan gyakoroljatok és adjatok visszajelzést?",
      "Telefonos videófelvétel és YouTube-feltöltés",
      "Készen álltok a hatodik meetingre?",
      "Meeting pack",
      "Elkészült a végleges pitchetek"
    ),
    slug = c(
      "index",
      "mitol-mukodik-egy-jo-pitch",
      "a-haromperces-pitch-felepitese",
      "story-first-eloszor-a-tortenetet-talaljatok-meg",
      "keszits-5-6-pontos-pitchvazlatot",
      "hogyan-alkossatok-kozos-tortenetet",
      "ki-es-hogyan-adja-elo",
      "hogyan-gyakoroljatok-es-adjatok-visszajelzest",
      "telefonos-videofelvetel-es-youtube-feltoltes",
      "meeting-readiness",
      "meeting-pack",
      "milestone"
    ),
    stringsAsFactors = FALSE
  )
)

module_7 <- make_module(
  module_number = 7L,
  module_title = "Beadás és lezárás",
  module_folder = "07-beadas-es-lezaras",
  pages = data.frame(
    page_type = c(
      "overview",
      "core",
      "core",
      "core",
      "core",
      "core",
      "readiness",
      "meeting_pack",
      "milestone"
    ),
    page_title = c(
      "Már csak a beadás van hátra",
      "Mit kell beadnotok?",
      "Egységes-e minden pályázati anyagotok?",
      "Végső tartalmi ellenőrzés",
      "Fájlok, linkek és hozzáférések ellenőrzése",
      "A pályázati felület használata",
      "Készen álltok a beadásra?",
      "Meeting pack",
      "Beadtátok a pályázatot"
    ),
    slug = c(
      "index",
      "mit-kell-beadnotok",
      "egyseges-e-minden-palyazati-anyagotok",
      "vegso-tartalmi-ellenorzes",
      "fajlok-linkek-es-hozzaferesek-ellenorzese",
      "a-palyazati-felulet-hasznalata",
      "submission-readiness",
      "meeting-pack",
      "milestone"
    ),
    stringsAsFactors = FALSE
  )
)

course_manifest <- do.call(
  rbind,
  list(module_1, module_2, module_3, module_4, module_5, module_6, module_7)
)
row.names(course_manifest) <- NULL

# Guardrails: stop immediately if the canonical structure changes unexpectedly.
stopifnot(nrow(course_manifest) == 83L)
stopifnot(!anyDuplicated(course_manifest$page_id))
stopifnot(!anyDuplicated(course_manifest$relative_path))
stopifnot(identical(
  as.integer(table(factor(
    course_manifest$page_type,
    levels = c("overview", "core", "readiness", "meeting_pack", "milestone")
  ))),
  c(7L, 55L, 7L, 7L, 7L)
))

dir.create("dev", showWarnings = FALSE, recursive = TRUE)
manifest_path <- file.path("dev", "course-manifest.csv")

write.csv(
  course_manifest,
  file = manifest_path,
  row.names = FALSE,
  fileEncoding = "UTF-8",
  na = ""
)

tree_path <- file.path("dev", "target-directory-tree.txt")
tree_lines <- "modules/"
module_numbers <- unique(course_manifest$module_number)

for (module_index in seq_along(module_numbers)) {
  current_module <- module_numbers[[module_index]]
  module_rows <- course_manifest[course_manifest$module_number == current_module, ]
  is_last_module <- module_index == length(module_numbers)
  module_branch <- if (is_last_module) "└── " else "├── "
  child_indent <- if (is_last_module) "    " else "│   "

  tree_lines <- c(
    tree_lines,
    paste0(module_branch, module_rows$module_folder[[1]], "/  # ",
           current_module, ". modul – ", module_rows$module_title[[1]])
  )

  for (page_index in seq_len(nrow(module_rows))) {
    is_last_page <- page_index == nrow(module_rows)
    page_branch <- if (is_last_page) "└── " else "├── "
    page <- module_rows[page_index, ]
    tree_lines <- c(
      tree_lines,
      paste0(child_indent, page_branch, page$file_name,
             "  # [", page$page_type, "] ", page$page_title)
    )
  }
}

writeLines(tree_lines, con = tree_path, useBytes = TRUE)

message("Created: ", manifest_path)
message("Created: ", tree_path)
message("Pages: ", nrow(course_manifest))
print(with(course_manifest, table(module_number, page_type)))
