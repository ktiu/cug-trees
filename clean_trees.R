library(tidyverse)
library(sf)

if(!file.exists("trees.csv")){
  curl::curl_download("https://opendata.arcgis.com/datasets/8c50110f190e43599baf50701aaff13a_0.csv",
                      "trees.csv")
}

trees_raw <- read_csv("trees.csv", guess_max = 1500)

gattungen <- tribble (
  ~pattern,    ~gattung,       ~typ,
  "buche",     "Buche",        "Laubbaum",
  "ahorn",     "Ahorn",        "Laubbaum",
  "eiche",     "Eiche",        "Laubbaum",
  "esche",     "Esche",        "Laubbaum",
  "birke",     "Birke",        "Laubbaum",
  "kastanie",  "Kastanie",     "Laubbaum",
  "kirsche",   "Kirsche",      "Obst-/Nussbaum",
  "linde",     "Linde",        "Laubbaum",
  "hasel",     "Hasel",        "Obst-/Nussbaum",
  "erle",      "Erle",         "Laubbaum",
  "pappel",    "Pappel",       "Laubbaum",
  "walnuss",   "Walnuss",      "Obst-/Nussbaum",
  "apfel",     "Apfel",        "Obst-/Nussbaum",
  "tanne",     "Tanne",        "Nadelbaum",
  "fichte",    "Fichte",       "Nadelbaum",
  "kiefer",    "Kiefer",       "Nadelbaum",
  "platane",   "Platane",      "Laubbaum",
  "eibe",      "Eibe",         "Laubbaum",
  "robinie",   "Robinie",      "Laubbaum",
  "weissdorn", "Weißdorn",     "Laubbaum",
  "birne",     "Birne",        "Obst-/Nussbaum",
  "ulme",      "Ulme",         "Laubbaum",
  "maulbeer",  "Maulbeerbaum", "Obst-/Nussbaum",
  "flieder",   "Flieder",      "Laubbaum",
  "weide",     "Weide",        "Laubbaum",
  "laerche",   "Lärche",       "Nadelbaum",
  "magnolie",  "Magnolie",     "Laubbaum",
  "mehlbeere", "Mehlbeere",    "Laubbaum"
)

find_gattung <- function(string){
  str_extract_all(string, regex(gattungen$pattern, ignore_case = T)) %>%
    unlist() %>%
    last() %>%
    tolower()
}

find_gattung <- Vectorize(find_gattung)

trees_raw %>%
  mutate(gattung=find_gattung(Gattung_Ar)) -> detected

"stadtteile/Stadtteile_Frankfurt_am_Main.shp" %>%
  read_sf() %>%
  st_transform(32632) -> stadtteile

detected %>%
  mutate(gattung=unname(gattung)) %>%
  left_join(gattungen, by=c(gattung="pattern")) %>%
  st_as_sf(coords=(c("RECHTSWERT", "HOCHWERT"))) %>%
  st_set_crs(32632) %>%
  st_join(stadtteile) -> geotrees

geotrees %>%
  transmute(Pflanzjahr,
            Kronendurchmesser=Kronendurc,
            Gattung=factor(gattung.y),
            Typ=factor(typ),
            Stadtteil=factor(STTLNAME),
            Objekt,
            geometry) -> trees

save(trees, stadtteile, file="trees.Rdata")
