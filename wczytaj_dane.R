rspo <- read_csv2("data/rspo_2024_07_11.csv", guess_max = 1e5)
rspo <- rspo %>% 
  select(
    RSPO = `Numer RSPO`, Kod = `Kod terytorialny gmina`, Typ, 
    Nazwa_szkoły = Nazwa, Faks, Email = `E-mail`,
    WWW = `Strona www`, Status = `Publiczność status`,
    Kategoria = `Kategoria uczniów`,
    Specyfika = `Specyfika placówki`,
    Dyrektor = `Imię i nazwisko dyrektora`,
    Data_założenia = `Data założenia`,
    Data_działalność = `Data rozpoczęcia działalności`,
    Typ_organu = `Typ organu prowadzącego`,
    Nazwa_organu = `Nazwa organu prowadzącego`,
    Struktura = `Miejsce w strukturze`,
    Liczba_uczniów = `Liczba uczniów`,
    Sport = `Tereny sportowe`,
    Języki = `Języki nauczane`,
    Logopeda = `Czy zatrudnia logopedę`,
    Pedagog = `Czy zatrudnia pedagoga`,
    Psycholog = `Czy zatrudnia psychologa`,
    Oddziały_podstawowe = `Oddziały podstawowe wg specyfiki`,
    Oddziały_dodatkowe = `Oddziały dodatkowe`
  ) %>% 
  mutate(Kod = parse_number(Kod) %>% as.character()) %>% 
  mutate(Kod = ifelse(str_length(Kod) == 6, paste0("0", Kod), Kod))

rspo <- rspo %>% 
  # będę pomijał szkoły specjalne i integracyjne
  mutate(
    Specjalna = ifelse(str_detect(Oddziały_podstawowe, "specjaln")
      | str_detect(Nazwa_szkoły, "SPECJ"), TRUE, FALSE),
    Integracyjna = str_detect(Oddziały_podstawowe, "integracyjn")
  ) %>% 
  select(RSPO, Liczba_uczniów, Oddziały_podstawowe, Typ_organu, 
    Specjalna, Integracyjna)

miasta500 <- c("Warszawa", "Kraków", "Łódź", "Wrocław", "Poznań")
miasta100 <-  c("Gdańsk", "Szczecin", "Lublin", "Bydgoszcz", "Białystok", "Katowice",
  "Gdynia", "Częstochowa", "Rzeszów", "Radom", "Toruń", "Sosnowiec", "Kielce",
  "Gliwice", "Olsztyn", "Bielsko-Biała","Zabrze", "Bytom", "Zielona Góra",
  "Rybnik", "Ruda Śląska", "Opole", "Tychy", "Gorzów Wielkopolski",
  "Dąbrowa Górnicza", "Elbląg", "Płock", "Koszalin", "Tarnów", "Włocławek",
  "Chorzów", "Wałbrzych")

wczytaj_szkoly <- function(rok) {
  # Funkcja wczytuje dane dla każdej ze szkół
  # rok: 2021:2025
  path <- paste0("data/egzamin_8kl_", rok, "_szkoly.xlsx")
  szkoly <- read_excel(path, skip = 1, sheet = 2) %>% 
    select(2:28)
  if (rok <= 2022 | rok == 2025) {
    szkoly <- select(szkoly, -27) # usuń ostatnią kolumnę
  } else {
    szkoly <- select(szkoly, -9) # usuń "Identyfikator szkoły"
  }
  
  nazw <- c("N", "Średnia", "SD", "Mediana", "Moda")
  nazw <- c(paste0("Pol_", nazw), paste0("Mat_", nazw), paste0("Ang_", nazw))
  names(szkoly)[12:26] <- nazw
  
  # szkoly %>% 
  #   mutate(Rok = rok, .before = 1) %>% 
  #   rename(Kod = `Kod teryt gminy`, Nazwa = `Nazwa szkoły`,
  #     Województwo = `województwo - nazwa`, Powiat = `powiat - nazwa`, 
  #     Gmina = `Gmina - nazwa`, Publiczność = `czy publiczna`,
  #     Typ_gminy = `Typ gminy`, Ulica = `Ulica nr`) %>% 
  #   rename_with(.cols = contains("placówki"), ~ "Rodzaj") %>% 
  #   mutate(Publiczność = fct_recode(Publiczność, "Publiczna" = "Tak",
  #     "Niepubliczna" = "Nie"))  %>% 
  #   mutate(Lokalizacja = case_when(
  #     Typ_gminy %in% c("Gmina wiejska", "Obszar wiejski") ~ "Wieś",
  #     Miejscowość %in% miasta500 ~  "Miasto > 500 tys.",
  #     Miejscowość %in% miasta100 ~  "Miasto 100-500 tys.",
  #     Typ_gminy %in% c("Miasto", "Gmina miejska") ~ "Miasto < 100 tys.",
  #     .default = Typ_gminy
  #   )) %>% 
  #   mutate(Lokalizacja = fct_relevel(Lokalizacja, c("Wieś", "Miasto < 100 tys.",
  #     "Miasto 100-500 tys.")))
  
  szkoly <- szkoly %>% 
    mutate(Rok = rok, .before = 1) %>% 
    rename(Kod = `Kod teryt gminy`, Nazwa = `Nazwa szkoły`,
      Województwo = `województwo - nazwa`, Powiat = `powiat - nazwa`, 
      Gmina = `Gmina - nazwa`, Publiczność = `czy publiczna`,
      Typ_gminy = `Typ gminy`, Ulica = `Ulica nr`) %>% 
    rename_with(.cols = contains("placówki"), ~ "Rodzaj") %>% 
    mutate(Publiczność = fct_recode(Publiczność, "Publiczna" = "Tak",
      "Niepubliczna" = "Nie")) %>% 
    mutate(Lokalizacja = case_when(
      Typ_gminy %in% c("Gmina wiejska", "Obszar wiejski") ~ "Wieś",
      # Obszar wiejski to w gminach miejsko-wiejskich
      Miejscowość %in% miasta500 ~  "Miasto > 500 tys.",
      Miejscowość %in% miasta100 ~  "Miasto 100-500 tys.",
      Typ_gminy %in% c("Miasto", "Gmina miejska") ~ "Miasto < 100 tys.",
      .default = Typ_gminy
    )) %>% 
    mutate(Lokalizacja = fct_relevel(Lokalizacja, c("Wieś", "Miasto < 100 tys.",
      "Miasto 100-500 tys.")))
  
  szkoly <- szkoly %>%
    left_join(rspo, by = "RSPO") %>% 
    filter(!str_detect(Nazwa, "CHMURZ")) %>% # specyficzne wyniki
    filter(Rodzaj == "dla młodzieży") %>% 
    filter(!(Specjalna | Integracyjna)) %>% 
    select(-c(Rodzaj, Specjalna, Integracyjna))
  
  szkoly
}

matura <- read_excel("data/matura_mat_2025_szkoly.xlsx", skip = 2, sheet = 1) %>% 
  select(2:13, 17:23, 27:33, 87:93, 335:340)

nazw <- c("N", "Nlaur", "Zda", "Średnia", "SD", "Mediana", "Moda")
nazw <- c(paste0("Pol_", nazw), paste0("Ang_", nazw), paste0("Mat_", nazw),
  paste0("MatRoz_", nazw)) %>% 
  setdiff("MatRoz_Zda") # nie da się nie zdać rozszerzenia
names(matura)[13:39] <- nazw

matura <- matura %>% 
  rename(Kod = `Kod teryt gminy`, Nazwa = `Nazwa szkoły`,
    Województwo = `Województwo - nazwa`, Powiat = `Powiat - nazwa`, 
    Gmina = `Gmina - nazwa`, Rodzaj = `rodzaj placówki\r\n`,
    Typ = `typ placówki\r\n`, Publiczność = `czy publiczna`,
    Typ_gminy = `Typ gminy`, Ulica = `Ulica nr`, RSPO = `RSPO szkoły`)

matura <- matura %>%
  mutate(Publiczność = fct_recode(Publiczność, "Publiczna" = "Tak",
    "Niepubliczna" = "Nie")) %>% 
  mutate(Lokalizacja = case_when(
    Typ_gminy %in% c("gmina wiejska", "obszar wiejski") ~ "Wieś",
    Miejscowość %in% miasta500 ~  "Miasto > 500 tys.",
    Miejscowość %in% miasta100 ~  "Miasto 100-500 tys.",
    Typ_gminy %in% c("miasto", "gmina miejska") ~ "Miasto < 100 tys.",
    .default = Typ_gminy
  )) %>% 
  mutate(Lokalizacja = fct_relevel(Lokalizacja, c("Wieś", "Miasto < 100 tys.",
    "Miasto 100-500 tys.")))

matura <- matura %>% 
  filter(!str_detect(Nazwa, "CHMURZ")) %>% #
  filter(Rodzaj == "dla młodzieży")

wykszt <- read_csv2("data/wykszt.csv") %>% 
  rename_with(~str_remove(., "ogółem;")) %>% 
  rename_with(~str_remove(., fixed(";2021;[osoba]")))
# Poziom wykształcenia z NSP 2021
wykszt <- wykszt %>% 
  select(Kod, 
    Wykszt_wyższe = wyższe,
    Wykszt_średnie = `średnie i policealne - ogółem`,
    Wykszt_zawodowe = `zasadnicze zawodowe/branżowe`, 
    Wykszt_gimnazjalne = gimnazjalne,
    Wykszt_podstawowe = `podstawowe ukończone`,
    Wykszt_nieukończone = `podstawowe nieukończone i bez wykształcenia szkolnego`
  ) %>% 
  mutate(Wykszt_podstawowe = Wykszt_podstawowe + Wykszt_gimnazjalne) %>% 
  select(-Wykszt_gimnazjalne) %>% 
  rowwise() %>% 
  mutate(Suma = sum(c_across(-Kod))) %>% 
  ungroup() %>% 
  mutate(across(Wykszt_wyższe:Wykszt_nieukończone, ~ . / Suma * 100)) %>% 
  select(-Suma) %>% 
  filter(! str_sub(Kod, -1) %in% c("0", "4", "5", "8")) %>% 
  mutate(Kod = str_sub(Kod, 1, 6))

matura <- matura %>% 
  filter(Lokalizacja %in% c("Miasto 100-500 tys.", "Miasto > 500 tys.")) %>% 
  mutate(Kod = str_sub(Kod, 1, 6)) %>% 
  drop_na(Mat_Średnia, Mat_N) %>% 
  summarise(Mat_matura = weighted.mean(Mat_Średnia, w = Mat_N), N = sum(Mat_N),
    .by = c(Województwo, Powiat, Gmina, Kod))


