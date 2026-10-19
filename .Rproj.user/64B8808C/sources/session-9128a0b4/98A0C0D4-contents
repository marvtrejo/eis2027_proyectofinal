#' ---
#' title: "Bloque I: transferencias públicas y desigualdad (ENIGH 2022 y 2024)"
#' format:
#'   html:
#'     toc: true
#'     code-fold: true
#'     embed-resources: true
#' execute:
#'   warning: false
#'   message: false
#' ---
#' 
#' # Configuración inicial
#' 
## --------------------------------------------------------------------------------------------
#| label: setup
# ruta <- "C:/RUTA/A/LA/CARPETA"   # ÚNICO valor editable: carpeta con las bases
# setwd(ruta)

library(dplyr); library(survey); library(convey)
library(dineq); library(flextable); library(ggplot2)
library(tidyr)

# options(survey.lonely.psu = "adjust")  # solo si svydesign marca "single PSU"

# Factores de deflactación (PROVISIONAL = nominal; Marvin los sustituye).
# Convención: se DIVIDE entre el factor; el año base vale 1.
def22 <- 1
def24 <- 1

base22 <- read.csv("concentradohogar22.csv")
base24 <- read.csv("concentradohogar24.csv")

precision <- function(cv) case_when(is.na(cv) ~ "–", 
                                    cv < 15 ~ "Alta", 
                                    cv < 30 ~ "Moderada", 
                                    TRUE ~ "Baja")

semaforo <- function(ft, cols) {           
  for (j in cols) {
    x <- ft$body$dataset[[j]]
    ft <- ft %>% bg(i = !is.na(x) & x >= 15 & x < 30, j = j, bg = "#FFF2CC") %>%
                 bg(i = !is.na(x) & x >= 30, j = j, bg = "#FCE4D6")
  }
  ft
}

#' 

#' 
#' # Tarea 0: Preparar las bases
#' 
## --------------------------------------------------------------------------------------------
#| label: t0-prep
# Variables de ingreso a deflactar (todas las fuentes, un factor por año)
comp_trans <- c("jubilacion", 
                "bene_gob", 
                "becas", 
                "donativos", 
                "remesas",
                "transf_hog", 
                "trans_inst")

fuentes    <- c("ingtrab", 
                "rentas", 
                "transfer", 
                "estim_alqu", 
                "otros_ing")

ing_vars   <- c("ing_cor", 
                fuentes, 
                comp_trans)

# Variables mínimas: si falta o no es numérica, falla aquí y no más adelante
req <- c(ing_vars, 
         "p65mas", 
         "tot_integ", 
         "tam_loc", 
         "upm", 
         "est_dis", 
         "factor")

for (b in list(base22, base24))
  stopifnot(all(req %in% names(b)), 
            all(sapply(b[req], 
                       is.numeric)
                )
            )

# Todo lo derivado se crea ANTES de svydesign() (crear columnas no es filtrar)
prep <- function(b, def) b %>% 
  mutate(across(all_of(ing_vars), ~ .x / def),
         otras  = transfer - jubilacion - bene_gob,   # por confirmar
         ictpc  = ing_cor / tot_integ,
         uno = 1,
         g65    = as.numeric(p65mas > 0),
         tam_loc = as.numeric(tam_loc),
         # deciles nacionales: TODOS los hogares, ictpc deflactado, ponderados por factor
         decil  = ntiles.wtd(ictpc, n = 10, weights = factor),
         recibe_jub = as.numeric(jubilacion > 0), 
         recibe_bene = as.numeric(bene_gob > 0)
         )

base22 <- prep(base22, def22)
base24 <- prep(base24, def24)

# Diseño muestral → convey_prep → subset (orden del profesor)
disenio <- function(b)
  svydesign(ids = ~upm, 
            strata = ~est_dis, 
            weights = ~factor, 
            data = b) %>%
  convey_prep()

d22 <- disenio(base22);  g22 <- subset(d22, p65mas > 0)   # grupo = subpoblación
d24 <- disenio(base24);  g24 <- subset(d24, p65mas > 0)

#' 
## --------------------------------------------------------------------------------------------
#| label: t0-verifica
# Cuadro 1: verificación de las bases
verifica <- function(b) c(
  "Hogares en la muestra"              = nrow(b),
  "Hogares expandidos"                 = sum(b$factor),
  "Personas expandidas (tot_integ)"    = sum(b$factor * b$tot_integ),
  "Hogares del grupo (muestra)"        = sum(b$p65mas > 0),
  "Hogares del grupo (expandidos)"     = sum(b$factor[b$p65mas > 0]),
  "Máx. |ing_cor − suma de fuentes|"   = max(abs(b$ing_cor - rowSums(b[fuentes]))),
  "Máx. |transfer − suma de componentes|" = max(abs(b$transfer - rowSums(b[comp_trans]))),
  "% hogares con ing_cor = 0 (ponderado)" = 100 * sum(b$factor[b$ing_cor == 0]) / sum(b$factor))

v <- sapply(list("2022" = base22, "2024" = base24), verifica)
cuadro1_df <- data.frame(Indicador = rownames(v), v, check.names = FALSE)

cuadro1 <- flextable(cuadro1_df) %>%
  theme_vanilla() %>%
  colformat_double(big.mark = ",", digits = 2) %>%
  set_caption("Cuadro 1. Verificación de las bases ENIGH 2022 y 2024") %>%
  add_footer_lines("Fuente: elaboración propia con datos de la ENIGH 2022 y 2024, INEGI.") %>%
  fontsize(size = 9, part = "footer")
cuadro1

dir.create("salidas", showWarnings = FALSE)
saveRDS(cuadro1_df, "salidas/cuadro1.rds")

#' 
#' # Tarea 1: tamaño del grupo
#' ## Cuadro 2
#' 
## --------------------------------------------------------------------------------------------
#| label: t1-cuadro2

vars_t1 <- ~ing_cor + transfer + jubilacion + bene_gob + recibe_jub + recibe_bene + tot_integ + p65mas
etq <- c(n = "Muestra (n)", uno = "Hogares expandidos", g65 = "% de hogares del país",
         ing_cor = "Ingreso corriente medio", transfer = "Transferencias medias",
         jubilacion = "Jubilaciones medias", bene_gob = "Programas gob. medios",
         recibe_jub = "Cobertura jubilación (%)", recibe_bene = "Cobertura prog. gob. (%)",
         tot_integ = "Integrantes del hogar", p65mas = "Personas 65+ por hogar")

fila <- function(x) data.frame(ind = names(coef(x)), est = coef(x),
                               se = as.numeric(SE(x)), cv = as.numeric(cv(x)) * 100)

t1 <- function(d) {
  lapply(c(resto = 0, grupo = 1), function(k) {
    s <- subset(d, g65 == k)
    bind_rows(data.frame(ind = "n", est = sum(weights(s) > 0)),
              fila(svytotal(~uno, s)), fila(svymean(vars_t1, s)))
  }) %>%
    bind_rows(.id = "pob") %>%
    bind_rows(data.frame(pob = "grupo", fila(svymean(~g65, d))))
}

cuadro2_df <- lapply(list(`2022` = d22, `2024` = d24), t1) %>%
  bind_rows(.id = "anio") %>%
  mutate(across(c(est, se), ~ if_else(ind %in% c("g65", "recibe_jub", "recibe_bene"), .x * 100, .x)),
         across(c(est, se, cv), ~ if_else(pob == "resto" & ind == "p65mas", NA_real_, .x)),
         prec = precision(cv)) %>%
  arrange(anio, match(ind, names(etq))) %>%
  mutate(ind = etq[ind]) %>%
  pivot_wider(names_from = pob, values_from = c(est, se, cv, prec)) %>%
  select(anio, ind, ends_with("resto"), ends_with("grupo"))

# Verificación: grupo + resto = total de hogares
stopifnot(isTRUE(all.equal(
  filter(cuadro2_df, ind == "Hogares expandidos") %>% with(est_resto + est_grupo),
  c(sum(base22$factor), sum(base24$factor)), check.attributes = FALSE)))

#' 
#' ## Prueba de hipótesis
## --------------------------------------------------------------------------------------------
#| label: t1-prueba
tt <- lapply(list(`2022` = d22, `2024` = d24), function(d) svyttest(tot_integ ~ g65, d))
nota_tt <- sapply(names(tt), function(a) with(tt[[a]], sprintf(
  "%s: diferencia grupo − resto en integrantes = %.2f (IC 95%%: %.2f a %.2f; p %s).",
  a, estimate, conf.int[1], conf.int[2],
  if (p.value < .001) "< 0.001" else sprintf("= %.3f", p.value))))

#' 
#' 
#' ## Tabla 2
#' 
## --------------------------------------------------------------------------------------------
#| label: t1-tabla2

cuadro2 <- flextable(cuadro2_df) %>%
  set_header_labels(anio = "Año", ind = "Indicador",
                    est_resto = "Estimación", se_resto = "EE", cv_resto = "CV (%)", prec_resto = "Precisión",
                    est_grupo = "Estimación", se_grupo = "EE", cv_grupo = "CV (%)", prec_grupo = "Precisión") %>%
  add_header_row(values = c("", "Resto de los hogares", "Hogares con 65+"), colwidths = c(2, 4, 4)) %>%
  merge_v(j = "anio") %>%
  theme_vanilla() %>%
  colformat_double(digits = 2, big.mark = ",") %>%
  colformat_double(i = ~ ind %in% c("Muestra (n)", "Hogares expandidos"),
                   j = c("est_resto", "est_grupo"), digits = 0, big.mark = ",") %>%
  semaforo(c("cv_resto", "cv_grupo")) %>%
  set_caption("Cuadro 2. Tamaño del grupo y precisión de las estimaciones, ENIGH 2022 y 2024") %>%
  add_footer_lines(c(
    "Nota: grupo = hogares con al menos un integrante de 65 años o más. Montos en pesos corrientes trimestrales por hogar. Las medias de transferencias incluyen hogares que no las reciben (valor 0); cobertura = % de hogares con monto > 0. EE: error estándar. Precisión según criterio INEGI (alta < 15, moderada 15–30, baja ≥ 30).",
    nota_tt,
    "Fuente: elaboración propia con datos de la ENIGH 2022 y 2024, INEGI.")) %>%
  fontsize(size = 9, part = "footer") %>%
  autofit()
cuadro2

#' 
#' ## Deciles
#' 
## --------------------------------------------------------------------------------------------
#| label: t1-decil

t1_decil <- function(g) {
  cvs <- svyby(~jubilacion + bene_gob, ~decil, g, svymean, vartype = "cv")
  data.frame(decil = as.character(as.roman(cvs$decil)),
             n = svyby(~uno, ~decil, g, unwtd.count)[[2]],
             cv_jub = cvs$cv.jubilacion * 100, cv_bene = cvs$cv.bene_gob * 100) %>%
    mutate(prec_jub = precision(cv_jub), .after = cv_jub) %>%
    mutate(prec_bene = precision(cv_bene))
}

cuadro3_df <- lapply(list(`2022` = g22, `2024` = g24), t1_decil) %>%
  bind_rows(.id = "anio") %>%
  pivot_wider(names_from = anio, values_from = -c(anio, decil), names_vary = "slowest")

cuadro3 <- flextable(cuadro3_df) %>%
  set_header_labels(values = c(decil = "Decil", setNames(
    rep(c("n", "CV % Jub.", "Precisión", "CV % Prog.", "Precisión"), 2), names(cuadro3_df)[-1]))) %>%
  add_header_row(values = c("", "2022", "2024"), colwidths = c(1, 5, 5)) %>%
  theme_vanilla() %>%
  colformat_double(digits = 2) %>%
  colformat_double(j = c("n_2022", "n_2024"), digits = 0, big.mark = ",") %>%
  semaforo(grep("^cv", names(cuadro3_df), value = TRUE)) %>%
  set_caption("Cuadro 3. Coeficiente de variación (%) de las medias de jubilación y programas gubernamentales por decil nacional, hogares con 65+, ENIGH 2022 y 2024") %>%
  add_footer_lines(c("Nota: deciles nacionales de ingreso corriente per cápita (todos los hogares). n = hogares en muestra del grupo en cada decil.",
                     "Fuente: elaboración propia con datos de la ENIGH 2022 y 2024, INEGI.")) %>%
  fontsize(size = 9, part = "footer") %>%
  autofit()
cuadro3

saveRDS(list(cuadro2 = cuadro2_df, cuadro3 = cuadro3_df, ttest = tt), "t1_resultados.rds")

#' 
#' ## Por analizar
#' 
## --------------------------------------------------------------------------------------------
#| label: t1-pendiente

svymean(~interaction(recibe_jub, recibe_bene), g24)
svymean(~interaction(recibe_jub, recibe_bene), g22)


