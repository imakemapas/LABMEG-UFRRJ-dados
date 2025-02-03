
#### 🌍 ─────────────────────────────────────────────── 🌍

#### 🌍 THERESA ROCCO PEREIRA BARBOSA

#### 🌍 Brasileira \| Geocientista \| Dados

#### 🌍 <imakemapas@outlook.com.br> \| +55 24 998417085

#### 🌍 ─────────────────────────────────────────────── 🌍

2025-02-03

### **INTEGRAÇÃO DE DADOS CIENTÍFICOS LEGADOS: PADRONIZAÇÃO DE COORDENADAS GEOGRÁFICAS**

A integração de dados científicos legados, especialmente aqueles com
informações georreferenciadas, apresenta desafios. Um dos problemas mais
comuns é a inconsistência no formato das coordenadas de latitude e
longitude. Essas inconsistências podem surgir devido a diferenças nos
sistemas de coleta de dados, variações culturais na representação de
coordenadas ou até mesmo erros de digitação.

Este código foi desenvolvido para enfrentar esses desafios, oferecendo
uma solução que padroniza as coordenadas geográficas em formato decimal.
O processo envolve a leitura dos dados a partir de uma planilha Excel,
renomeando colunas e removendo linhas com valores ausentes. Correção de
inconsistências nos símbolos utilizados nas coordenadas – como a
substituição de vírgula por ponto e remoção de espaços desnecessários –
além de tratar casos em que o ponto decimal pode estar ausente,
convertendo números inteiros grandes no formato decimal esperado. O
objeto final é salvo como um shapefile, garantindo que os dados
espaciais integrados estejam prontos para análises geográficas complexas
e mapeamentos.

O fluxo trabalho é dividido nas etapas:

1.  Carregamento e limpeza dos dados
2.  Padronização de símbolos e formatos
3.  Exportação dos dados corrigidos

A combinação de técnicas de manipulação de strings e transformação de
coordenadas em R garante que os dados estejam padronizados e prontos
para uso em estudos e aplicações geoespaciais. Este fluxo de trabalho
torna o processo replicável, podendo ser adaptado a outros conjuntos de
dados legados que enfrentem desafios semelhantes.

##### **0. CARREGAMENTO BIBLIOTECAS NECESSÁRIAS ———————————–**

``` r
library(dplyr)       # Para manipulação de dados
library(readxl)      # Para leitura de arquivos Excel
library(stringr)     # Para manipulação de strings (texto)
library(sf)          # Para trabalhar com dados espaciais
```

``` r
# Leitura da planilha
dft <- readxl::read_excel("tabela_grau_decimal.xlsx")

# Renomeia 
dft <- dft |> 
  dplyr::rename(Sample_ID = `Sample ID`)

# Renomeia e seleciona as colunas necessárias
dfc <- dft |> 
  dplyr::select(Sample_ID, Lat, Long) |> 
  na.exclude()

nrow(dft)
```

    ## [1] 201

``` r
nrow(dfc)
```

    ## [1] 201

###### **2. PADRONIZAÇÃO DE SÍMBOLOS E FORMATOS ———————————–**

``` r
# Função para padronizar coordenadas
padronizar_coordenadas <- function(coord) {
  coord <- as.character(coord) |>  
    str_replace_all(",", ".") |>  # Substitui "," por "."
    str_trim()                    # Remove espaços extras
  
  # Adiciona ponto decimal se estiver ausente em valores numéricos grandes
  if (str_detect(coord, "^-?\\d{5,}$")) {
    coord <- str_replace(coord, "(-?\\d{2})(\\d+)", "\\1.\\2")
  }
  
  return(as.numeric(coord))
}

# Aplica a função para corrigir Lat e Long
dfc <- dfc |> 
  dplyr::mutate(
    Lat = sapply(Lat, padronizar_coordenadas),
    Long = sapply(Long, padronizar_coordenadas)
  )

str(dfc)
```

    ## tibble [201 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Sample_ID: chr [1:201] "GB20A" "GB40BB" "GB18AC" "GB2AH" ...
    ##  $ Lat      : Named num [1:201] -28.4 -28.4 -28.4 -28.4 -28.4 ...
    ##   ..- attr(*, "names")= chr [1:201] "-28.36" "-28.36" "-28.36" "-28.36" ...
    ##  $ Long     : Named num [1:201] -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 ...
    ##   ..- attr(*, "names")= chr [1:201] "-49.6" "-49.6" "-49.6" "-49.6" ...

##### **3. EXPORTAÇÃO DOS DADOS CORRIGIDOS —————————————**

``` r
# Renomear colunas em dff
dff <- dfc |> 
  dplyr::rename(LATf = Lat, LONGf = Long) 

# Renomear colunas em dft
dft <- dft |> 
  dplyr::rename(LATi = Lat, LONGi = Long)

names(dft)
```

    ##  [1] "Reference" "Sample_ID" "LATi"      "LONGi"     "SiO2"      "MgO"       "TiO2"      "Al2O3"     "Fe2O3t"    "MnO"      
    ## [11] "CaO"       "Na2O"      "K2O"       "P2O5"      "LOI"       "Fe2O3"     "FeO"       "Ba"        "Rb"        "Sr"       
    ## [21] "Y"         "Zr"        "Ti"

``` r
names(dff)
```

    ## [1] "Sample_ID" "LATf"      "LONGf"

``` r
dff <- inner_join(dff, dft, by = "Sample_ID")
names(dff)
```

    ##  [1] "Sample_ID" "LATf"      "LONGf"     "Reference" "LATi"      "LONGi"     "SiO2"      "MgO"       "TiO2"      "Al2O3"    
    ## [11] "Fe2O3t"    "MnO"       "CaO"       "Na2O"      "K2O"       "P2O5"      "LOI"       "Fe2O3"     "FeO"       "Ba"       
    ## [21] "Rb"        "Sr"        "Y"         "Zr"        "Ti"

``` r
dff <- dff |> dplyr::mutate(LATtemp = LATf, LONGtemp = LONGf)
str(dff)
```

    ## tibble [201 × 27] (S3: tbl_df/tbl/data.frame)
    ##  $ Sample_ID: chr [1:201] "GB20A" "GB40BB" "GB18AC" "GB2AH" ...
    ##  $ LATf     : Named num [1:201] -28.4 -28.4 -28.4 -28.4 -28.4 ...
    ##   ..- attr(*, "names")= chr [1:201] "-28.36" "-28.36" "-28.36" "-28.36" ...
    ##  $ LONGf    : Named num [1:201] -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 ...
    ##   ..- attr(*, "names")= chr [1:201] "-49.6" "-49.6" "-49.6" "-49.6" ...
    ##  $ Reference: chr [1:201] "Mantovani et al., 1985" "Mantovani et al., 1985" "Mantovani et al., 1985; Marques et al., 1999" "Mantovani et al., 1985" ...
    ##  $ LATi     : chr [1:201] "-28.36" "-28.36" "-28.36" "-28.36" ...
    ##  $ LONGi    : chr [1:201] "-49.6" "-49.6" "-49.6" "-49.6" ...
    ##  $ SiO2     : num [1:201] 49.5 49.7 51.1 53 48.7 ...
    ##  $ MgO      : num [1:201] 8.53 7.73 6.38 6.07 5.95 5.44 4.77 5.12 4.59 4.79 ...
    ##  $ TiO2     : num [1:201] 0.8 0.9 0.99 0.87 1.43 1.35 1.22 1.69 1.69 1.71 ...
    ##  $ Al2O3    : num [1:201] 15.8 15.9 15.8 16.1 15.2 ...
    ##  $ Fe2O3t   : num [1:201] 9.77 10.2 10.97 10.44 12.88 ...
    ##  $ MnO      : num [1:201] 0.15 0.17 0.19 0.17 0.21 0.17 0.18 0.22 0.19 0.21 ...
    ##  $ CaO      : num [1:201] 10.52 10.81 9.43 8.94 10.13 ...
    ##  $ Na2O     : num [1:201] 1.84 2.31 2.31 2.51 2.27 2.34 2.76 2.5 3 2.53 ...
    ##  $ K2O      : num [1:201] 0.57 0.56 0.84 1.22 0.4 1.17 1.57 1.24 0.76 0.9 ...
    ##  $ P2O5     : num [1:201] 0.07 0.13 0.11 0.16 0.18 0.15 0.22 0.17 0.22 0.18 ...
    ##  $ LOI      : chr [1:201] "2.44" "1.5" "1.9" "0.42" ...
    ##  $ Fe2O3    : num [1:201] 7.22 6.35 NA NA 5.69 8.93 5.48 7.8 6.68 NA ...
    ##  $ FeO      : num [1:201] 2.55 3.85 NA NA 7.19 3.53 5.61 5.59 5.36 NA ...
    ##  $ Ba       : num [1:201] 113 181 221 322 196 272 407 403 327 365 ...
    ##  $ Rb       : num [1:201] 15 9 22 35 10 46 50 41 58 35 ...
    ##  $ Sr       : num [1:201] 188 199 228 237 254 232 225 259 279 265 ...
    ##  $ Y        : num [1:201] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Zr       : num [1:201] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Ti       : num [1:201] 4796 5396 5935 5216 8573 ...
    ##  $ LATtemp  : Named num [1:201] -28.4 -28.4 -28.4 -28.4 -28.4 ...
    ##   ..- attr(*, "names")= chr [1:201] "-28.36" "-28.36" "-28.36" "-28.36" ...
    ##  $ LONGtemp : Named num [1:201] -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 ...
    ##   ..- attr(*, "names")= chr [1:201] "-49.6" "-49.6" "-49.6" "-49.6" ...

``` r
dff <- dff |> 
  mutate(
    LOI = as.numeric(LOI)
    )
str(dff)
```

    ## tibble [201 × 27] (S3: tbl_df/tbl/data.frame)
    ##  $ Sample_ID: chr [1:201] "GB20A" "GB40BB" "GB18AC" "GB2AH" ...
    ##  $ LATf     : Named num [1:201] -28.4 -28.4 -28.4 -28.4 -28.4 ...
    ##   ..- attr(*, "names")= chr [1:201] "-28.36" "-28.36" "-28.36" "-28.36" ...
    ##  $ LONGf    : Named num [1:201] -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 ...
    ##   ..- attr(*, "names")= chr [1:201] "-49.6" "-49.6" "-49.6" "-49.6" ...
    ##  $ Reference: chr [1:201] "Mantovani et al., 1985" "Mantovani et al., 1985" "Mantovani et al., 1985; Marques et al., 1999" "Mantovani et al., 1985" ...
    ##  $ LATi     : chr [1:201] "-28.36" "-28.36" "-28.36" "-28.36" ...
    ##  $ LONGi    : chr [1:201] "-49.6" "-49.6" "-49.6" "-49.6" ...
    ##  $ SiO2     : num [1:201] 49.5 49.7 51.1 53 48.7 ...
    ##  $ MgO      : num [1:201] 8.53 7.73 6.38 6.07 5.95 5.44 4.77 5.12 4.59 4.79 ...
    ##  $ TiO2     : num [1:201] 0.8 0.9 0.99 0.87 1.43 1.35 1.22 1.69 1.69 1.71 ...
    ##  $ Al2O3    : num [1:201] 15.8 15.9 15.8 16.1 15.2 ...
    ##  $ Fe2O3t   : num [1:201] 9.77 10.2 10.97 10.44 12.88 ...
    ##  $ MnO      : num [1:201] 0.15 0.17 0.19 0.17 0.21 0.17 0.18 0.22 0.19 0.21 ...
    ##  $ CaO      : num [1:201] 10.52 10.81 9.43 8.94 10.13 ...
    ##  $ Na2O     : num [1:201] 1.84 2.31 2.31 2.51 2.27 2.34 2.76 2.5 3 2.53 ...
    ##  $ K2O      : num [1:201] 0.57 0.56 0.84 1.22 0.4 1.17 1.57 1.24 0.76 0.9 ...
    ##  $ P2O5     : num [1:201] 0.07 0.13 0.11 0.16 0.18 0.15 0.22 0.17 0.22 0.18 ...
    ##  $ LOI      : num [1:201] 2.44 1.5 1.9 0.42 2.58 2.58 1.35 2.01 1.58 1.48 ...
    ##  $ Fe2O3    : num [1:201] 7.22 6.35 NA NA 5.69 8.93 5.48 7.8 6.68 NA ...
    ##  $ FeO      : num [1:201] 2.55 3.85 NA NA 7.19 3.53 5.61 5.59 5.36 NA ...
    ##  $ Ba       : num [1:201] 113 181 221 322 196 272 407 403 327 365 ...
    ##  $ Rb       : num [1:201] 15 9 22 35 10 46 50 41 58 35 ...
    ##  $ Sr       : num [1:201] 188 199 228 237 254 232 225 259 279 265 ...
    ##  $ Y        : num [1:201] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Zr       : num [1:201] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Ti       : num [1:201] 4796 5396 5935 5216 8573 ...
    ##  $ LATtemp  : Named num [1:201] -28.4 -28.4 -28.4 -28.4 -28.4 ...
    ##   ..- attr(*, "names")= chr [1:201] "-28.36" "-28.36" "-28.36" "-28.36" ...
    ##  $ LONGtemp : Named num [1:201] -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 -49.6 ...
    ##   ..- attr(*, "names")= chr [1:201] "-49.6" "-49.6" "-49.6" "-49.6" ...

``` r
summary(dff)
```

    ##   Sample_ID              LATf            LONGf         Reference             LATi              LONGi                SiO2      
    ##  Length:201         Min.   :-29.79   Min.   :-58.32   Length:201         Length:201         Length:201         Min.   :47.97  
    ##  Class :character   1st Qu.:-28.36   1st Qu.:-49.85   Class :character   Class :character   Class :character   1st Qu.:50.91  
    ##  Mode  :character   Median :-28.12   Median :-49.60   Mode  :character   Mode  :character   Mode  :character   Median :51.74  
    ##                     Mean   :-24.59   Mean   :-43.57                                                            Mean   :52.91  
    ##                     3rd Qu.:-27.60   3rd Qu.:-49.46                                                            3rd Qu.:53.44  
    ##                     Max.   : 25.71   Max.   : 54.82                                                            Max.   :69.56  
    ##                                                                                                                               
    ##       MgO             TiO2           Al2O3           Fe2O3t           MnO              CaO             Na2O            K2O       
    ##  Min.   :1.120   Min.   :0.800   Min.   :11.93   Min.   : 4.83   Min.   :0.0900   Min.   : 2.77   Min.   :1.810   Min.   :0.150  
    ##  1st Qu.:4.320   1st Qu.:1.470   1st Qu.:13.26   1st Qu.:11.95   1st Qu.:0.1700   1st Qu.: 7.83   1st Qu.:2.500   1st Qu.:0.970  
    ##  Median :4.610   Median :1.830   Median :13.65   Median :12.87   Median :0.1800   Median : 8.37   Median :2.670   Median :1.430  
    ##  Mean   :4.601   Mean   :2.414   Mean   :13.84   Mean   :12.33   Mean   :0.1786   Mean   : 8.27   Mean   :2.712   Mean   :1.513  
    ##  3rd Qu.:5.120   3rd Qu.:3.600   3rd Qu.:14.34   3rd Qu.:13.46   3rd Qu.:0.2000   3rd Qu.: 9.15   3rd Qu.:2.910   3rd Qu.:1.850  
    ##  Max.   :8.530   Max.   :4.330   Max.   :16.75   Max.   :14.90   Max.   :0.2400   Max.   :11.61   Max.   :3.890   Max.   :4.190  
    ##                                                                                                                                  
    ##       P2O5             LOI              Fe2O3             FeO               Ba               Rb               Sr        
    ##  Min.   :0.0700   Min.   :-0.0600   Min.   : 0.000   Min.   : 0.620   Min.   :  58.0   Min.   :  5.46   Min.   : 116.0  
    ##  1st Qu.:0.2200   1st Qu.: 0.6625   1st Qu.: 4.400   1st Qu.: 4.765   1st Qu.: 336.0   1st Qu.: 23.00   1st Qu.: 228.0  
    ##  Median :0.3000   Median : 1.0250   Median : 5.320   Median : 6.450   Median : 479.5   Median : 38.00   Median : 294.0  
    ##  Mean   :0.3634   Mean   : 1.1366   Mean   : 6.163   Mean   : 6.098   Mean   : 481.3   Mean   : 44.39   Mean   : 448.8  
    ##  3rd Qu.:0.5200   3rd Qu.: 1.4800   3rd Qu.: 6.800   3rd Qu.: 7.795   3rd Qu.: 611.2   3rd Qu.: 48.00   3rd Qu.: 701.0  
    ##  Max.   :1.0500   Max.   : 4.2500   Max.   :15.080   Max.   :10.130   Max.   :1137.0   Max.   :186.00   Max.   :1050.0  
    ##                   NA's   :3         NA's   :132      NA's   :139      NA's   :7                                         
    ##        Y                Zr              Ti           LATtemp          LONGtemp     
    ##  Min.   : 20.00   Min.   : 62.0   Min.   : 4796   Min.   :-29.79   Min.   :-58.32  
    ##  1st Qu.: 32.00   1st Qu.:149.8   1st Qu.: 8978   1st Qu.:-28.36   1st Qu.:-49.85  
    ##  Median : 38.00   Median :269.0   Median :11361   Median :-28.12   Median :-49.60  
    ##  Mean   : 37.13   Mean   :235.5   Mean   :14837   Mean   :-24.59   Mean   :-43.57  
    ##  3rd Qu.: 41.00   3rd Qu.:309.0   3rd Qu.:21642   3rd Qu.:-27.60   3rd Qu.:-49.46  
    ##  Max.   :103.00   Max.   :499.0   Max.   :25959   Max.   : 25.71   Max.   : 54.82  
    ##  NA's   :45       NA's   :45      NA's   :9

``` r
# CRS Final
## O Instituto Brasileiro de Geografia e Estatística (IBGE) recomenda a Projeção Equivalente de Albers 
## com o datum horizontal SIRGAS2000 para a preservação e o cálculo de áreas no território brasileiro
## Referência: 'Informações técnicas e legais para a utilização dos dados publicados' (IBGE, 2023) 
## https://biblioteca.ibge.gov.br/index.php/biblioteca-catalogo?view=detalhes&id=2101998 - Access in 2024 November
final_crs <- 'PROJCS["Conica_Equivalente_de_Albers_Brasil",
    GEOGCS["GCS_SIRGAS2000",
    DATUM["D_SIRGAS2000",
    SPHEROID["Geodetic_Reference_System_of_1980",6378137,298.2572221009113]],
    PRIMEM["Greenwich",0],
    UNIT["Degree",0.017453292519943295]],
    PROJECTION["Albers"],
    PARAMETER["standard_parallel_1",-2],
    PARAMETER["standard_parallel_2",-22],
    PARAMETER["latitude_of_origin",-12],
    PARAMETER["central_meridian",-54],
    PARAMETER["false_easting",5000000],
    PARAMETER["false_northing",10000000],
    UNIT["Meter",1]]'
```

``` r
# Converte o dataframe final diretamente para um SpatVector
vect <- terra::vect(dff, geom = c("LONGtemp", "LATtemp"), crs = "EPSG:4326")

# Reprojeta para o sistema de coordenadas final
vect_ibge <- terra::project(vect, final_crs)

# Extrai as coordenadas e adiciona como colunas
coords <- terra::crds(vect_ibge)
vect_ibge$X <- coords[, 1]
vect_ibge$Y <- coords[, 2]
```

``` r
# Salva o objeto espacial como um shapefile
terra::writeVector(vect_ibge, "grau_decimal_final.shp", overwrite = TRUE)
```
