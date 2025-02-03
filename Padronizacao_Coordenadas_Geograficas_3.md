
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
renomeando colunas e removendo linhas com valores ausentes. Remoção de
espaços desnecessários – além de tratar casos em que o ponto decimal
pode estar ausente, convertendo números inteiros grandes no formato
decimal esperado. O objeto final é salvo como um shapefile, garantindo
que os dados espaciais integrados estejam prontos para análises
geográficas complexas e mapeamentos.

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

##### **1. CARREGAMENTO E LIMPEZA DOS DADOS —————————————**

``` r
# Leitura da planilha
dft <- readxl::read_excel("tabela_sem_ponto.xlsx")

# Renomeia 
dft <- dft |> 
  dplyr::rename(Sample_ID = `Sample ID`)

# Renomeia e seleciona as colunas necessárias
dfc <- dft |> 
  dplyr::select(Sample_ID, Lat, Long) |> 
  na.exclude()
```

``` r
# Checa numero de linhas noS dataframes inicial e com NA removidos.
nrow(dft)
```

    ## [1] 185

``` r
nrow(dfc)
```

    ## [1] 185

``` r
# Função para padronizar coordenadas
padronizar_coordenadas <- function(coord) {
  coord <- as.character(coord) |>  
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

    ## tibble [185 × 3] (S3: tbl_df/tbl/data.frame)
    ##  $ Sample_ID: chr [1:185] "JS01" "JS02" "JS03" "SJ04" ...
    ##  $ Lat      : num [1:185] -29.4 -29.4 -29.4 -29.7 -29.7 ...
    ##  $ Long     : Named num [1:185] -54.8 -54.8 -54.8 -53.8 -53.8 ...
    ##   ..- attr(*, "names")= chr [1:185] "-547623" "-547623" "-547623" "-537729" ...

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

    ## tibble [185 × 27] (S3: tbl_df/tbl/data.frame)
    ##  $ Sample_ID: chr [1:185] "JS01" "JS02" "JS03" "SJ04" ...
    ##  $ LATf     : num [1:185] -29.4 -29.4 -29.4 -29.7 -29.7 ...
    ##  $ LONGf    : Named num [1:185] -54.8 -54.8 -54.8 -53.8 -53.8 ...
    ##   ..- attr(*, "names")= chr [1:185] "-547623" "-547623" "-547623" "-537729" ...
    ##  $ Reference: chr [1:185] "Mincato, 2000" "Mincato, 2000" "Mincato, 2000" "Mincato, 2000" ...
    ##  $ LATi     : num [1:185] -293602 -293602 -293602 -296572 -296572 ...
    ##  $ LONGi    : chr [1:185] "-547623" "-547623" "-547623" "-537729" ...
    ##  $ SiO2     : num [1:185] 57.6 53.3 54.8 55.2 54.1 ...
    ##  $ MgO      : num [1:185] 2.98 6.39 4.4 3.47 3.35 6.04 5.3 3.43 3.02 6.77 ...
    ##  $ TiO2     : num [1:185] 1.74 1 1.4 1.94 1.79 0.99 1.16 1.64 1.86 1.03 ...
    ##  $ Al2O3    : num [1:185] 13.1 15.7 13.7 12.9 12.5 ...
    ##  $ Fe2O3t   : num [1:185] 13.1 10.6 12.5 15.1 15 ...
    ##  $ MnO      : num [1:185] 0.18 0.16 0.19 0.2 0.18 0.15 0.19 0.17 0.19 0.18 ...
    ##  $ CaO      : num [1:185] 6.17 10.01 7.95 6.89 6.44 ...
    ##  $ Na2O     : num [1:185] 2.54 2.1 2.52 2.49 2.43 1.95 2.37 2.58 2.38 2.41 ...
    ##  $ K2O      : num [1:185] 2.54 1.67 1.69 2.03 2.57 1.58 1.53 2.58 2.36 1.35 ...
    ##  $ P2O5     : num [1:185] 0.26 0.15 0.21 0.27 0.27 0.14 0.15 0.29 0.21 0.14 ...
    ##  $ LOI      : chr [1:185] "0.32" "0.19" "0.2" "0.32" ...
    ##  $ Fe2O3    : logi [1:185] NA NA NA NA NA NA ...
    ##  $ FeO      : logi [1:185] NA NA NA NA NA NA ...
    ##  $ Ba       : num [1:185] 487 359 383 347 327 430 334 472 553 484 ...
    ##  $ Rb       : num [1:185] 106 80 63 74 113 44 49 105 83 39 ...
    ##  $ Sr       : num [1:185] 190 215 225 216 214 159 225 194 189 224 ...
    ##  $ Y        : num [1:185] 41 31 32 39 45 19 23 40 32 22 ...
    ##  $ Zr       : num [1:185] 208 161 156 187 188 111 127 218 183 127 ...
    ##  $ Ti       : num [1:185] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ LATtemp  : num [1:185] -29.4 -29.4 -29.4 -29.7 -29.7 ...
    ##  $ LONGtemp : Named num [1:185] -54.8 -54.8 -54.8 -53.8 -53.8 ...
    ##   ..- attr(*, "names")= chr [1:185] "-547623" "-547623" "-547623" "-537729" ...

``` r
dff <- dff |> 
  mutate(
    LOI = as.numeric(LOI),
    Fe2O3 = as.numeric(Fe2O3),
    FeO = as.numeric(FeO)
    )
str(dff)
```

    ## tibble [185 × 27] (S3: tbl_df/tbl/data.frame)
    ##  $ Sample_ID: chr [1:185] "JS01" "JS02" "JS03" "SJ04" ...
    ##  $ LATf     : num [1:185] -29.4 -29.4 -29.4 -29.7 -29.7 ...
    ##  $ LONGf    : Named num [1:185] -54.8 -54.8 -54.8 -53.8 -53.8 ...
    ##   ..- attr(*, "names")= chr [1:185] "-547623" "-547623" "-547623" "-537729" ...
    ##  $ Reference: chr [1:185] "Mincato, 2000" "Mincato, 2000" "Mincato, 2000" "Mincato, 2000" ...
    ##  $ LATi     : num [1:185] -293602 -293602 -293602 -296572 -296572 ...
    ##  $ LONGi    : chr [1:185] "-547623" "-547623" "-547623" "-537729" ...
    ##  $ SiO2     : num [1:185] 57.6 53.3 54.8 55.2 54.1 ...
    ##  $ MgO      : num [1:185] 2.98 6.39 4.4 3.47 3.35 6.04 5.3 3.43 3.02 6.77 ...
    ##  $ TiO2     : num [1:185] 1.74 1 1.4 1.94 1.79 0.99 1.16 1.64 1.86 1.03 ...
    ##  $ Al2O3    : num [1:185] 13.1 15.7 13.7 12.9 12.5 ...
    ##  $ Fe2O3t   : num [1:185] 13.1 10.6 12.5 15.1 15 ...
    ##  $ MnO      : num [1:185] 0.18 0.16 0.19 0.2 0.18 0.15 0.19 0.17 0.19 0.18 ...
    ##  $ CaO      : num [1:185] 6.17 10.01 7.95 6.89 6.44 ...
    ##  $ Na2O     : num [1:185] 2.54 2.1 2.52 2.49 2.43 1.95 2.37 2.58 2.38 2.41 ...
    ##  $ K2O      : num [1:185] 2.54 1.67 1.69 2.03 2.57 1.58 1.53 2.58 2.36 1.35 ...
    ##  $ P2O5     : num [1:185] 0.26 0.15 0.21 0.27 0.27 0.14 0.15 0.29 0.21 0.14 ...
    ##  $ LOI      : num [1:185] 0.32 0.19 0.2 0.32 0.79 3.89 0.2 0.36 0.13 1.1 ...
    ##  $ Fe2O3    : num [1:185] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ FeO      : num [1:185] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Ba       : num [1:185] 487 359 383 347 327 430 334 472 553 484 ...
    ##  $ Rb       : num [1:185] 106 80 63 74 113 44 49 105 83 39 ...
    ##  $ Sr       : num [1:185] 190 215 225 216 214 159 225 194 189 224 ...
    ##  $ Y        : num [1:185] 41 31 32 39 45 19 23 40 32 22 ...
    ##  $ Zr       : num [1:185] 208 161 156 187 188 111 127 218 183 127 ...
    ##  $ Ti       : num [1:185] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ LATtemp  : num [1:185] -29.4 -29.4 -29.4 -29.7 -29.7 ...
    ##  $ LONGtemp : Named num [1:185] -54.8 -54.8 -54.8 -53.8 -53.8 ...
    ##   ..- attr(*, "names")= chr [1:185] "-547623" "-547623" "-547623" "-537729" ...

``` r
summary(dff)
```

    ##   Sample_ID              LATf            LONGf         Reference              LATi            LONGi                SiO2      
    ##  Length:185         Min.   :-29.66   Min.   :-54.76   Length:185         Min.   :-296572   Length:185         Min.   :48.93  
    ##  Class :character   1st Qu.:-29.35   1st Qu.:-52.69   Class :character   1st Qu.:-293501   Class :character   1st Qu.:50.76  
    ##  Mode  :character   Median :-28.09   Median :-51.30   Mode  :character   Median :-280546   Mode  :character   Median :52.66  
    ##                     Mean   :-26.99   Mean   :-51.43                      Mean   :-250989                      Mean   :56.00  
    ##                     3rd Qu.:-25.54   3rd Qu.:-49.50                      3rd Qu.:-253091                      3rd Qu.:57.58  
    ##                     Max.   :-20.30   Max.   :-47.75                      Max.   : -22319                      Max.   :71.97  
    ##                                                                                                                              
    ##       MgO            TiO2           Al2O3           Fe2O3t           MnO              CaO              Na2O            K2O       
    ##  Min.   :0.37   Min.   :0.640   Min.   :11.05   Min.   : 4.70   Min.   :0.0100   Min.   : 0.890   Min.   :1.780   Min.   :0.290  
    ##  1st Qu.:2.98   1st Qu.:1.040   1st Qu.:12.60   1st Qu.: 9.75   1st Qu.:0.1600   1st Qu.: 6.070   1st Qu.:2.470   1st Qu.:1.040  
    ##  Median :4.13   Median :1.660   Median :13.04   Median :12.90   Median :0.1900   Median : 8.210   Median :2.710   Median :1.570  
    ##  Mean   :3.81   Mean   :1.957   Mean   :13.22   Mean   :11.70   Mean   :0.1749   Mean   : 7.244   Mean   :2.821   Mean   :2.077  
    ##  3rd Qu.:5.12   3rd Qu.:2.780   3rd Qu.:13.58   3rd Qu.:14.36   3rd Qu.:0.2100   3rd Qu.: 9.290   3rd Qu.:3.140   3rd Qu.:2.590  
    ##  Max.   :7.79   Max.   :4.230   Max.   :15.92   Max.   :16.43   Max.   :0.2800   Max.   :11.280   Max.   :4.840   Max.   :5.490  
    ##                                                                                                                                  
    ##       P2O5             LOI              Fe2O3          FeO            Ba               Rb               Sr              Y        
    ##  Min.   :0.0800   Min.   :-0.0500   Min.   : NA   Min.   : NA   Min.   :  38.0   Min.   :  7.00   Min.   : 80.0   Min.   :17.00  
    ##  1st Qu.:0.2000   1st Qu.: 0.3225   1st Qu.: NA   1st Qu.: NA   1st Qu.: 247.0   1st Qu.: 29.00   1st Qu.:173.5   1st Qu.:31.00  
    ##  Median :0.2700   Median : 0.6850   Median : NA   Median : NA   Median : 347.0   Median : 42.00   Median :251.0   Median :38.00  
    ##  Mean   :0.3126   Mean   : 0.8225   Mean   :NaN   Mean   :NaN   Mean   : 444.5   Mean   : 73.37   Mean   :314.3   Mean   :38.62  
    ##  3rd Qu.:0.4000   3rd Qu.: 1.0975   3rd Qu.: NA   3rd Qu.: NA   3rd Qu.: 618.0   3rd Qu.:100.00   3rd Qu.:409.0   3rd Qu.:43.00  
    ##  Max.   :1.3800   Max.   : 3.8900   Max.   : NA   Max.   : NA   Max.   :1620.0   Max.   :229.00   Max.   :843.0   Max.   :88.00  
    ##                   NA's   :3         NA's   :185   NA's   :185   NA's   :8                         NA's   :2                      
    ##        Zr              Ti           LATtemp          LONGtemp     
    ##  Min.   : 71.0   Min.   : 5875   Min.   :-29.66   Min.   :-54.76  
    ##  1st Qu.:157.5   1st Qu.: 6235   1st Qu.:-29.35   1st Qu.:-52.69  
    ##  Median :219.5   Median : 6415   Median :-28.09   Median :-51.30  
    ##  Mean   :230.4   Mean   :12176   Mean   :-26.99   Mean   :-51.43  
    ##  3rd Qu.:292.8   3rd Qu.:21133   3rd Qu.:-25.54   3rd Qu.:-49.50  
    ##  Max.   :637.0   Max.   :24640   Max.   :-20.30   Max.   :-47.75  
    ##  NA's   :1       NA's   :174

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
terra::writeVector(vect_ibge, "grau_sem_ponto_final.shp", overwrite = TRUE)
```
