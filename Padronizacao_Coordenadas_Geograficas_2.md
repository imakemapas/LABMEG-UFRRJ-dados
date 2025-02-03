
### 🌍 ─────────────────────────────────────────────── 🌍

### 🌍 THERESA ROCCO PEREIRA BARBOSA

### 🌍 Brasileira \| Geocientista \| Dados

### 🌍 <imakemapas@outlook.com.br> \| +55 24 998417085

### 🌍 ─────────────────────────────────────────────── 🌍

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
```

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
