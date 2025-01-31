
### 🌍 ─────────────────────────────────────────────── 🌍

### 🌍 THERESA ROCCO PEREIRA BARBOSA

### 🌍 Brasileira \| Geocientista \| Dados

### 🌍 <imakemapas@outlook.com.br> \| +55 24 998417085

### 🌍 ─────────────────────────────────────────────── 🌍
2025-01-04

### **INTEGRAÇÃO DE DADOS CIENTÍFICOS LEGADOS: PADRONIZAÇÃO DE COORDENADAS GEOGRÁFICAS**

A integração de dados científicos legados, especialmente aqueles com
informações georreferenciadas, apresenta desafios. Um dos problemas mais
comuns é a inconsistência no formato das coordenadas de latitude e
longitude. Essas inconsistências podem surgir devido a diferenças nos
sistemas de coleta de dados, variações culturais na representação de
coordenadas ou até mesmo erros de digitação.

Este código foi desenvolvido para lidar com esses desafios, oferecendo
uma solução para a padronização de coordenadas geográficas. O processo
envolve a identificação do formato atual das coordenadas (como graus,
minutos e segundos, graus decimais ou graus e minutos decimais), a
correção de símbolos inconsistentes (como “º” em vez de “°”) e a
conversão para um formato uniforme e utilizável. Além disso, o código
realiza a limpeza de dados, removendo valores ausentes e corrigindo
erros específicos, como coordenadas mal formatadas.

O trabalho é dividido em etapas claras:

1.  Carregamento e limpeza dos dados
2.  Padronização de símbolos e formatos
3.  Identificação do formato das coordenadas
4.  Conversão para graus decimais
5.  Exportação dos dados corrigidos

Este processo não apenas resolve problemas imediatos de inconsistência
de dados, mas também estabelece um fluxo de trabalho replicável para
futuras integrações de dados georreferenciados, lembrando que cada caso
é um caso e adaptações provavelmente precisão ser feitas.

A combinação de técnicas de programação em R, como manipulação de
strings e conversão de coordenadas, garante que os dados estejam prontos
para uso em análises científicas e mapeamentos.

##### **0. CARREGAMENTO BIBLIOTECAS NECESSÁRIAS ———————————–**

``` r
library(dplyr)       # Para manipulação de dados
library(readxl)      # Para leitura de arquivos Excel
library(stringr)     # Para manipulação de strings (texto)
library(sf)          # Para trabalhar com dados espaciais
```

##### **1. CARREGAMENTO E LIMPEZA DOS DADOS —————————————**

``` r
# Lê os dados de uma planilha Excel
dft <- readxl::read_excel("tabela_grau_min_seg.xlsx")

# Renomeia a coluna "Sample ID" para "Sample_ID" (remove o espaço no nome)
dft <- dft |> dplyr::rename(Sample_ID = `Sample ID`)

# Seleciona apenas as colunas relevantes para nosso objetivo: Sample_ID, Lat (latitude) e Long (longitude)
dfc <- dft |> dplyr::select(Sample_ID, Lat, Long)

# Remove linhas com valores ausentes (NA)
dfc <- na.exclude(dfc)


nrow(dft)
```

    ## [1] 362

``` r
nrow(dfc)
```

    ## [1] 359

###### **2. PADRONIZAÇÃO DE SÍMBOLOS E FORMATOS ———————————–**

``` r
# Função para padronizar símbolos e remover espaços nas coordenadas
padronizar_coordenadas <- function(coord) {
  coord <- coord |> 
    str_replace_all("º", "°") |>   # Substitui "º" por "°"
    str_replace_all("\\s", "")     # Remove todos os espaços
  return(coord)
}

# Aplica a função de padronização às colunas Lat e Long
dfc <- dfc |> 
  dplyr::mutate(
    Lat = sapply(Lat, padronizar_coordenadas),
    Long = sapply(Long, padronizar_coordenadas)
  )
```

Haverá situações em que o ajuste tem que ser manual (e tudo bem!)

``` r
# Correção manual de valores específicos de longitude
dfc$Long <- ifelse(dfc$Long == "54613°W", "54.613°W", dfc$Long)

# Correção manual de valores específicos de latitude e longitude
dfc <- dfc |> 
  mutate(
    # Ajusta a latitude para o formato desejado
    Lat = case_when(
      Lat == "20°23.58'" ~ "20°23.58'S",
      Lat == "20°25.2'"  ~ "20°25.2'S",
      Lat == "19°52.68'" ~ "19°52.68'S",
      Lat == "20°26.1'"  ~ "20°26.1'S",
      Lat == "18°32.4'"  ~ "18°32.4'S",
      Lat == "20°33.36'" ~ "20°33.36'S",
      Lat == "22°6.66'"  ~ "22°6.66'S",
      Lat == "22°8.7'"   ~ "22°8.7'S",
      Lat == "21°38.58'" ~ "21°38.58'S",
      Lat == "20°26.46'" ~ "20°26.46'S",
      Lat == "20°25.14'" ~ "20°25.14'S",
      TRUE ~ Lat
    ),
    
    # Ajusta a longitude para o formato desejado
    Long = case_when(
      Long == "54°40.26'" ~ "54°40.26'W",
      Long == "54°44.28'" ~ "54°44.28'W",
      Long == "54°26.52'" ~ "54°26.52'W",
      Long == "54°44.4'"  ~ "54°44.4'W",
      Long == "53°11.94'" ~ "53°11.94'W",
      Long == "54°39.9'"  ~ "54°39.9'W",
      Long == "54°47.7'"  ~ "54°47.7'W",
      Long == "54°49.44'" ~ "54°49.44'W",
      Long == "55°7.38'"  ~ "55°7.38'W",
      Long == "54°46.5'"  ~ "54°46.5'W",
      Long == "54°51.48'" ~ "54°51.48'W",
      TRUE ~ Long
    )
  )
```

##### 3. **IDENTIFICAÇÃO DO FORMATO DAS COORDENADAS ——————————**

###### Essa regex (expressão regular) **`[0-9]+°[0-9]+'[0-9]+(\\.[0-9]+)?\"[NSWE]`** é usada para identificar coordenadas no formato **Graus, Minutos e Segundos (GMS)**, como por exemplo: 26°59’33.32”S.

- **`[0-9]+:`** - `[0-9]:` Corresponde a qualquer dígito de 0 a 9 e `+:`
  Indica que o dígito pode aparecer uma ou mais vezes.

- **`°:`** - Corresponde ao símbolo de graus (°). Esse símbolo é
  literalmente procurado na string.

- **`':`** - Corresponde ao símbolo de minutos (’). Também é um
  caractere literal.

- **`(\\.[0-9]+)?:`** - `\\.:` O ponto (.) é um caractere especial em
  regex, então usamos `\\.` para representar o ponto literal (como em
  33.32).

- **`?:`**- Indica que o grupo anterior (o ponto e os números após ele)
  é opcional. Ou seja, pode ou não existir.

- **`\":`** - `\":` O símbolo de segundos (“) é um caractere especial em
  strings, então usamos `\"` para representá-lo literalmente.

- **`[NSWE]:`** - `[NSWE]`: Corresponde a um dos caracteres N, S, W ou
  E, que indicam a direção da coordenada (Norte, Sul, Oeste ou Leste).

<br>

###### Essa regex **`[0-9]+(\\.[0-9]+)?°?[NSWE]`** é usada para identificar coordenadas no formato **Graus Decimais**, como por exemplo: 32.595°S.

- **`[0-9]+:`** - `[0-9]`: Corresponde a qualquer dígito de 0 a 9 e `+`:
  Indica que o dígito pode aparecer uma ou mais vezes.

- **`(\\.[0-9]+)?:`** - `\\.`: O ponto (.) é um caractere especial em
  regex, então usamos `\\.` para representar o ponto literal (como em
  `32.595`).  
  `[0-9]+`: Captura uma sequência de números após o ponto.  
  `?`: Indica que o grupo anterior (o ponto e os números após ele) é
  opcional. Ou seja, pode ou não existir.

- **`°?:`** - `°`: Corresponde ao símbolo de graus (°). Esse símbolo é
  literalmente procurado na string.  
  `?`: Indica que o símbolo de graus é opcional. Ou seja, pode ou não
  existir.

- **`[NSWE]:`** - `[NSWE]`: Corresponde a um dos caracteres `N`, `S`,
  `W` ou `E`, que indicam a direção da coordenada (Norte, Sul, Oeste ou
  Leste).

<br>

###### Essa regex **`[0-9]+°[0-9]+(\\.[0-9]+)?'[NSWE]`** é usada para identificar coordenadas no formato **Graus e Minutos Decimais**, como por exemplo: 45°30.25’N.

- **`[0-9]+:`** - `[0-9]`: Corresponde a qualquer dígito de 0 a 9 e `+`:
  Indica que o dígito pode aparecer uma ou mais vezes.

- **`°:`** - Corresponde ao símbolo de graus (°). Esse símbolo é
  literalmente procurado na string.

- **`[0-9]+:`** - `[0-9]`: Corresponde a qualquer dígito de 0 a 9 e `+`:
  Indica que o dígito pode aparecer uma ou mais vezes.

- **`(\\.[0-9]+)?:`** - `\\.`: O ponto (.) é um caractere especial em
  regex, então usamos `\\.` para representar o ponto literal (como em
  `30.25`).  
  `[0-9]+`: Captura uma sequência de números após o ponto.  
  `?`: Indica que o grupo anterior (o ponto e os números após ele) é
  opcional. Ou seja, pode ou não existir.

- **`':`** - Corresponde ao símbolo de minutos (`'`). Também é um
  caractere literal.

- **`[NSWE]:`** - `[NSWE]`: Corresponde a um dos caracteres `N`, `S`,
  `W` ou `E`, que indicam a direção da coordenada (Norte, Sul, Oeste ou
  Leste).

``` r
# Função para identificar o formato da coordenada
identify_format <- function(coord) {
  # Verifica se é graus, minutos e segundos (GMS)
  if (str_detect(coord, "[0-9]+°[0-9]+'[0-9]+(\\.[0-9]+)?\"[NSWE]")) {
    return("GMS")
  }
  # Verifica se é graus decimais
  else if (str_detect(coord, "[0-9]+(\\.[0-9]+)?°?[NSWE]")) {
    return("Decimal")
  }
  # Verifica se é graus e minutos decimais
  else if (str_detect(coord, "[0-9]+°[0-9]+(\\.[0-9]+)?'[NSWE]")) {
    return("GrausMinutosDecimais")
  }
  # Caso não seja reconhecido
  else {
    return("Unknown")
  }
}

# Adiciona colunas para identificar o formato de latitude e longitude
dfc <- dfc |> 
  dplyr::mutate(
    Lat_Format  = sapply(Lat, identify_format),
    Long_Format = sapply(Long, identify_format)
  )

# Separa os dados em dataframes distintos com base no formato das coordenadas
df_gms                    <- dfc |> dplyr::filter(Lat_Format == "GMS" & Long_Format == "GMS")
df_decimal                <- dfc |> dplyr::filter(Lat_Format == "Decimal" & Long_Format == "Decimal")
df_graus_minutos_decimais <- dfc |> dplyr::filter(Lat_Format == "GrausMinutosDecimais" & Long_Format == "GrausMinutosDecimais")
df_Unknown                <- dfc |> dplyr::filter(Lat_Format == "Unknown" & Long_Format == "Unknown")
```

``` r
# Checa numero de linhas no dataframes distintos
nrow(dft)
```

    ## [1] 362

``` r
nrow(dfc)
```

    ## [1] 359

``` r
nrow(df_gms)
```

    ## [1] 275

``` r
nrow(df_decimal)
```

    ## [1] 62

``` r
nrow(df_graus_minutos_decimais)
```

    ## [1] 22

``` r
nrow(df_Unknown)
```

    ## [1] 0

``` r
print(nrow(df_gms) +nrow(df_decimal) +nrow(df_graus_minutos_decimais))
```

    ## [1] 359

##### **4. CONVERSÃO PARA GRAUS DECIMAIS —————————————–**

``` r
# Função para converter graus, minutos e segundos (GMS) para graus decimais
convert_gms <- function(coord) {
  parts   <- str_match(coord, "([0-9]+)°([0-9]+)'([0-9]+(\\.[0-9]+)?)\"([NSWE])")
  degrees <- as.numeric(parts[1, 2])
  minutes <- as.numeric(parts[1, 3])
  seconds <- as.numeric(parts[1, 4])
  direction <- parts[1, 6]

  decimal <- degrees + minutes / 60 + seconds / 3600
  if (direction %in% c("S", "W")) {
    decimal <- -decimal
  }
  return(decimal)
}

# Função para converter graus decimais (já no formato correto)
convert_decimal <- function(coord) {
  parts <- str_match(coord, "([0-9]+(\\.[0-9]+)?)[°º]?\\s*([NSWE])")
  degrees <- as.numeric(parts[1, 2])
  direction <- parts[1, 4]
  
  decimal <- degrees
  if (direction %in% c("S", "W")) {
    decimal <- -decimal
  }
  return(decimal)
}

# Função para converter graus e minutos decimais para graus decimais
convert_graus_minutos_decimais <- function(coord) {
  parts <- str_match(coord, "([0-9]+)°\\s*([0-9]+(\\.[0-9]+)?)'\\s*([NSWE])")
  degrees <- as.numeric(parts[1, 2])
  minutes <- as.numeric(parts[1, 3])
  direction <- parts[1, 5]

  decimal <- degrees + minutes / 60
  if (direction %in% c("S", "W")) {
    decimal <- -decimal
  }
  return(decimal)
}

# Aplica as funções de conversão aos dataframes correspondentes
df_gms <- df_gms |> 
  dplyr::mutate(
    Lat = sapply(Lat, convert_gms),
    Long = sapply(Long, convert_gms)
  )

df_decimal <- df_decimal |> 
  dplyr::mutate(
    Lat = sapply(Lat, convert_decimal),
    Long = sapply(Long, convert_decimal)
  )

df_graus_minutos_decimais <- df_graus_minutos_decimais |>
  dplyr::mutate(
    Lat = sapply(Lat, convert_graus_minutos_decimais),
    Long = sapply(Long, convert_graus_minutos_decimais)
  )

# Combina os dataframes convertidos em um único dataframe final
dff <- bind_rows(df_gms, df_decimal, df_graus_minutos_decimais)
```

##### **5. EXPORTAÇÃO DOS DADOS CORRIGIDOS —————————————**

``` r
names(dft)
```

    ##  [1] "Reference" "Sample_ID" "Lat"       "Long"      "SiO2"      "MgO"       "TiO2"     
    ##  [8] "Al2O3"     "Fe2O3t"    "MnO"       "CaO"       "Na2O"      "K2O"       "P2O5"     
    ## [15] "LOI"       "Fe2O3"     "FeO"       "Ba"        "Rb"        "Sr"        "Y"        
    ## [22] "Zr"        "Ti"

``` r
names(dff)
```

    ## [1] "Sample_ID"   "Lat"         "Long"        "Lat_Format"  "Long_Format"

``` r
# Renomear colunas em dff
dff <- dff |> 
  dplyr::rename(LATf = Lat, LONGf = Long) 

# Renomear colunas em dft
dft <- dft |> 
  dplyr::rename(LATi = Lat, LONGi = Long)

names(dft)
```

    ##  [1] "Reference" "Sample_ID" "LATi"      "LONGi"     "SiO2"      "MgO"       "TiO2"     
    ##  [8] "Al2O3"     "Fe2O3t"    "MnO"       "CaO"       "Na2O"      "K2O"       "P2O5"     
    ## [15] "LOI"       "Fe2O3"     "FeO"       "Ba"        "Rb"        "Sr"        "Y"        
    ## [22] "Zr"        "Ti"

``` r
names(dff)
```

    ## [1] "Sample_ID"   "LATf"        "LONGf"       "Lat_Format"  "Long_Format"

``` r
dff <- inner_join(dff, dft, by = "Sample_ID")
names(dff)
```

    ##  [1] "Sample_ID"   "LATf"        "LONGf"       "Lat_Format"  "Long_Format" "Reference"  
    ##  [7] "LATi"        "LONGi"       "SiO2"        "MgO"         "TiO2"        "Al2O3"      
    ## [13] "Fe2O3t"      "MnO"         "CaO"         "Na2O"        "K2O"         "P2O5"       
    ## [19] "LOI"         "Fe2O3"       "FeO"         "Ba"          "Rb"          "Sr"         
    ## [25] "Y"           "Zr"          "Ti"

``` r
dff <- dff |> dplyr::select(-c("Lat_Format", "Long_Format")) |> 
  mutate(LATtemp = LATf, LONGtemp = LONGf)
```

``` r
# Final CRS
## The Brazilian Institute of Geography and Statistics (IBGE) recommends the Albers Equivalent Projection 
## with SIRGAS2000 horizontal datum for the preservation and calculation of areas in Brazilian territory
## Reference: 'Informações técnicas e legais para a utilização dos dados publicados' (IBGE, 2023) 
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
terra::writeVector(vect_ibge, "grau_min_seg_final.shp", overwrite = TRUE)
```
