
#### 🌍 ─────────────────────────────────────────────── 🌍

#### 🌍 THERESA ROCCO PEREIRA BARBOSA

#### 🌍 Brasileira \| Geocientista \| Dados

#### 🌍 <imakemapas@outlook.com.br> \| +55 24 998417085

#### 🌍 ─────────────────────────────────────────────── 🌍

#### **INTEGRAÇÃO DE DADOS CIENTÍFICOS LEGADOS: PADRONIZAÇÃO DE COORDENADAS GEOGRÁFICAS**

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
erros específicos, como coordenadas mal formatadas. O objeto final é
salvo como um shapefile, garantindo que os dados espaciais integrados
estejam prontos para análises geográficas complexas e mapeamentos.

O fluxo trabalho é dividido nas etapas:

1.  Carregamento e limpeza dos dados
2.  Padronização de símbolos e formatos
3.  Identificação do formato das coordenadas
4.  Conversão para graus decimais
5.  Exportação dos dados corrigidos

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
dft <- readxl::read_excel("tabela_grau_min_seg.xlsx")

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
# Checa numero de linhas nos dataframes distintos
nrow(df_Unknown)
```

    ## [1] 0

``` r
nrow(dfc)
```

    ## [1] 359

``` r
print(nrow(df_gms) +nrow(df_decimal) + nrow(df_graus_minutos_decimais))
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

    ##  [1] "Reference" "Sample_ID" "Lat"       "Long"      "SiO2"      "MgO"       "TiO2"      "Al2O3"     "Fe2O3t"    "MnO"      
    ## [11] "CaO"       "Na2O"      "K2O"       "P2O5"      "LOI"       "Fe2O3"     "FeO"       "Ba"        "Rb"        "Sr"       
    ## [21] "Y"         "Zr"        "Ti"

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

    ##  [1] "Reference" "Sample_ID" "LATi"      "LONGi"     "SiO2"      "MgO"       "TiO2"      "Al2O3"     "Fe2O3t"    "MnO"      
    ## [11] "CaO"       "Na2O"      "K2O"       "P2O5"      "LOI"       "Fe2O3"     "FeO"       "Ba"        "Rb"        "Sr"       
    ## [21] "Y"         "Zr"        "Ti"

``` r
names(dff)
```

    ## [1] "Sample_ID"   "LATf"        "LONGf"       "Lat_Format"  "Long_Format"

``` r
dff <- inner_join(dff, dft, by = "Sample_ID")
names(dff)
```

    ##  [1] "Sample_ID"   "LATf"        "LONGf"       "Lat_Format"  "Long_Format" "Reference"   "LATi"        "LONGi"       "SiO2"       
    ## [10] "MgO"         "TiO2"        "Al2O3"       "Fe2O3t"      "MnO"         "CaO"         "Na2O"        "K2O"         "P2O5"       
    ## [19] "LOI"         "Fe2O3"       "FeO"         "Ba"          "Rb"          "Sr"          "Y"           "Zr"          "Ti"

``` r
dff <- dff |> dplyr::select(-c("Lat_Format", "Long_Format")) |> 
  dplyr::mutate(LATtemp = LATf, LONGtemp = LONGf)
str(dff)
```

    ## tibble [359 × 27] (S3: tbl_df/tbl/data.frame)
    ##  $ Sample_ID: chr [1:359] "CS900" "CS843" "CB10" "LR02A" ...
    ##  $ LATf     : Named num [1:359] -27 -27 -28.1 -29.6 -29.5 ...
    ##   ..- attr(*, "names")= chr [1:359] "26°59'33.32\"S" "26°59'33.32\"S" "28º8'10\"S" "29°35'51.14\"S" ...
    ##  $ LONGf    : Named num [1:359] -51.3 -51.3 -49.5 -51.4 -51.4 ...
    ##   ..- attr(*, "names")= chr [1:359] "51°15'11.84\"W" "51°15'11.84\"W" "49º28'50\"W" "51°21'26.64\"W" ...
    ##  $ Reference: chr [1:359] "Peate and Hawkesworth, 1996" "Peate and Hawkesworth, 1996" "Acosta et al., 2023" "Rossetti et al. 2014" ...
    ##  $ LATi     : chr [1:359] "26°59'33.32\"S" "26°59'33.32\"S" "28º8'10\"S" "29°35'51.14\"S" ...
    ##  $ LONGi    : chr [1:359] "51°15'11.84\"W" "51°15'11.84\"W" "49º28'50\"W" "51°21'26.64\"W" ...
    ##  $ SiO2     : num [1:359] 54.4 53.2 52.9 55.9 55.6 ...
    ##  $ MgO      : num [1:359] 5.02 5.32 4.65 3.85 4.08 7.14 8.62 8.77 3.53 4.2 ...
    ##  $ TiO2     : num [1:359] 1.36 1.3 1.4 1.3 1.33 1.17 1.07 1.06 1.4 1.31 ...
    ##  $ Al2O3    : num [1:359] 13.9 14.1 13.6 14.4 14.3 ...
    ##  $ Fe2O3t   : num [1:359] 12.2 12.8 13.1 10.2 10.4 ...
    ##  $ MnO      : num [1:359] 0.19 0.18 0.19 0.13 0.15 0.17 0.16 0.17 0.15 0.14 ...
    ##  $ CaO      : num [1:359] 8.75 9.12 8.49 6.38 7.11 9.81 9.71 9.69 6.47 6.69 ...
    ##  $ Na2O     : num [1:359] 2.66 2.63 2.64 2.82 2.63 2.03 1.82 1.88 2.76 2.91 ...
    ##  $ K2O      : num [1:359] 1.37 1.23 0.97 2.65 2.34 1.06 0.92 0.96 2.54 2.37 ...
    ##  $ P2O5     : num [1:359] 0.18 0.19 0.18 0.28 0.29 0.16 0.14 0.14 0.3 0.28 ...
    ##  $ LOI      : chr [1:359] "0.4" "0.43" "1.7" "1.8" ...
    ##  $ Fe2O3    : num [1:359] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ FeO      : logi [1:359] NA NA NA NA NA NA ...
    ##  $ Ba       : num [1:359] 365 327 308 619 585 256 233 240 627 608 ...
    ##  $ Rb       : num [1:359] 49 44 32.5 88 82 32 28 27 92 86 ...
    ##  $ Sr       : num [1:359] 220 217 243 273 275 ...
    ##  $ Y        : num [1:359] 32 29 29.5 28 27 22 19 20 33 30 ...
    ##  $ Zr       : num [1:359] 149 139 153 211 203 ...
    ##  $ Ti       : num [1:359] 8153 7794 8393 7794 7974 ...
    ##  $ LATtemp  : Named num [1:359] -27 -27 -28.1 -29.6 -29.5 ...
    ##   ..- attr(*, "names")= chr [1:359] "26°59'33.32\"S" "26°59'33.32\"S" "28º8'10\"S" "29°35'51.14\"S" ...
    ##  $ LONGtemp : Named num [1:359] -51.3 -51.3 -49.5 -51.4 -51.4 ...
    ##   ..- attr(*, "names")= chr [1:359] "51°15'11.84\"W" "51°15'11.84\"W" "49º28'50\"W" "51°21'26.64\"W" ...
    ##  - attr(*, "na.action")= 'exclude' Named int [1:3] 110 258 271
    ##   ..- attr(*, "names")= chr [1:3] "110" "258" "271"

``` r
dff <- dff |> 
  mutate(
    LOI = as.numeric(LOI),
    FeO = as.numeric(FeO)
    )
str(dff)
```

    ## tibble [359 × 27] (S3: tbl_df/tbl/data.frame)
    ##  $ Sample_ID: chr [1:359] "CS900" "CS843" "CB10" "LR02A" ...
    ##  $ LATf     : Named num [1:359] -27 -27 -28.1 -29.6 -29.5 ...
    ##   ..- attr(*, "names")= chr [1:359] "26°59'33.32\"S" "26°59'33.32\"S" "28º8'10\"S" "29°35'51.14\"S" ...
    ##  $ LONGf    : Named num [1:359] -51.3 -51.3 -49.5 -51.4 -51.4 ...
    ##   ..- attr(*, "names")= chr [1:359] "51°15'11.84\"W" "51°15'11.84\"W" "49º28'50\"W" "51°21'26.64\"W" ...
    ##  $ Reference: chr [1:359] "Peate and Hawkesworth, 1996" "Peate and Hawkesworth, 1996" "Acosta et al., 2023" "Rossetti et al. 2014" ...
    ##  $ LATi     : chr [1:359] "26°59'33.32\"S" "26°59'33.32\"S" "28º8'10\"S" "29°35'51.14\"S" ...
    ##  $ LONGi    : chr [1:359] "51°15'11.84\"W" "51°15'11.84\"W" "49º28'50\"W" "51°21'26.64\"W" ...
    ##  $ SiO2     : num [1:359] 54.4 53.2 52.9 55.9 55.6 ...
    ##  $ MgO      : num [1:359] 5.02 5.32 4.65 3.85 4.08 7.14 8.62 8.77 3.53 4.2 ...
    ##  $ TiO2     : num [1:359] 1.36 1.3 1.4 1.3 1.33 1.17 1.07 1.06 1.4 1.31 ...
    ##  $ Al2O3    : num [1:359] 13.9 14.1 13.6 14.4 14.3 ...
    ##  $ Fe2O3t   : num [1:359] 12.2 12.8 13.1 10.2 10.4 ...
    ##  $ MnO      : num [1:359] 0.19 0.18 0.19 0.13 0.15 0.17 0.16 0.17 0.15 0.14 ...
    ##  $ CaO      : num [1:359] 8.75 9.12 8.49 6.38 7.11 9.81 9.71 9.69 6.47 6.69 ...
    ##  $ Na2O     : num [1:359] 2.66 2.63 2.64 2.82 2.63 2.03 1.82 1.88 2.76 2.91 ...
    ##  $ K2O      : num [1:359] 1.37 1.23 0.97 2.65 2.34 1.06 0.92 0.96 2.54 2.37 ...
    ##  $ P2O5     : num [1:359] 0.18 0.19 0.18 0.28 0.29 0.16 0.14 0.14 0.3 0.28 ...
    ##  $ LOI      : num [1:359] 0.4 0.43 1.7 1.8 1.5 1.7 1.5 1.1 0.7 1.6 ...
    ##  $ Fe2O3    : num [1:359] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ FeO      : num [1:359] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Ba       : num [1:359] 365 327 308 619 585 256 233 240 627 608 ...
    ##  $ Rb       : num [1:359] 49 44 32.5 88 82 32 28 27 92 86 ...
    ##  $ Sr       : num [1:359] 220 217 243 273 275 ...
    ##  $ Y        : num [1:359] 32 29 29.5 28 27 22 19 20 33 30 ...
    ##  $ Zr       : num [1:359] 149 139 153 211 203 ...
    ##  $ Ti       : num [1:359] 8153 7794 8393 7794 7974 ...
    ##  $ LATtemp  : Named num [1:359] -27 -27 -28.1 -29.6 -29.5 ...
    ##   ..- attr(*, "names")= chr [1:359] "26°59'33.32\"S" "26°59'33.32\"S" "28º8'10\"S" "29°35'51.14\"S" ...
    ##  $ LONGtemp : Named num [1:359] -51.3 -51.3 -49.5 -51.4 -51.4 ...
    ##   ..- attr(*, "names")= chr [1:359] "51°15'11.84\"W" "51°15'11.84\"W" "49º28'50\"W" "51°21'26.64\"W" ...
    ##  - attr(*, "na.action")= 'exclude' Named int [1:3] 110 258 271
    ##   ..- attr(*, "names")= chr [1:3] "110" "258" "271"

``` r
summary(dff)
```

    ##   Sample_ID              LATf            LONGf         Reference             LATi              LONGi                SiO2      
    ##  Length:359         Min.   :-33.43   Min.   :-56.90   Length:359         Length:359         Length:359         Min.   :46.39  
    ##  Class :character   1st Qu.:-29.48   1st Qu.:-52.30   Class :character   Class :character   Class :character   1st Qu.:51.12  
    ##  Mode  :character   Median :-29.13   Median :-51.19   Mode  :character   Mode  :character   Mode  :character   Median :53.20  
    ##                     Mean   :-28.49   Mean   :-51.43                                                            Mean   :56.90  
    ##                     3rd Qu.:-28.70   3rd Qu.:-50.32                                                            3rd Qu.:65.84  
    ##                     Max.   :-18.54   Max.   :-29.37                                                            Max.   :70.06  
    ##                                                                                                                               
    ##       MgO             TiO2           Al2O3           Fe2O3t            MnO              CaO              Na2O           K2O       
    ##  Min.   :0.330   Min.   :0.650   Min.   :11.73   Min.   : 5.060   Min.   :0.0500   Min.   : 0.460   Min.   :1.57   Min.   :0.450  
    ##  1st Qu.:1.400   1st Qu.:0.950   1st Qu.:12.76   1st Qu.: 7.045   1st Qu.:0.1100   1st Qu.: 3.170   1st Qu.:2.34   1st Qu.:1.020  
    ##  Median :4.190   Median :1.210   Median :13.44   Median :10.920   Median :0.1700   Median : 7.950   Median :2.58   Median :1.770  
    ##  Mean   :3.778   Mean   :1.418   Mean   :13.62   Mean   :10.562   Mean   :0.1568   Mean   : 6.812   Mean   :2.55   Mean   :2.253  
    ##  3rd Qu.:5.450   3rd Qu.:1.605   3rd Qu.:14.35   3rd Qu.:13.085   3rd Qu.:0.1900   3rd Qu.: 9.360   3rd Qu.:2.78   3rd Qu.:3.800  
    ##  Max.   :8.830   Max.   :4.280   Max.   :17.01   Max.   :16.420   Max.   :0.2600   Max.   :11.380   Max.   :3.92   Max.   :6.010  
    ##                                                                                                                                   
    ##       P2O5              LOI             Fe2O3            FeO            Ba              Rb               Sr        
    ##  Min.   : 0.1000   Min.   :-0.110   Min.   :12.99   Min.   : NA   Min.   :108.0   Min.   :  8.00   Min.   :  40.0  
    ##  1st Qu.: 0.1800   1st Qu.: 1.085   1st Qu.:13.87   1st Qu.: NA   1st Qu.:323.0   1st Qu.: 29.00   1st Qu.: 152.5  
    ##  Median : 0.2400   Median : 1.600   Median :13.88   Median : NA   Median :465.0   Median : 58.00   Median : 207.0  
    ##  Mean   : 0.3891   Mean   : 1.863   Mean   :13.96   Mean   :NaN   Mean   :463.9   Mean   : 86.79   Mean   : 229.8  
    ##  3rd Qu.: 0.2700   3rd Qu.: 2.200   3rd Qu.:14.06   3rd Qu.: NA   3rd Qu.:613.0   3rd Qu.:159.45   3rd Qu.: 251.0  
    ##  Max.   :52.0000   Max.   : 8.400   Max.   :15.03   Max.   : NA   Max.   :948.0   Max.   :306.00   Max.   :1180.0  
    ##                                     NA's   :353     NA's   :359                                                    
    ##        Y                Zr              Ti           LATtemp          LONGtemp     
    ##  Min.   : 17.00   Min.   : 72.0   Min.   : 3897   Min.   :-33.43   Min.   :-56.90  
    ##  1st Qu.: 27.00   1st Qu.:137.5   1st Qu.: 5516   1st Qu.:-29.48   1st Qu.:-52.30  
    ##  Median : 33.00   Median :189.0   Median : 6715   Median :-29.13   Median :-51.19  
    ##  Mean   : 35.59   Mean   :192.1   Mean   : 7553   Mean   :-28.49   Mean   :-51.43  
    ##  3rd Qu.: 39.00   3rd Qu.:241.6   3rd Qu.: 8393   3rd Qu.:-28.70   3rd Qu.:-50.32  
    ##  Max.   :152.00   Max.   :596.0   Max.   :25060   Max.   :-18.54   Max.   :-29.37  
    ##                                   NA's   :62

``` r
str(dff)
```

    ## tibble [359 × 27] (S3: tbl_df/tbl/data.frame)
    ##  $ Sample_ID: chr [1:359] "CS900" "CS843" "CB10" "LR02A" ...
    ##  $ LATf     : Named num [1:359] -27 -27 -28.1 -29.6 -29.5 ...
    ##   ..- attr(*, "names")= chr [1:359] "26°59'33.32\"S" "26°59'33.32\"S" "28º8'10\"S" "29°35'51.14\"S" ...
    ##  $ LONGf    : Named num [1:359] -51.3 -51.3 -49.5 -51.4 -51.4 ...
    ##   ..- attr(*, "names")= chr [1:359] "51°15'11.84\"W" "51°15'11.84\"W" "49º28'50\"W" "51°21'26.64\"W" ...
    ##  $ Reference: chr [1:359] "Peate and Hawkesworth, 1996" "Peate and Hawkesworth, 1996" "Acosta et al., 2023" "Rossetti et al. 2014" ...
    ##  $ LATi     : chr [1:359] "26°59'33.32\"S" "26°59'33.32\"S" "28º8'10\"S" "29°35'51.14\"S" ...
    ##  $ LONGi    : chr [1:359] "51°15'11.84\"W" "51°15'11.84\"W" "49º28'50\"W" "51°21'26.64\"W" ...
    ##  $ SiO2     : num [1:359] 54.4 53.2 52.9 55.9 55.6 ...
    ##  $ MgO      : num [1:359] 5.02 5.32 4.65 3.85 4.08 7.14 8.62 8.77 3.53 4.2 ...
    ##  $ TiO2     : num [1:359] 1.36 1.3 1.4 1.3 1.33 1.17 1.07 1.06 1.4 1.31 ...
    ##  $ Al2O3    : num [1:359] 13.9 14.1 13.6 14.4 14.3 ...
    ##  $ Fe2O3t   : num [1:359] 12.2 12.8 13.1 10.2 10.4 ...
    ##  $ MnO      : num [1:359] 0.19 0.18 0.19 0.13 0.15 0.17 0.16 0.17 0.15 0.14 ...
    ##  $ CaO      : num [1:359] 8.75 9.12 8.49 6.38 7.11 9.81 9.71 9.69 6.47 6.69 ...
    ##  $ Na2O     : num [1:359] 2.66 2.63 2.64 2.82 2.63 2.03 1.82 1.88 2.76 2.91 ...
    ##  $ K2O      : num [1:359] 1.37 1.23 0.97 2.65 2.34 1.06 0.92 0.96 2.54 2.37 ...
    ##  $ P2O5     : num [1:359] 0.18 0.19 0.18 0.28 0.29 0.16 0.14 0.14 0.3 0.28 ...
    ##  $ LOI      : num [1:359] 0.4 0.43 1.7 1.8 1.5 1.7 1.5 1.1 0.7 1.6 ...
    ##  $ Fe2O3    : num [1:359] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ FeO      : num [1:359] NA NA NA NA NA NA NA NA NA NA ...
    ##  $ Ba       : num [1:359] 365 327 308 619 585 256 233 240 627 608 ...
    ##  $ Rb       : num [1:359] 49 44 32.5 88 82 32 28 27 92 86 ...
    ##  $ Sr       : num [1:359] 220 217 243 273 275 ...
    ##  $ Y        : num [1:359] 32 29 29.5 28 27 22 19 20 33 30 ...
    ##  $ Zr       : num [1:359] 149 139 153 211 203 ...
    ##  $ Ti       : num [1:359] 8153 7794 8393 7794 7974 ...
    ##  $ LATtemp  : Named num [1:359] -27 -27 -28.1 -29.6 -29.5 ...
    ##   ..- attr(*, "names")= chr [1:359] "26°59'33.32\"S" "26°59'33.32\"S" "28º8'10\"S" "29°35'51.14\"S" ...
    ##  $ LONGtemp : Named num [1:359] -51.3 -51.3 -49.5 -51.4 -51.4 ...
    ##   ..- attr(*, "names")= chr [1:359] "51°15'11.84\"W" "51°15'11.84\"W" "49º28'50\"W" "51°21'26.64\"W" ...
    ##  - attr(*, "na.action")= 'exclude' Named int [1:3] 110 258 271
    ##   ..- attr(*, "names")= chr [1:3] "110" "258" "271"

``` r
summary(dff)
```

    ##   Sample_ID              LATf            LONGf         Reference             LATi              LONGi                SiO2      
    ##  Length:359         Min.   :-33.43   Min.   :-56.90   Length:359         Length:359         Length:359         Min.   :46.39  
    ##  Class :character   1st Qu.:-29.48   1st Qu.:-52.30   Class :character   Class :character   Class :character   1st Qu.:51.12  
    ##  Mode  :character   Median :-29.13   Median :-51.19   Mode  :character   Mode  :character   Mode  :character   Median :53.20  
    ##                     Mean   :-28.49   Mean   :-51.43                                                            Mean   :56.90  
    ##                     3rd Qu.:-28.70   3rd Qu.:-50.32                                                            3rd Qu.:65.84  
    ##                     Max.   :-18.54   Max.   :-29.37                                                            Max.   :70.06  
    ##                                                                                                                               
    ##       MgO             TiO2           Al2O3           Fe2O3t            MnO              CaO              Na2O           K2O       
    ##  Min.   :0.330   Min.   :0.650   Min.   :11.73   Min.   : 5.060   Min.   :0.0500   Min.   : 0.460   Min.   :1.57   Min.   :0.450  
    ##  1st Qu.:1.400   1st Qu.:0.950   1st Qu.:12.76   1st Qu.: 7.045   1st Qu.:0.1100   1st Qu.: 3.170   1st Qu.:2.34   1st Qu.:1.020  
    ##  Median :4.190   Median :1.210   Median :13.44   Median :10.920   Median :0.1700   Median : 7.950   Median :2.58   Median :1.770  
    ##  Mean   :3.778   Mean   :1.418   Mean   :13.62   Mean   :10.562   Mean   :0.1568   Mean   : 6.812   Mean   :2.55   Mean   :2.253  
    ##  3rd Qu.:5.450   3rd Qu.:1.605   3rd Qu.:14.35   3rd Qu.:13.085   3rd Qu.:0.1900   3rd Qu.: 9.360   3rd Qu.:2.78   3rd Qu.:3.800  
    ##  Max.   :8.830   Max.   :4.280   Max.   :17.01   Max.   :16.420   Max.   :0.2600   Max.   :11.380   Max.   :3.92   Max.   :6.010  
    ##                                                                                                                                   
    ##       P2O5              LOI             Fe2O3            FeO            Ba              Rb               Sr        
    ##  Min.   : 0.1000   Min.   :-0.110   Min.   :12.99   Min.   : NA   Min.   :108.0   Min.   :  8.00   Min.   :  40.0  
    ##  1st Qu.: 0.1800   1st Qu.: 1.085   1st Qu.:13.87   1st Qu.: NA   1st Qu.:323.0   1st Qu.: 29.00   1st Qu.: 152.5  
    ##  Median : 0.2400   Median : 1.600   Median :13.88   Median : NA   Median :465.0   Median : 58.00   Median : 207.0  
    ##  Mean   : 0.3891   Mean   : 1.863   Mean   :13.96   Mean   :NaN   Mean   :463.9   Mean   : 86.79   Mean   : 229.8  
    ##  3rd Qu.: 0.2700   3rd Qu.: 2.200   3rd Qu.:14.06   3rd Qu.: NA   3rd Qu.:613.0   3rd Qu.:159.45   3rd Qu.: 251.0  
    ##  Max.   :52.0000   Max.   : 8.400   Max.   :15.03   Max.   : NA   Max.   :948.0   Max.   :306.00   Max.   :1180.0  
    ##                                     NA's   :353     NA's   :359                                                    
    ##        Y                Zr              Ti           LATtemp          LONGtemp     
    ##  Min.   : 17.00   Min.   : 72.0   Min.   : 3897   Min.   :-33.43   Min.   :-56.90  
    ##  1st Qu.: 27.00   1st Qu.:137.5   1st Qu.: 5516   1st Qu.:-29.48   1st Qu.:-52.30  
    ##  Median : 33.00   Median :189.0   Median : 6715   Median :-29.13   Median :-51.19  
    ##  Mean   : 35.59   Mean   :192.1   Mean   : 7553   Mean   :-28.49   Mean   :-51.43  
    ##  3rd Qu.: 39.00   3rd Qu.:241.6   3rd Qu.: 8393   3rd Qu.:-28.70   3rd Qu.:-50.32  
    ##  Max.   :152.00   Max.   :596.0   Max.   :25060   Max.   :-18.54   Max.   :-29.37  
    ##                                   NA's   :62

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
terra::writeVector(vect_ibge, "grau_min_seg_final.shp", overwrite = TRUE)
```
