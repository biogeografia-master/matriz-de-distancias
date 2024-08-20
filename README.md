Generación de la matriz de distancias
================
Biogeografía (GEO-131)
2024-08-20

Versión HTML (quizá más legible),
[aquí](https://biogeografia-master.github.io/matriz-de-distancias/README.html)

## Introducción

En este ejercicio, nos enfocaremos en la **Generación de la Matriz de
Distancias** entre puntos en un espacio bidimensional. La matriz de
distancias es una herramienta crucial en la geografía, biogeografía y
análisis espacial, ya que permite cuantificar la proximidad entre
diferentes puntos de interés. Como métrica usaremos la distancia
euclidiana.

## Teoría

La distancia entre dos puntos $(x_i, y_i)$ y $(x_j, y_j)$ se puede
calcular de múltiples formas. Nosotros utilizaremos la distancia
euclidiana:

$$
d_{ij} = \sqrt{(x_i - x_j)^2 + (y_i - y_j)^2}
$$

La matriz de distancias es una matriz cuadrada donde cada elemento
$d_{ij}$ representa la distancia entre los puntos $i$ y $j$.

## Ejercicio

### Planteamiento del Problema

Consideremos tres puntos $A$, $B$ y $C$ en un plano bidimensional. Los
puntos $A$ y $B$ estarán cercanos entre sí, mientras que el punto $C$
estará alejado de ambos. Las coordenadas de estos puntos son:

- Punto $A$: $(x_A, y_A) = (2, 2)$
- Punto $B$: $(x_B, y_B) = (3, 3)$
- Punto $C$: $(x_C, y_C) = (10, 10)$

Queremos calcular la matriz de distancias para estos tres puntos.

### Demostración con código reproducible en R

Primero, definimos las coordenadas de los puntos y calculamos las
distancias entre ellos:

``` r
# Coordenadas de los puntos
x_A <- 2; y_A <- 2
x_B <- 3; y_B <- 3
x_C <- 10; y_C <- 10

# Crear una matriz para almacenar las coordenadas
coords <- matrix(c(x_A, y_A, x_B, y_B, x_C, y_C), ncol = 2, byrow = TRUE)
rownames(coords) <- c("A", "B", "C")
colnames(coords) <- c("X", "Y")

# Función para calcular la matriz de distancias
distance_matrix <- function(coords) {
  dist(coords)
}

# Calcular la matriz de distancias
distances <- distance_matrix(coords)
as.matrix(distances)
```

    ##           A        B         C
    ## A  0.000000 1.414214 11.313708
    ## B  1.414214 0.000000  9.899495
    ## C 11.313708 9.899495  0.000000

La matriz de distancias se compone del triángulo inferior, el triángulo
superior y la diagonal principal. La diagonal principal contiene ceros,
ya que representa la distancia de un punto consigo mismo. En este caso,
por tratarse de distancia euclidiana, los triángulos superior e inferior
son simétricos entre sí, ya que la distancia entre dos puntos es la
misma independientemente de la dirección en la que se mida.

## Tu turno

### Parte 1. Calcula una matriz de distancias

Mandato:

1.  Elige un conjunto de los que aparecen abajo, poniéndote de acuerdo
    con tus compañeros y compañeros para evitar duplicidad.

2.  Calcula la matriz de distancias entre tres puntos de coordenadas
    $X, Y$ diferentes.

3.  Si puedes (si tienes lápiz) aplica un sombreado intenso a las
    distancias diagonal principal, un sombreado moderado a las
    distancias intermedias y sin sombreado a las distancias grandes. Si
    tienes lápices de distintos colores, podrías usar el tono para
    representar las distintas distancias (aunque esto es
    semiológicamente delicado).

> Podría servirte el representar los tres puntos de tu conjunto elegido
> en un plano cartesiano. Así podrás visualizar mejor su distribución.

``` r
library(knitr)
library(dplyr)

# Número de conjuntos
num_sets <- 20

# Función para generar una tabla para cada conjunto
generate_points <- function(set_id) {
  
  # Generar coordenadas enteras para los puntos A, B, C
  points <- data.frame(
    `ID Punto` = c("A", "B", "C"),
    X = c(sample(1:5, 2), sample(8:10, 1)),
    Y = c(sample(1:5, 2), sample(8:10, 1)),
    check.names = F
  )
  
  # Crear una lista que combine el encabezado con la tabla
  output <- list(
    paste0("**Conjunto #", set_id, "**"),
    kable(points, align = "c")
  )
  
  return(list(points, output))
}

# Generar las 20 tablas estableciendo la semilla para reproducibilidad
set.seed(123); points_list <- lapply(
  1:num_sets, function(x) generate_points(x)[[2]])
set.seed(123); points_df <- lapply(
  1:num_sets, function(x) generate_points(x)[[1]])

# Imprimir las tablas
for (table in points_list) {
  cat(table[[1]], "\n\n")
  print(table[[2]])
  cat("\n\n")
}
```

**Conjunto \#1**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  3  |  2  |
|    B     |  2  |  5  |
|    C     | 10  |  9  |

**Conjunto \#2**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  3  |  1  |
|    B     |  1  |  2  |
|    C     |  9  | 10  |

**Conjunto \#3**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  5  |  1  |
|    B     |  3  |  4  |
|    C     | 10  |  8  |

**Conjunto \#4**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  1  |  2  |
|    B     |  5  |  3  |
|    C     | 10  |  9  |

**Conjunto \#5**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  1  |  4  |
|    B     |  2  |  2  |
|    C     | 10  |  8  |

**Conjunto \#6**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  3  |  4  |
|    B     |  5  |  3  |
|    C     |  8  |  9  |

**Conjunto \#7**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  5  |  1  |
|    B     |  3  |  2  |
|    C     |  8  | 10  |

**Conjunto \#8**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  3  |  5  |
|    B     |  4  |  3  |
|    C     |  8  |  9  |

**Conjunto \#9**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  1  |  5  |
|    B     |  2  |  4  |
|    C     |  8  | 10  |

**Conjunto \#10**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  5  |  1  |
|    B     |  2  |  3  |
|    C     |  8  |  8  |

**Conjunto \#11**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  5  |  2  |
|    B     |  3  |  4  |
|    C     |  8  | 10  |

**Conjunto \#12**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  4  |  3  |
|    B     |  2  |  4  |
|    C     |  9  |  9  |

**Conjunto \#13**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  1  |  1  |
|    B     |  2  |  2  |
|    C     |  9  |  8  |

**Conjunto \#14**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  5  |  3  |
|    B     |  2  |  1  |
|    C     | 10  |  9  |

**Conjunto \#15**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  1  |  1  |
|    B     |  4  |  3  |
|    C     |  9  | 10  |

**Conjunto \#16**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  4  |  3  |
|    B     |  3  |  2  |
|    C     |  8  |  8  |

**Conjunto \#17**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  3  |  2  |
|    B     |  5  |  3  |
|    C     |  9  |  8  |

**Conjunto \#18**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  5  |  2  |
|    B     |  4  |  5  |
|    C     | 10  |  9  |

**Conjunto \#19**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  4  |  4  |
|    B     |  2  |  5  |
|    C     |  9  |  9  |

**Conjunto \#20**

| ID Punto |  X  |  Y  |
|:--------:|:---:|:---:|
|    A     |  1  |  3  |
|    B     |  2  |  4  |
|    C     |  9  |  9  |

### Función para calcular la matriz de distancias y generar un mapa de calor con `ggplot2`

``` r
library(ggplot2)
library(reshape2)

calculate_distance_matrix <- function(x_coords, y_coords, title) {
  coords <- cbind(x_coords, y_coords)
  rownames(coords) <- LETTERS[1:nrow(coords)]
  colnames(coords) <- c("X", "Y")
  dist_matrix <- as.matrix(dist(coords))
  
  # Convertir la matriz de distancias a formato largo para ggplot2
  dist_long <- melt(dist_matrix)
  colnames(dist_long) <- c("Punto1", "Punto2", "Distancia")
  
  # Crear el mapa de calor usando ggplot2
  heatmap_plot <- ggplot(dist_long, aes(x = Punto1, y = Punto2, fill = Distancia)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "blue") +
    geom_text(aes(label = sprintf("%.2f", Distancia)), color = "black", size = 4) +
    theme_minimal() +
    labs(title = paste0("Mapa de Calor de la Matriz de Distancias (Conjunto ", title, ")"),
         x = "Punto",
         y = "Punto",
         fill = "Distancia")
  
  print(heatmap_plot)
  
  return(dist_matrix)
}
```

### Aplicar la función para calcular la matriz de distancias y generar un mapa de calor para todos los conjuntos

``` r
distance_matrices <- lapply(1:20, function(conjunto) {
  print(paste0("Conjunto ", conjunto))
  calculate_distance_matrix(
    x_coords = points_df[[conjunto]]$X,
    y_coords = points_df[[conjunto]]$Y,
    title = conjunto)
})
names(distance_matrices) <- paste0('Conjunto ', 1:20)
distance_matrices
```

### Parte 2. Biometría básica

Mandato:

1.  Rellena [este
    formulario](https://docs.google.com/forms/d/e/1FAIpQLSe2vi6we4tZBG3jxb0v7SrTrJRwbngt3VEuFgjXxrowNnxRZA/viewform?usp=sharing).

<img src="qr.jpg" style="width:35.0%" />

2.  Cuando haya varias respuestas en línea, para no complicarlo, elige
    sólo 3 conjunto de datos (es decir, mediciones de 3 estudiantes), y
    sólo dos variables (e.g. mediciones de longitud de meñique y pulgar,
    pero no tienes que elegir necesariamente estos dos, pueden ser
    otros, lo importante es que sólo sean dos para simplificar) que
    utilizarás como coordenadas X e Y. Los datos se alojarán en esta
    [hoja de
    cálculo](https://docs.google.com/spreadsheets/d/14JrVEx-oKtIsGCDFh049DDtoh7o4FQJWFuDWNYS4nfk/edit?usp=sharing).

3.  Genera la mariz de distancias.

4.  Interpreta el resultado. Formula preguntas basándote en los
    resultados obtenidos. No olvides que estás trabajando con rasgos
    biométricos de personas; las distancias son número “insensibles”,
    pero están expresando algo sobre la biometría de las personas
    analizadas. Por ejemplo, ¿Qué significado tienen las distancias
    pequeñas (si las hubiere) en el contexto analizado? ¿Qué significan
    las distancias muy grandes?

5.  Bonus. Con suerte, empeño e inteligencia artificial, intenta
    reproducirlo en R, ya sea en mi servidor (si tienes acceso), o en
    [rdrr.io](https://rdrr.io/snippets/). Para ello, necesitarás dos
    cosas:

    1.  Ejecutar todo el código de R que se encuentra arriba. La mejor
        manera es clonar este repositorio
        (<https://github.com/biogeografia-master/matriz-de-distancias.git>)
        y ejecutarlo desde mi servidor o desde tu PC, si tienes R
        instalado. Para clonar puedes usar el botón verde `Code` que se
        encuentra en la página del repositorio. Con RStudio, podrás
        hacerlo también, usando `New Project`. Más detalles en el aula.
    2.  Pasarle los datos al intérprete de R, ya sea mediante un archivo
        (en mi servidor) o creando un `data.frame` directamente en
        rdrr.io con algo como esto:

``` r
datos <- data.frame(conjunto = c(AQUÍ VAN LOS NOMBRES/PSEUDÓNIMOS DE LAS PERSONAS SEPARADOS POR COMAS),
                    x = c(AQUÍ VAN LAS MEDICIONES DE UNO DE LOS DEDOS ELEGIDOS),
                    Y = c(AQUÍ VAN LAS MEDICIONES DEL OTRO DEDO ELEGIDO))

calculate_distance_matrix(
    x_coords = datos$x,
    y_coords = datos$x,
    title = datos$conjunto)
```
