# Datos proporcionados FUERA DE TEMPORADA 
eliminacion_hatching <- c(0.0042, 0.0126, 0.0084, 0.0000, 0.0000, 0.0005, 0.0000)
utas_entrega <- c(372, 387.5, 381, 393, 390, 379, 378)
mortalidad_eclosion <- c(0.0025, 0.0018, 0.0053, 0.0016, 0.0015, 0.0035, 0.0002)
mortalidad_start_feeding <- c(0.0598, 0.0160, 0.0222, 0.0238, 0.0214, 0.0325, 0.0151)



# Función para calcular el intervalo de confianza del 95%
calcular_intervalo_confianza <- function(datos) {
  n <- length(datos)
  media <- mean(datos)
  error_estandar <- sd(datos) / sqrt(n)
  intervalo <- qt(0.90, df=n-1) * error_estandar
  return(c(media - intervalo, media + intervalo))
}

# Calcular los intervalos de confianza para ambos conjuntos de datos
intervalo_confianza_eclosion <- calcular_intervalo_confianza(mortalidad_eclosion)
intervalo_confianza_start_feeding <- calcular_intervalo_confianza(mortalidad_start_feeding)
ic_utas <- calcular_intervalo_confianza(utas_entrega)
ic_eliminacion <- calcular_intervalo_confianza(eliminacion_hatching)

# Convertir a porcentajes
intervalo_confianza_eclosion_porcentaje <- intervalo_confianza_eclosion * 100
intervalo_confianza_start_feeding_porcentaje <- intervalo_confianza_start_feeding * 100
ic_eliminacion_porcentaje <- ic_eliminacion * 100


# Crear una tabla con los resultados
library(dplyr)

resultados <- data.frame(
  Metricas = c("Mortalidad Eclosion", "Mortalidad Start Feeding","Utas Entregas","Eliminacion"),
  Media = c(mean(mortalidad_eclosion) * 100, mean(mortalidad_start_feeding) * 100, mean(utas_entrega), mean(eliminacion_hatching) * 100),
  IC_Lower = c(intervalo_confianza_eclosion_porcentaje[1], intervalo_confianza_start_feeding_porcentaje[1], ic_utas[1], ic_eliminacion_porcentaje[1]),
  IC_Upper = c(intervalo_confianza_eclosion_porcentaje[2], intervalo_confianza_start_feeding_porcentaje[2], ic_utas[2], ic_eliminacion_porcentaje[2])
)

# Mostrar la tabla
print(resultados)
