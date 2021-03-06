# Evidencias-Estadistica-2021
Repositorio de archivos y documentos del curso de Estadística, 2021.

## Evidencia 1
Analisis de datos de la ENDUTIH sobre horas dedicadas a internet versus otras variables.
Temas: Estadística descriptiva, gráficos y contraste de hipótesis.

La Encuesta Nacional sobre Disponibilidad y Uso de Tecnologías de la Información en los Hogares (ENDUTIH) tiene como finalidad obtener información sobre la disponibilidad y el uso de las tecnologías de información y comunicaciones en los hogares y su utilización por los individuos de seis años o más en México, para generar información estadística en el tema y apoyar la toma de decisiones en cuestión de políticas públicas.

Analisis de la base de datos respecto a las variables de: edad y tiempo que dedica diariamente a Internet:

1. Para los datos en general, determina el  promedio de tiempo dedicado a Internet.
2. Para el total de datos, determina la varianza y la desviación estándar del tiempo que dedican al uso de Internet.
3. Para los datos por género, determina en promedio quién dedica más tiempo a Internet: hombres o mujeres.
4. Para los hombres, calcula el coeficiente de correlación lineal entre la edad y el tiempo dedicado al uso de Internet.
5. Para las mujeres, calcula el coeficiente de correlación lineal entre la edad y el tiempo dedicado al uso de Internet.
6. Para los datos por género: determina la mediana de la edad y del tiempo dedicado a Internet.

Prueba de hipótesis

Imagina que el promedio que dedica una persona a Internet (sin importar su género) es de 7 horas diarias. Con los datos anteriores, prueba las siguientes hipótesis:

H0: µ = 7 contra la alternativa de que Ha : µ ≠ 7 con un nivel de significancia de 0.05. Realiza todas las etapas de una prueba de hipótesis y concluye sobre el contexto del problema. ¿Es el tiempo promedio dedicado a Internet diferente a 7?

Establece e interpreta el intervalo de confianza al 95%.

Realiza un resumen de los hallazgos.

## Evidencia 2
Analisis de series de tiempo.
El análisis de series de tiempo constituye una de las herramientas más importantes dentro de la toma de decisiones. Nos ayuda a realizar pronósticos como también a evaluar impactos; por ejemplo, el análisis de series de tiempo nos permite responder a las siguientes preguntas: ¿Cuál será el crecimiento anual del PIB en 2021?, ¿Cuál es la tendencia de una variable? ¿Cómo se va a comportar en el futuro? El análisis de series de tiempo nos brinda respuestas cuantitativas a estas preguntas y más.

Para este modulo vamos a estudiar tres variables:
1. Indicador Trimestral de la Actividad Económica Estatal: Este indicador de coyuntura ofrece un panorama sobre la evolución económica de las entidades federativas del país. 
2. Indicador de la Actividad Industrial (manufactura) por entidad federativa
3. Producto Interno Bruto del sector manufacturero por entidad federativa

Instrucciones: 

Elige una de las siguientes entidades: Coahuila, Nuevo León, Jalisco, Sinaloa, Chihuahua o Baja California y realiza lo que se te pide.

1. Para el total de la actividad economica (ITAEE), realiza un pronostico de media móvil hasta el último trimestre del 2023. El ITAEE se encuentra disponible en el Banco de Información Económica del INEGI. Puedes seleccionar la serie que va desde 2008 hasta 2021 para realizar el pronóstico. 
2. Para el caso del indicador de la actividad industrial, realiza un pronóstico para el 2023. Los datos se encuentran en el Banco de Información Económica.
3. Por último, realiza un pronóstico del PIB del sector manufacturero de la entidad. 
4. Responde a las siguientes preguntas: ¿cómo se espera que se comporte la manufactura en la entidad? ¿cuál es la perspectiva a largo plazo? Realiza una reflexión del panorama económico de la entidad. 

Entregables: Archivo en Word con los ejercicios y pronósticos, códigos en R utilizados. 

## Evidencia 3
Modelos de regresión lineal múltiple: relación entre el precio de vehiculos vs otras variables.

Este ejercicio consiste en realizar un análisis de datos sobre un dataset de 100,000 vehiculos y 9 atributos. El objetivo es conseguir un modelo de regresión lineal con un resultado aceptable. Antes del analisis de regresión se pide:

1. Realizar limpieza de datos
2. Estadísticos descriptivos: ¿cuál es el precio promedio por cada una de las marcas de automoviles?, ¿cuánto es el kilometraje promedio para cada una de las marcas?, ¿cuál es la desviación estándar del precio y del kilometraje?, ¿cuál es el precio promedio para los autos electricos, los de gasolina y los de diesel?, ¿cuál es el precio promedio de los automoviles automaticos y estandar? 
3. Analisis exploratorio de datos: Generar gráficos de dispersión entre precio vs kilometraje por marca y por tipo de vehiculo, analisis de correlación entre precio y kilometraje con su respectiva interpretación, gráfico de precio a través de los años por tipo de automóvil. 
4. Analisis de regresión: Realizar todos los pasos del analisis, determinar los coeficientes, evaluar p-value, pruebas de hipotesis de los coeficientes, error estándar, R cuadrado ajustado, analisis de los residuos.

Recuerda que la evidencia debe llevar una introducción, explicar la base de datos, mencionar la metodología del analisis de regresión, estadísticos descriptivos, analisis exploratorio de los datos, modelo de regresión y resultados. 
