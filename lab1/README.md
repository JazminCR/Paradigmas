---
title: Laboratorio de Funcional
author: Jazmín Caruso Rojo, Martina Ibañez Wulbrandt, Juan Mazzaforte.
---
La consigna del laboratorio está en https://tinyurl.com/funcional-2024-famaf

# 1. Tareas
Pueden usar esta checklist para indicar el avance.

## Verificación de que pueden hacer las cosas.
- [x] Haskell instalado y testeos provistos funcionando. (En Install.md están las instrucciones para instalar.)

## 1.1. Lenguaje
- [x] Módulo `Dibujo.hs` con el tipo `Dibujo` y combinadores. Puntos 1 a 3 de la consigna.
- [x] Definición de funciones (esquemas) para la manipulación de dibujos.
- [x - punto extra] Módulo `Pred.hs`. Punto extra si definen predicados para transformaciones innecesarias (por ejemplo, espejar dos veces es la identidad).

## 1.2. Interpretación geométrica
- [x] Módulo `Interp.hs`.

## 1.3. Expresión artística (Utilizar el lenguaje)
- [x] El dibujo de `Dibujos/Feo.hs` se ve lindo.
- [x] Módulo `Dibujos/Grilla.hs`.
- [x] Módulo `Dibujos/Escher.hs`.
- [x + bonus] Listado de dibujos en `Main.hs`.

## 1.4 Tests
- [x] Tests para `Dibujo.hs`.
- [x] Tests para `Pred.hs`.

# 2. Experiencia
En este proyecto trabajamos con la programación funcional `Haskell`, lo cual hacía bastante no utilizabamos, al principio nos costó acostumbrarnos a la sintaxis y semántica del lenguaje pero al tiempo pudimos llevarlo bien y entender su funcionamiento, por ejemplo, cómo estructurar el código, cómo manejar las funciones de alto orden, entre otros.
Utilizamos la biblioteca de gráficos `Gloss`, la cual desconocíamos. Nos encontramos con el desafío de familiarizarnos con ella, para lo cual leímos su documentación oficial e hicimos varios ejemplos prácticos para comprenderla. Al final nos pareció muy interesante y divertido su uso. 
Al proyecto lo desarrollamos siguiendo paso a paso el enunciado, comenzamos por el módulo `Dibujo.hs` donde definimos el tipo principal para hacer dibujos, y funciones básicas para poder operar con los dibujos. Seguimos en el archivo `Pred.hs` definiendo funciones (predicados) sin hacer pattern-matching. 
Una vez que terminamos con la sintaxis de nuestro lenguaje, pasamos a la interpretación, para ello trabjamos en el módulo `Interp.hs` donde importamos la librería `gloss` y escribimos la semántica de cada operación de nuestro lenguaje.
Teniendo estos dos importantes pasos listos, pasamos a lo más divertido del proyecto, utilizar el lenguaje!
Primero corroboramos que el `Dibujos/Feo.hs` se dibuje correctamente. Luego realizamos nuestras propias creaciones. Empezamos por `Dibujos/Grilla.hs` que se trata de una grilla numerada de 8x8. Seguimos con `Dibujos/Escher.hs` que fue el más complejo, para ello leímos el artículo recomendado y realizamos muchas figuras de pruebas hasta dar con las correctas. Para finalizar quisimos probar otras cosas, como funciones que no habíamos utilizado o mostrar figuras rellenas de color, esto puede apreciarse en `Dibujos/Patron.hs` y `Dibujos/Cadenas.hs`.
Modificamos el main para que al enviar el comando `cabal run dibujos -- --lista` imprima en pantalla los nombres de todos los dibujos registrados y pregunte cuál mostrar.
Por último, agregamos test para `Dibujo.hs` y `Pred.hs`, estos pueden verse en la carpeta test. Para realizarlos utilizamos `GitHub Copilot` y `HUnit` que proporciona una forma de escribir y ejecutar pruebas unitarias para verificar el comportamiento de las funciones y módulos de Haskell. Sobre estas herramientas tampoco conocíamos y nos resultó muy práctica y útil su incorporación.
Como grupo, hicimos la mayor parte posible del proyecto en las horas de clases pero se nos complicaba a la hora de ejecutar el proyecto ya que no contamos con la posibilidad de llevar nuestra computadora a clases. Por lo tanto nos organizamos para terminar el proyecto en nuestras casas. La colaboración en equipo fue algo fundamental para lograr los objetivos.
En conclusión, fue un proyecto desafiante pero súper interesante en el cual pudimos ver buenos resultados, aprender mucho sobre la programación funcional y desarrollar nuevas habilidades técnicas.

# 3. Preguntas

## 1. ¿Por qué están separadas las funcionalidades en los módulos indicados? Explicar detalladamente la responsabilidad de cada módulo.
Los módulos están separados de acuerdo a su responsabilidad en el proyecto. Cada uno tiene una función claramente definida y se encarga de aspectos específicos del programa, lo que facilita sobretodo la comprensión del código. Además, modularizando el trabajo se promueve la reutilización del código y se facilita su mantenimiento.

### Dibujo.hs
Acá se define la estructura de datos `Dibujo` y las funciones relacionadas con la manipulación de dibujos básicos y composiciones de dibujos. Al principio definimos el tipo Dibujo, con el cual luego podremos crear dibujos complejos. Tenemos funciones constructoras como `figura`, `encimar`, `apilar`, `juntar`, `rotar`, `rot45`, `espejar`, y funciones como rotaciones, superposiciones y composiciones. (`^^^`, `.-.`, `///`, `r90`, `r180`, `r270`), además de funciones que combinan los constructores del principio, como `encimar4`, `cuarteto`, `ciclar`
También tenemos la función `change`, `mapDib` y `foldDib` que nos ayudan un montón en otras partes del proyecto y nos ahorra escribir código repetitivo. 

### Pred.hs
Este módulo se encarga de definir y trabajar con predicados. Acá definimos el tipo `Pred` para representar un predicado sobre los elementos de un dibujo. También implementamos una función para manipular dibujos a partir de predicados, como `cambiar`, dos funciones para verificar si un predicado se cumple para alguna o todas las figuras de un dibujo,`anyDib`, `allDib`, y otras dos funciones que sirven para realizar operaciones lógicas entre predicados: `orP`, y `andP`.

### Interp.hs
Módulo encargado de la interpretación y visualización de los dibujos en una pantalla. Definimos funciones para interpretar los elementos de un dibujo y representarlos visualmente, como `interp`, `ov`, `r45`, `rot`, `esp`, `sup`, `jun`, y `api`.
Utilizamos operaciones con vectores para poder trabajar con las coordenadas que utiliza Gloss para poder representar los dibujos.

### Main.hs
Este módulo se encarga de la lógica principal del proyecto. Se definió una lista de configuraciones de dibujos disponibles y fueron proporcionadas funciones para seleccionar y mostrar un dibujo específico. También se maneja la interacción con el usuario a través de la línea de comandos, permitiendo seleccionar un dibujo de la lista de opciones disponibles.

## 2. ¿Por qué las figuras básicas no están incluidas en la definición del lenguaje, y en vez de eso, es un parámetro del tipo?
Las figuras básicas no están incluidas directamente en la definición del lenguaje ya que esto le permite a éste adaptarse a distintas figuras y facilita la adición de nuevas formas si así se quisiera. Si no, estaríamos limitados a las figuras que hayan sido definidas únicamente dentro del lenguaje.

## 3. ¿Qué ventaja tiene utilizar una función de `fold` sobre hacer pattern-matching directo?
Fold es una función de orden superior que procesa una estructura de datos en algún orden y construye un valor de retorno. Su ventaja principal es que permite aplicar una función a cada elemento de forma más compacta, mientras que pattern-matching es más explicito y detallado, y se necesita especificar todos los posibles casos de patrones, lo cual lleva a un código más extenso, verboso y propenso a errores ya que no es fácil de entender.

## 4. ¿Cuál es la diferencia entre los predicados definidos en Pred.hs y los tests?
La diferencia entre los predicados definidos en `Pred.hs` y los tests está en su propósito y aplicación.
En Pred.hs, los predicados se definen como funciones que toman un elemento y devuelven un valor booleano, utilizados para operaciones como cambiar figuras que satisfacen cierto criterio, comprobar si alguna o todas las figuras cumplen un predicado, o combinar predicados, como nombramos anteriormente. 
Por otro lado, los tests están diseñados para probar la funcionalidad de estas funciones, verificando si producen los resultados esperados en diferentes casos de prueba. 
Mientras que los predicados en Pred.hs forman parte del código base del sistema y se utilizan para operaciones específicas sobre dibujos, los tests se centran en asegurar que esas operaciones funcionen correctamente en diferentes escenarios.

# 4. Extras
Creamos otras dos figuras.
- El dibujo de `Dibujos/Patron.hs` se trata de repetir la misma figura, conservando así la repetición de las formas, lo cual crea un efecto visual interesante. Lo primero que hicimos fue seleccionar un dibujo principal y darle color de relleno utilizando `polygon`, y luego lo combinamos de una manera atractiva/agradable. Por último, lo repetimos varias veces en el dibujo logrando asi el efecto deseado. La idea surgió de buscar diseños de arte, dentro de ellos encontamos los llamados "patrones repetitivos".
- El dibujo de `Dibujos/Cadenas.hs` surge de querer hacer algo abstracto y probar cosas nuevas. Se nos ocurrió crear una figura rellena combinando los distintos puntos de las coordenadas, de esta forma obtuvimos una imagen random, a la cual luego le aplicamos la función `ciclar` para convertirla en algo más abstracto. Al final también utilizamos el concepto de patrón para lograr un efecto más llamativo.
