---
title: Laboratorio de Programación Orientada a Objetos
author: Caruso Rojo, Jazmín & Ibañez Wulbrandt, Martina & Mazzaforte Juan.
---

El enunciado del laboratorio se encuentra en [este link](https://docs.google.com/document/d/1wLhuEOjhdLwgZ4rlW0AftgKD4QIPPx37Dzs--P1gIU4/edit#heading=h.xe9t6iq9fo58).

# 1. Tareas
Pueden usar esta checklist para indicar el avance.

## Verificación de que pueden hacer las cosas.
- [x] Java 17 instalado. Deben poder compilar con `make` y correr con `make run` para obtener el mensaje de ayuda del programa.

## 1.1. Interfaz de usuario
- [x] Estructurar opciones
- [x] Construir el objeto de clase `Config`

## 1.2. FeedParser
- [x] `class Article`
    - [x] Atributos
    - [x] Constructor
    - [x] Método `print`
    - [x] _Accessors_
- [x] `parseXML`

## 1.3. Entidades nombradas
- [x] Pensar estructura y validarla con el docente
- [x] Implementarla
- [x] Extracción
    - [x] Implementación de heurísticas
- [x] Clasificación
    - [x] Por tópicos
    - [x] Por categorías
- Estadísticas
    - [x] Por tópicos
    - [x] Por categorías
    - [x] Impresión de estadísticas

## 1.4 Limpieza de código
- [x] Pasar un formateador de código
- [x] Revisar TODOs

# 2. Experiencia
En este proyecto, basado en Programación Orientada a Objetos (POO) y desarrolado en Java, logramos superar nuestra inexperiencia inicial tanto con el lenguaje como con el paradigma de programación, y encontramos la experiencia bastante agradable.
Las funcionalidades principales del proyecto incluyen: lectura de feeds, buena interfaz de usuario, identificar y clasificar entidades, cómputo de estadísticas.
La primera dificultad que enfrentamos fue en el método *ParseXML* de la clase *FeedParser* , era lo más complejo hasta el momento y requería leer bastante documentación para hacer buen uso de *DOM*.
La implementación de heurísticas resultó ser más sencilla gracias a que teníamos un ejemplo previo, sin embargo, reconocemos que no son muy robustas. Mejorarlas podría ser una mejora del proyecto, o tal como dice el enunciado, se podría utilizar una API especializada en la extracción de entidades para mayor precisión.
La extracción y clasificación de entidades fue algo complicado, para poder lograrlo tuvimos que crear varias clases adicionales, como *HeuristicFactory*, *EntityClassifier*, *DictionaryReader*, y modificar la estructura de las entidades ya que las que teniamos al principios eran muy básicas.
Luego, a cada entidad le añadimos algunas características propias. También añadimos entidades al diccionario que nos dieron *dictionary.json* para que quede un poco más completo.
Asegurar que la interfaz funcione de manera óptima para todos los casos fue laborioso, pero lo logramos. Quizás si se quiere una interfaz más precisa estaría bueno tener algunos ejemplos de lo que debe devolver en ciertos casos.
Finalmente, el cómputo de estadísticas se realizó según las categorías o tópicos. 
Como punto extra pudimos hacer uso de una API de Geolocalización para obtener información acerca de las entidades que corresponden a la categoría *Loation*.
Dado que es un tipo de proyecto que no estamos acostumbrados realizar, estamos satisfechos con los resultados obtenidos. Aprendimos mucho sobre el paradigma orientado a objetos y su implementación. Está bueno realizar proyectos que tengan una utilidad práctica y que demuestren el trabajo realizado.


# 3. Preguntas
1. Explicar brevemente la estructura de datos elegida para las entidades nombradas.

La **estructura de datos elegida para las entidades nombradas** es una clase abstracta llamada *NamedEntity*, de la cual heredan varias clases que representan diferentes categorias de entidades nombradas, estas son *Location*, *Organization*, *Person* y *Other*.

Atributos:
- *label*: representa el nombre de la entidad.
- *topics*: representa los temas asociados con la entidad.
- *ocurrence*: número de ocurrencias de la entidad.

Dentro de los métodos, tenemos:

Constructor:
- *NamedEntity(String label, List<String> topics)* que inicializa los atributos *label* y *topics*, y establece *ocurrence* en 1.

Getters:
- *getLabel()*
- *getTopics()*
- *getOcurrence()*

*incrementOcurrence()*

Abstractos: deben ser implementados por las clases concretas.
- *getCategory()*: devuelve la categoría de la entidad.
- *printSpecificInfo()*: imprime información específica de la entidad.

*toString()*: devuelve una representación en cadena de la entidad.

Luego, cada clase concreta extiende de esta clase para representar una entidad, definiendo sus propios atributos e implementando los métodos abstractos.

Elegimos esta estructura ya que nos permite aprovechar el polimorfismo, es decir, la herencia nos permite la reutilización del código y la extensión de funcionalidades.
De esta manera, cada clase concreta encapsula los datos específicos y la lógica relacionada con el tipo de entidad, manteniendo la interfaz común definida en la clase abstracta.

2. Explicar brevemente cómo se implementaron las heurísticas de extracción.

Tenemos la interfaz *NamedEntityHeuristic* que define el método *extractCandidates*, elcual toma un texto como entrada y devuelve una lista de cadena que representan las entidadas candidatas.
Cada heurística se basa en la interfaz nombrada anteriormente e implementa ese método, de manera diferente, para identificar entidades nombradas.

Las **heurísticas que implementamos** son las siguientes:

- *AcronymsHeuristic*: diseñada para extraer acrónimos del texto. Se usa una expresión regular que identifica palabras formadas por dos o más letras mayúsculas consecutivas.
- *AllWordsHeuristic*: se enfoca en extraer todas las palabras presentes en el texto. La expresión que se utiliza identifica cualquier palabra formada por más de un caracter.
- *FollowingWordHeuristic*: identifica palabras del texto que siguen a ciertas palabras específicas ("en", "de", "sr.", y "sra.").

En general, en todas se comienza con la normalización del texto, eliminando caracteres no deseados y acentos para simplificar el análisis. Luego, se usa una expresión regular que identifica las palabras deseadas. Por último, la heurística recorre el texto buscando coincidencias con ese patrón y agrega las palabras encontradas a la lista de candidatos.

# 4. Extras
Hicimos uso de una API de Geolocalización, *Nominatim*, para obtener las coordenadas (latitud y longitud) de las entidades que corresponden a la categoría *Location*.
Para poder visualizar estas características y las de las demás entidades, creamos el método *printSpecificInfo()*.