
---

title: Computación Distribuida con Apache Spark

author: Caruso Rojo, Jazmín & Ibañez Wulbrandt, Martina & Mazzaforte Juan.

---

El enunciado del laboratorio se encuentra en [este link](https://docs.google.com/document/d/1N6V8fjBbDrCGEHfxVkHVa5kZuO8i9pnSlU5cM3VUAQM/edit#heading=h.xe9t6iq9fo58).

# 1. Tareas

- [x] Crear la big data escribiendo en un archivo los artículos (title + description) de todos los feeds.

- [x] Montar un Cluster de Spark con un master y dos workers en forma local.

- [x] Computar entidades nombradas utilizando la api de Spark para Java.

- [x] Imprimir por pantalla las entidades nombradas que ocurren en el big data.

# 2. Introducción

## Spark

Apache Spark es un motor de análisis unificado y potente que sirve para procer datos a gran escala. Empresas como Netflix, Amazon, NASA, TripAdvisor y eBay usan Spark para procesar petabytes de datos en clústeres de más de 8,000 nodos.

Algunas ***características*** de Apache Spark son:

* **Velocidad:** Spark es 100 veces más rápido que otras tecnologías porque guarda los datos intermedios en memoria, evitando lecturas y escrituras en disco. Incluso cuando se usa el disco, sigue siendo 10 veces más rápido que tecnologías como Hadoop.
* **Soporte Multilenguaje:** Ofrece APIs de alto nivel en Scala, Java, Python y R, lo que permite a los programadores trabajar con datos masivos y procesarlos a gran escala.
* **Componentes de Analítica Avanzada:** Spark soporta MapReduce, incluye componentes para consultas SQL, aprendizaje automático y algoritmos de grafos, satisfaciendo las necesidades tecnológicas actuales.

***Modelo de Ejecución de Spark: Arquitectura Master-Slave***

Spark sigue la arquitectura master-slave. El nodo maestro se llama "driver" y los nodos esclavos se llaman "executors".
Cuando se ejecuta una aplicación en Spark, se crea un driver y varios executors que procesan la aplicación.

* **Driver de Spark:** El driver es responsable de analizar, distribuir, programar y monitorear el trabajo de los executors.
* **Executor de Spark:** Los executors son los trabajadores principales. Ejecutan las tareas asignadas por el driver y devuelven los resultados.

***Modos de Despliegue de Apache Spark***

* **Modo Cliente:** El driver se inicia en la máquina local, pero los executors corren en clústeres.
* **Modo Clúster:** Tanto los drivers como los executors corren en clústeres. 
* **Modo Local:** Todo se ejecuta en una única JVM. Este modo no es muy práctico y se usa principalmente para depuración y desarrollo.

## Maven

Apache Maven es una herramienta muy potente de gestión de proyectos que se utiliza para gestionar dependencias, como herramienta de compilación e incluso como herramienta de documentación. Es de código abierto y gratuita. Se puede utilizar con diversos lenguajes como C#, Ruby, Scala, pero se usa principalmente en proyectos **Java**.

Maven utiliza convenciones sobre dónde colocar ciertos archivos para el proceso de _build_ de un proyecto, por lo que solo tenemos que establecer las excepciones, esto nos simplifica mucho el trabajo. Además, es una **herramienta declarativa**. Es decir, todo lo que definamos se almacena en un archivo XML que Maven lee a la hora de funcionar.

***¿Qué es el archivo pom.xml?***

La unidad básica de trabajo en Maven es el llamado **Modelo de Objetos de Proyecto** conocido simplemente como **POM** (de sus siglas en inglés: _Project Object Model_).
Se trata de un archivo XML llamado `pom.xml` que se encuentra por defecto en la raíz de los proyectos y que **contiene toda la información del proyecto**: su configuración y dependencias.

***¿Qué podemos hacer con Maven?***
-   **Gestionar las dependencias** del proyecto, para descargar e instalar módulos, paquetes y herramientas que sean necesarios para el mismo.
-   **Compilar el código fuente** de la aplicación de manera automática.
-   **Empaquetar el código** en archivos .jar o .zip. *(para esto lo utilizamos nosotros en el proyecto)*
-   **Instalar los paquetes** en un repositorio 
-   **Generar documentación** a partir del código fuente.
-   **Gestionar las distintas fases del ciclo de vida** de las _build_: validación, generación de código fuente, procesamiento, generación de recursos, compilación, ejecución de test.

# 3. Cambios realizados

**Modularizamos** el contenido de *App.java*, todo lo que se trataba sobre computar entidades ahora se encuentra en *ComputeEntites.java*.

Maven impone una estructura de directorio estándar para los proyectos, la cual es: *src/main/java/...*. En el proyecto anterior nosotros teníamos *src/...*. Por lo tanto tuvimos que **modificar las rutas** para leer los feeds y el diccionario. Además de incluir un archivo *pom.xml* para el manejo de dependencias.

En *ComputeEntities.java* implementamos lo nuevo del proyecto: computar entidades nombradas de un archivo de texto en forma distribuida mediante Spark.

**Modificamos las clases** *DictionaryReader*, *EntityClassifier*, *NamedEntity* para que implementen la interfaz *Serializable* y así puedan ser serializadas y enviadas a los nodos.

Primero **creamos el archivo de big data** con el contenido de los títulos y descripciones de los feeds indicados. Este es el archivo que va a consumir el programa para realizar el cómputo de las estadísticas. A los resultados obtenidos se los va a guardar en el directorio *output* (además de imprimirlos en la terminal).

Luego **configuramos Spark y lo usamos** para leer el archivo de entrada en un RDD de líneas. Cargamos el diccionario y creamos una instancia para clasificar entidades. Obtenemos el nombre de la heurística configurada y usamos *flatMap* para procesar cada línea del RDD, dividiéndolas en artículos. Para cada artículo se extraen candidatos y se clasifican. Después convertimos esas entidades ya clasificadas en pares de etiquetas y entidades para contar las ocurrencias de cada una. Para mostrar el resultado coleccionamos las entidades en un mapa global y las imprimimos. Para mayor comodidad de lectura convertimos las entidades en una lista de cadenas y las guardamos en el directorio de salida. Computamos las estadísticas usando *reduceByKey* para contar las ocurrencias de categorías o tópicos según corresponda (el resultado se imprime y guarda en archivos). Finalmente se cierra el contexto de Spark.

**Agregamos un contador** EXECUTION TIME que inicia cuando se empieza a leer la big data y finaliza luego de imprimir los resultados. Aunque para mayor precisión es conveniente utilizar el comando *time* al principio de la linea cuando se vaya a ejecutar el programa.

El lab2 utilizaba una api de geolocalización para obtener las coordenadas de las entidades con categoría "Localidad". En este laboratorio quitamos esa funcionalidad porque hacía que la ejecución del programa sea más lenta.

## Resumen, uso de Spark

- Lectura del archivo de texto en un RDD.
- Extracción y clasificación de entidades.
- Mapeo a pares.
- Reducción por clave para contar ocurrencias.
- Paralelización de la lista de entidades clasificadas.
- Guardar resultados en un archivo de salida.
- Cómputo de estadísticas.

# 4. Pasos a seguir para ejecutar el programa

Recordar tener instalado lo especificado en **README.md**

**Modo clúster**: la aplicación se ejecutará en un entorno distribuido utilizando los recursos de los workers conectados al master.

**1) Crear un Master con 2 workers**:

- ir al directorio *$PATH_TO_SPARK/spark-3.5.1-bin-hadoop3/conf*

- allí crear un archivo llamado *spark-env.sh* con el siguiente contenido:

```
SPARK_WORKER_CORES=1

SPARK_WORKER_INSTANCES=2

SPARK_WORKER_MEMORY=1g
```

- luego inicializar el Master con:

```
$ $PATH_TO_SPARK/spark-3.5.1-bin-hadoop3/sbin/start-master.sh
```

- ir al sitio *localhost:8080*, allí se podrá encontrar una url del estilo *spark://$LOCALHOST:7077*.

- para inicializar los workers:

```
$ $PATH_TO_SPARK/spark-3.5.1-bin-hadoop3/sbin/start-worker.sh spark://$LOCALHOST:7077
```

- en el sitio se podrá observar el estado de los mismos.

**2) Compilar**:

Asegurarnos de estar en el directorio *lab03-repository*

```
$ mvn clean install

$ $PATH_TO_SPARK/spark-3.5.1-bin-hadoop3/bin/spark-submit --class App --master spark://$LOCALHOST:7077 target/spark-project-1.0-SNAPSHOT.jar $FLAGS
```

**3) Cerrar clúster**:

```
$ $PATH_TO_SPARK/spark-3.5.1-bin-hadoop3/sbin/stop-master.sh
$ $PATH_TO_SPARK/spark-3.5.1-bin-hadoop3/sbin/stop-worker.sh
```

**Modo local**: Spark utiliza cierta cantidad de hilos (en este caso 2) en la máquina local para ejecutar la aplicación:

**1) Compilar**:

Asegurarnos de estar en el directorio *lab03-repository*

```
$ mvn clean install

$ $PATH_TO_SPARK/spark-3.5.1-bin-hadoop3/bin/spark-submit --class App --master local[2] target/spark-project-1.0-SNAPSHOT.jar $FLAGS 
```

**Aclaración**: con el último comando (ya sea clúster o local) deben ir las FLAGS necesarias para que el programa realice las tareas. Por ejemplo, para que calcule el cómputo de las entidades de todos los feeds con *AllWordsHeuristic* y estadísticas por tópicos:

```
$ $PATH_TO_SPARK/spark-3.5.1-bin-hadoop3/bin/spark-submit --class App --master ... target/spark-project-1.0-SNAPSHOT.jar -ne AllWordsHeuristic -sf topic
```

# 5. Evaluación

**Archivo de 26,2 kB (26.160 bytes)**

Para probar el proyecto primero lo corrimos en modo local, pero al hacerlo ya sea dándole 1, 2 o 4 hilos siempre demoraba el mismo tiempo. Deducimos que esto ocurre debido a que en una máquina local los múltiples hilos compiten por los mismos recursos.

Entonces probamos con el modo clúster, iniciamos el master y los workers, y obtuvimos los siguientes resultados:

EXECUTION TIME
|workers |    1    |     2     |     4     |
|--------|---------|-----------|-----------|
|sec     |    14   |    23     |     30    |

Esto ocurre ya que en un clúster, la comunicación entre los workers introduce latencia. Es decir que mientras más trabajadores haya, más comunicación se requiere y más esfuerzo para el master en gestionar, coordinar y dividir las tareas, lo cual causa un aumento en el tiempo de ejecución debido a la sobrecarga y sincronización.
Al hacer la prueba con un archivo pequeño no se puede apreciar el trabajo dividido de los workers.

**Archivos más grandes**

Realicé la prueba con archivos de mayor tamaño y sucedió lo mismo, demora más con más workers :( no sé si se debe a que el archivo todavía no es lo suficientemente grande o si en la implementación del código hay algo que deba ser diferente.

# 6. Extras

Comparamos el rendimiento entre el lab2 y el lab3 computando las entidades de todos los feeds utilizando *AllWordsHeuristic* e imprimiento las estadísticas según *cat*, y los resultados fueron los siguientes:

- Lab2:
![lab2](https://github.com/JazminCR/graficos_lab3/blob/main/allWords_lab2.png?raw=true)
EXECUTION TIME = 0sec

- Lab3:
![lab3](https://github.com/JazminCR/graficos_lab3/blob/main/allWords_lab3.png?raw=true)
EXECUTION TIME = 6sec

En este caso los datos a procesar no son demasiados. El lab2 lo hace más rápido porque simplemente tiene que computar las entidades. En cambio, el lab3, además de realizar el cómputo, tiene que dividir las tareas y coordinarlas con los workers lo cual produce sobrecarga.

Si el archivo a procesar fuera más grande podría verse la ventaja de utilizar el lab3. En un caso así, al lab2 le costaría más computar, mientras que en el lab3 la sobrecarga se recompensa con el trabajo distribuido.

# 7. Experiencia y conclusión

Durante el desarrollo de este proyecto, aprender sobre Apache Spark y la programación distribuida fue interesante pero a medida que profundizamos en la implementación y funcionamiento de esta tecnología surgieron ciertos desafíos.

No poder ver en detalle las tareas y procesos ejecutados por cada worker dificulta la comprensión del rendimiento y eficacia del programa.

Además, ya que el aprendizaje corre por nuestra cuenta, es decir, leer la documentación y descubrir/investigar cómo se van haciendo las cosas, no podemos asegurar que funcione de manera óptima, quizás podríamos haber ido por otro camino o tomar otras decisiones. Creemos que nos encontramos limitados a la hora de evaluar si el proyecto funciona a su máximo potencial o determinar si existe una alternativa más adecuada.

Si bien está bueno conocer nuevas plataformas y tecnologías, esta vez nos resultó un poco más complicado. Esperamos que se hayan cumplido las expectativas y sentado una base para futuros proyectos con programación distribuida.
