## Lab 3 - Computación Distribuida con Apache Spark

### Abstract
Este laboratorio extiende el lector de feeds del laboratorio 2 para que soporte "big data" en el cómputo de entidades nombradas utilizando Apache Spark.

### Requisitos

- **Java 17**.
- **Apache Spark 3.5.1**: descargar desde [aquí](https://dlcdn.apache.org/spark/spark-3.5.1/spark-3.5.1-bin-hadoop3.tgz).
- **Hadoop 3**: incluido en la descarga anterior.
- **Maven**: descargar con ``$ sudo apt install maven``.

### Compilar

```
$ cd lab03-repository
$ mvn install
$ mvn clean package
$ $PATH_TO_SPARK/spark-3.5.1-bin-hadoop3/bin/spark-submit --class App --master local[2] target/spark-project-1.0-SNAPSHOT.jar 
```

Para más detaller leer **INFORME.md**