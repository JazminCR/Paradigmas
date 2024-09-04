package namedEntities;

import org.apache.spark.SparkConf;
import scala.Tuple2;
import org.apache.spark.api.java.JavaPairRDD;
import org.apache.spark.api.java.JavaRDD;
import org.apache.spark.sql.SparkSession;
import org.apache.spark.api.java.JavaSparkContext;

import feed.Article;
import namedEntities.heuristics.NamedEntityHeuristic;
import namedEntities.heuristics.HeuristicFactory;
import java.io.IOException;
import utils.Config;
import utils.StatisticsComputer;
import java.io.FileWriter;
import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import java.util.stream.Collectors;

public class ComputeEntities {

    public void computeNamedEntities(Config config, List<Article> allArticles) {

        // crear big data
        try (FileWriter writer = new FileWriter("big_data.txt")) {
            for (Article article : allArticles) {
                writer.write(article.getTitle() + "\n");
                writer.write(article.getDescription() + "\n");
            }
            writer.close();
            System.out.println("Big data written to big_data.txt");
        } catch (IOException e) {
            System.out.println("Error writing big data");
            System.exit(1);
        }

        // configurar Spark
        SparkConf conf = new SparkConf()
                .setAppName("NewsEntityProcessor");
        JavaSparkContext sc = new JavaSparkContext(conf);

        // rutas de entrada, salida y diccionario con las entidades
        String inputFile = "big_data.txt";
        String outputDir = "output";
        String dictionaryFile = "src/main/java/data/dictionary.json";

        // capturar tiempo de inicio
        long startTime = System.currentTimeMillis();
        
        // leer el archivo de texto (big_data.txt)
        JavaRDD<String> lines = sc.textFile(inputFile);

        // llenar el diccionario con las entidades conocidas
        Map<String, NamedEntity> entityDictionary = new HashMap<>();
        try {
            entityDictionary = DictionaryReader.readDictionary(dictionaryFile);
        } catch (IOException e) {
            e.printStackTrace();
            System.exit(1);
        }

        // clasificador de entidades
        EntityClassifier classifier = new EntityClassifier(entityDictionary);
        // heurística
        String heuristicName = config.getHeuristicKey();
        System.out.println("Configured heuristic: " + heuristicName);
        NamedEntityHeuristic heuristic = HeuristicFactory.getHeuristic(heuristicName);

        if (heuristic == null) {
            System.out.println("Heuristic not found\nPlease rerun with the -h option to see the available heuristics");
            return;
        }

        // procesar cada línea para extraer y clasificar entidades nombradas
        JavaRDD<NamedEntity> classifiedEntities = lines.flatMap(iterator -> {
            List<String> articlesList = Arrays.asList(iterator.split("\n"));

            return articlesList.stream().flatMap(article -> {
                List<String> candidates = heuristic.extractCandidates(article);
                List<NamedEntity> entities = classifier.classifyEntities(candidates);

                return entities.stream();
            }).iterator();

        }).filter(entity -> entity != null);

        // reducir las entidades para contar las ocurrencias globales

        JavaPairRDD<String, NamedEntity> pairedEntitiesRDD = classifiedEntities.mapToPair(entity ->
        new Tuple2<>(entity.getLabel().toLowerCase(), entity));

        JavaPairRDD<String, NamedEntity> reducedEntitiesRDD = pairedEntitiesRDD.reduceByKey((entity1, entity2) -> {
            entity1.incrementOcurrence();
            return entity1;
        });

        Map<String, NamedEntity> globalEntitiesMap = reducedEntitiesRDD.collectAsMap();

        // iterar sobre las entradas del mapa e imprimir cada una
        for (Map.Entry<String, NamedEntity> entry : globalEntitiesMap.entrySet()) {
            System.out.println(entry.getValue().toString());
        }

        // convertir entidades a String antes de guardar y procesar para estadísticas
        List<String> entitiesList = globalEntitiesMap.values().stream()
                .map(NamedEntity::toString)
                .collect(Collectors.toList());

        JavaRDD<String> classifiedEntitiesStrings = sc.parallelize(entitiesList);

        // guardar los resultados en un archivo de salida
        classifiedEntitiesStrings.saveAsTextFile(outputDir + "/classified_entities");

        // computar estadísticas
        if (config.getComputeStats()) {
            System.out.println("Computing statistics...");
            if (config.getStatsKey().equals("cat")) {
                JavaPairRDD<String, Integer> categoryStats = classifiedEntitiesStrings.flatMapToPair(entity -> {
                    String[] parts = entity.split(",");
                    String category = parts[1].substring(parts[1].indexOf(":") + 1).trim();
                    return Arrays.asList(new Tuple2<>(category, 1)).iterator();
                }).reduceByKey(Integer::sum);
                categoryStats.foreach(tuple -> System.out.println(tuple._1 + ": " + tuple._2));
                categoryStats.saveAsTextFile(outputDir + "/category_stats");
            } else if (config.getStatsKey().equals("topic")) {
                JavaPairRDD<String, Integer> topicStats = classifiedEntitiesStrings.flatMapToPair(entity -> {
                    List<String> topics = Arrays.asList(entity.split(",")[2].split(":")[1].split(";"));
                    List<Tuple2<String, Integer>> pairs = new ArrayList<>();
                    for (String topic : topics) {
                        pairs.add(new Tuple2<>(topic, 1));
                    }
                    return pairs.iterator();
                }).reduceByKey(Integer::sum);
                topicStats.foreach(tuple -> System.out.println(tuple._1 + ": " + tuple._2));
                topicStats.saveAsTextFile(outputDir + "/topic_stats");
            } else {
                System.out.println("Invalid stats key");
            }
        } else {
            System.out.println("If you want to compute statistics, rerun with the -sf option\n");
        }

        // capturar tiempo de finalización
        long endTime = System.currentTimeMillis();
        // calcular tiempo de ejecución
        long executionTime = (endTime - startTime) / 1000;
        System.out.println("EXECUTION TIME: " + executionTime + " sec");

        // cerrar el contexto de Spark
        sc.close();
    }
}