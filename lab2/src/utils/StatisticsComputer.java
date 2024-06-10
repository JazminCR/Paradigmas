package utils;

import namedEntities.NamedEntity;

import java.util.List;
import java.util.Map;
import java.util.HashMap;

public class StatisticsComputer {

    public static void computeCategoryStatistics(List<NamedEntity> entities) {

        Map<String, Map<String, Integer>> categoryLabelCounts = new HashMap<>();

        // Contar la ocurrencia de cada categoría y label
        for (NamedEntity entity : entities) {
            String category = entity.getCategory();
            String label = entity.getLabel();

            if (!categoryLabelCounts.containsKey(category)) {
                categoryLabelCounts.put(category, new HashMap<>());
            }

            Map<String, Integer> labelCounts = categoryLabelCounts.get(category);
            labelCounts.put(label, labelCounts.getOrDefault(label, 0) + entity.getOcurrence());
        }

        // Imprimir las estadísticas por categoría
        for (Map.Entry<String, Map<String, Integer>> categoryEntry : categoryLabelCounts.entrySet()) {
            String category = categoryEntry.getKey();
            Map<String, Integer> labelCounts = categoryEntry.getValue();

            System.out.println("Categoría: " + category);
            for (Map.Entry<String, Integer> labelEntry : labelCounts.entrySet()) {
                String label = labelEntry.getKey();
                int count = labelEntry.getValue();

                System.out.println("\t" + label + "(" + count + ")");
            }
        }

    }

    public static void computeTopicStatistics(List<NamedEntity> entities) {
        Map<String, Map<String, Integer>> topicLabelCounts = new HashMap<>();

        // Contar la ocurrencia de cada toíc y label
        for (NamedEntity entity : entities) {
            List<String> topics = entity.getTopics();
            String label = entity.getLabel();

            for (String topic : topics) {
                if (!topicLabelCounts.containsKey(topic)) {
                    topicLabelCounts.put(topic, new HashMap<>());
                }

                Map<String, Integer> labelCounts = topicLabelCounts.get(topic);
                labelCounts.put(label, labelCounts.getOrDefault(label, 0) + entity.getOcurrence());
            }
        }

        // Imprimir las estadísticas por topic
        for (Map.Entry<String, Map<String, Integer>> topicEntry : topicLabelCounts.entrySet()) {
            String topic = topicEntry.getKey();
            Map<String, Integer> labelCounts = topicEntry.getValue();

            System.out.println("Topico: " + topic);
            for (Map.Entry<String, Integer> labelEntry : labelCounts.entrySet()) {
                String label = labelEntry.getKey();
                int count = labelEntry.getValue();

                System.out.println("\t" + label + "(" + count + ")");
            }
        }
    }
}
