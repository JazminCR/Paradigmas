package namedEntities;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import namedEntities.category.Other;
import namedEntities.category.Location;
import namedEntities.category.Organization;
import namedEntities.category.Person;
import java.io.Serializable;

public class EntityClassifier implements Serializable {

    private Map<String, NamedEntity> dictionary;

    public EntityClassifier(Map<String, NamedEntity> dictionary) {
        this.dictionary = dictionary;
    }

    public List<NamedEntity> classifyEntities(List<String> candidates) {
        List<NamedEntity> classifiedEntities = new ArrayList<>();

        for (String candidate : candidates) {
            NamedEntity entity = dictionary.get(candidate.toLowerCase());
            if (entity != null) {
                NamedEntity classifiedEntity = getExistingEntity(classifiedEntities, entity);
                if (classifiedEntity != null) {
                    classifiedEntity.incrementOcurrence();
                } else {
                    classifiedEntities.add(entity);
                }
            } else {
                classifiedEntities.add(new Other(candidate, List.of("OTHER")));
            }
        }

        return classifiedEntities;
    }

    private NamedEntity getExistingEntity(List<NamedEntity> entities, NamedEntity entity) {
        for (NamedEntity e : entities) {
            if (e.getLabel().equals(entity.getLabel())) {
                return e;
            }
        }
        return null;
    }

    public void printEntitiesResults(List<NamedEntity> classifiedEntities) {
        for (NamedEntity entity : classifiedEntities) {
            System.out.println();
            System.out.println("Entity: " + entity.getLabel());
            System.out.println("Category: " + entity.getCategory());
            System.out.println("Topics: " + entity.getTopics());
            System.out.println("Ocurrence: " + entity.getOcurrence());

            // Print specific information for each category
            entity.printSpecificInfo();
        }
    }
}