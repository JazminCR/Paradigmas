package namedEntities;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.List;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import namedEntities.category.Location;
import namedEntities.category.Organization;
import namedEntities.category.Other;
import namedEntities.category.Person;
import utils.GeoLocationService;

public class DictionaryReader {

    public static Map<String, NamedEntity> readDictionary(String filePath) throws IOException {
        // Leer el contenido del archivo JSON
        String jsonData = new String(Files.readAllBytes(new File(filePath).toPath()));

        // Crear un mapa para almacenar las entradas del diccionario
        Map<String, NamedEntity> entityMap = new HashMap<>();

        // Convertir la cadena JSON en un array JSON
        JSONArray dictionary = new JSONArray(jsonData);

        // Recorrer cada entrada del diccionario
        for (int i = 0; i < dictionary.length(); i++) {
            JSONObject entry = dictionary.getJSONObject(i);
            String label = entry.getString("label");
            String category = entry.getString("Category");
            List<String> topics = new ArrayList<>();
            JSONArray topicsArray = entry.getJSONArray("Topics");
            for (int j = 0; j < topicsArray.length(); j++) {
                topics.add(topicsArray.getString(j));
            }
            List<String> keywords = new ArrayList<>();
            JSONArray keywordsArray = entry.getJSONArray("keywords");
            for (int j = 0; j < keywordsArray.length(); j++) {
                keywords.add(keywordsArray.getString(j).toLowerCase());
            }

            NamedEntity entity = null;

            // Determinar la subclase adecuada de NamedEntity según la categoría
            switch (category) {
                case "PERSON":
                    int age = 0;
                    String gender = "Unknown";
                    entity = new Person(label, topics, age, gender);
                    break;
                case "LOCATION":
                    double[] coordinates = GeoLocationService.getCoordinates(label);
                    double latitude = coordinates[0];
                    double longitude = coordinates[1];
                    entity = new Location(label, topics, latitude, longitude);
                    break;
                case "ORGANIZATION":
                    int yearFounded = 0;
                    int numberOfEmployees = 0;
                    entity = new Organization(label, topics, yearFounded, numberOfEmployees);
                    break;
                default:
                    entity = new Other(label, topics);
                    break;
            }

            // Almacenar la entidad en el mapa para cada palabra clave del diccionario
            for (String keyword : keywords) {
                entityMap.put(keyword, entity);
            }
        }

        // Devolver el mapa resultante
        return entityMap;
    }
}
