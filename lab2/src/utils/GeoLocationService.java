package utils;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;
import org.json.JSONArray;
import org.json.JSONObject;


public class GeoLocationService {


    public static double[] getCoordinates(String location) {
        double[] coordinates = new double[2];
        try {
            String encodedLocation = URLEncoder.encode(location, "UTF-8");
            String urlString = "https://nominatim.openstreetmap.org/search?q=" + encodedLocation + "&format=json&limit=1";
            URL url = new URL(urlString);
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setRequestProperty("User-Agent", "Mozilla/5.0");
            connection.setRequestProperty("Content-Type", "application/json");
            
            int responseCode = connection.getResponseCode();
            if (responseCode == 200) {
                BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                String inputLine;
                StringBuilder response = new StringBuilder();
                while ((inputLine = in.readLine()) != null) {
                    response.append(inputLine);
                }
                in.close();
                JSONArray jsonArray = new JSONArray(response.toString());
                if (jsonArray.length() > 0) {
                    JSONObject jsonObject = jsonArray.getJSONObject(0);
                    coordinates[0] = jsonObject.getDouble("lat");
                    coordinates[1] = jsonObject.getDouble("lon");
                }
            } else {
                System.out.println("HTTP error code: " + responseCode);
            }
        } catch (Exception e) {
            System.out.println("Error getting coordinates for location: " + location);
        }
        return coordinates;
    }
}







