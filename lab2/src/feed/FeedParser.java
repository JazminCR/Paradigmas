package feed;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;
import java.util.ArrayList;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.InputSource;
import java.io.StringReader;

public class FeedParser {

    public static List<Article> parseXML(String xmlData, String feedKey) {

        // lista para almacenar los artículos extraídos del XML
        List<Article> articles = new ArrayList<>();

        try {
            // configuración del parser (analizador XML)
            DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
            DocumentBuilder builder = factory.newDocumentBuilder();

            // parseo del XML
            Document document = builder.parse(new InputSource(new StringReader(xmlData)));
            document.getDocumentElement().normalize();

            // obtención de elementos "item" del XML que representan cada artículo
            NodeList itemList = document.getElementsByTagName("item");

            // iteración sobre cada "item"
            for (int i = 0; i < itemList.getLength(); i++) {
                // variables para almacenar los datos de cada artículo
                String title = null;
                String description = null;
                String link = null;
                String pubDate = null;

                // obtención de los nodos hijos de cada "item"
                Node itemNode = itemList.item(i);
                NodeList itemChildren = itemNode.getChildNodes();

                // iteración sobre los nodos hijos
                for (int j = 0; j < itemChildren.getLength(); j++) {
                    Node childNode = itemChildren.item(j);
                    // identificación del tipo de nodo hijo y almacenamiento de los datos
                    switch (childNode.getNodeName()) {
                        case "title":
                            title = childNode.getTextContent();
                            break;
                        case "description":
                            description = childNode.getTextContent();
                            break;
                        case "link":
                            link = childNode.getTextContent();
                            break;
                        case "pubDate":
                            pubDate = childNode.getTextContent();
                            break;
                        default:
                            break;
                    }
                }

                // si todos los datos del artículo son válidos, se crea un objeto Article y se
                // añade a la lista
                if (title != null && description != null && link != null && pubDate != null) {
                    Article articulo = new Article(title, description, pubDate, link, feedKey);
                    articles.add(articulo);
                }
            }

        } catch (Exception e) {
            System.out.println("Error parsing XML");
        }

        return articles;
    }

    public static String fetchFeed(String feedURL) throws MalformedURLException, IOException, Exception {

        URL url = new URL(feedURL);
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();

        connection.setRequestMethod("GET");
        connection.setRequestProperty("Content-Type", "application/json");

        // TODO: Cambiar el user-agent al nombre de su grupo.
        // Si todos los grupos usan el mismo user-agent, el servidor puede bloquear las
        // solicitudes.
        connection.setRequestProperty("user-agent", "JMJ");
        connection.setConnectTimeout(5000);
        connection.setReadTimeout(5000);

        int status = connection.getResponseCode();
        if (status != 200) {
            throw new Exception("HTTP error code: " + status);
        } else {
            BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
            String inputLine;
            StringBuffer content = new StringBuffer();
            while ((inputLine = in.readLine()) != null) {
                content.append(inputLine);
            }
            in.close();
            connection.disconnect();
            return content.toString();
        }
    }
}
