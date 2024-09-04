import feed.Article;
import feed.FeedParser;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import utils.Config;
import utils.FeedsData;
import utils.JSONParser;
import utils.UserInterface;
import utils.StatisticsComputer;
import namedEntities.heuristics.NamedEntityHeuristic;
import namedEntities.heuristics.HeuristicFactory;
import namedEntities.NamedEntity;
import namedEntities.EntityClassifier;
import namedEntities.DictionaryReader;
import namedEntities.ComputeEntities;

public class App {

  public static void main(String[] args) {

    List<FeedsData> feedsDataArray = new ArrayList<>();
    try {
      feedsDataArray = JSONParser.parseJsonFeedsData("src/main/java/data/feeds.json");
    } catch (IOException e) {
      e.printStackTrace();
      System.exit(1);
    }

    UserInterface ui = new UserInterface();
    Config config = ui.handleInput(args, feedsDataArray);

    // Debug: print the feedKey to ensure it's set correctly
    if (config.getFeedKey() != null) {
      System.out.println("Configured feed: " + config.getFeedKey());
    } else {
      System.out.println("No feedKey provided, all feeds will be processed");
    }

    run(config, feedsDataArray);
  }

  // TODO: Change the signature of this function if needed
  private static void run(Config config, List<FeedsData> feedsDataArray) {

    // verifica si la lista de datos de feeds está vacía o es nula
    if (feedsDataArray == null || feedsDataArray.size() == 0) {
      System.out.println("No feeds data found");
      return;
    }

    // lista para almacenar todos los artículos extraídos de los feeds
    List<Article> allArticles = new ArrayList<>();

    // TODO: Populate allArticles with articles from corresponding feeds

    // verifica si se especificó un feed en la configuración
    if (config.getFeedKey() != null) {
      boolean feedFound = false;
      for (FeedsData feedData : feedsDataArray) {
        if (feedData.getLabel().equals(config.getFeedKey())) {
          feedFound = true;
          try {
            // obtiene el contenido del feed
            String feedContent = FeedParser.fetchFeed(feedData.getUrl());
            List<Article> articles = FeedParser.parseXML(feedContent, config.getFeedKey());
            allArticles.addAll(articles);
          } catch (Exception e) {
            e.printStackTrace();
          }
          break;
        }
        // en caso de que el feed no se encuentre en la lista de feeds
      }
      if (!feedFound) {
        System.out.println("Feed not found\nPlease rerun with the -h option to see the available feeds");
        return;
      }
    } else {
      // en caso de que no se haya especificado un feed, se procesan todos los feeds
      for (FeedsData feedData : feedsDataArray) {
        try {
          String feedContent = FeedParser.fetchFeed(feedData.getUrl());
          List<Article> articles = FeedParser.parseXML(feedContent, feedData.getLabel());
          allArticles.addAll(articles);
        } catch (Exception e) {
          e.printStackTrace();
        }
      }
    }

    if (config.getPrintFeed()) {
      for (Article article : allArticles) {
        System.out.println("\nArticle from feed: " + article.getFeedKey()); // Debug: print feedKey for each article
        article.print();
      }
    }

    if (config.getComputeNamedEntities()) {
      ComputeEntities processor = new ComputeEntities();
      processor.computeNamedEntities(config, allArticles);
    } else {
      System.out.println("\nIf you want to compute named entities, rerun with the -ne option\n");
    }

  }

}