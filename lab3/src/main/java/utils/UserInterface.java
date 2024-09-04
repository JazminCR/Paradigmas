package utils;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

public class UserInterface {

    private HashMap<String, String> optionDict;
    private static HashMap<String, String[]> heuristicKeys;

    private List<Option> options;

    public UserInterface() {
        options = new ArrayList<Option>();
        options.add(new Option("-h", "--help", 0));
        options.add(new Option("-f", "--feed", 1));
        options.add(new Option("-ne", "--named-entity", 1));
        options.add(new Option("-pf", "--print-feed", 0));
        options.add(new Option("-sf", "--stats-format", 1));

        optionDict = new HashMap<String, String>();

        heuristicKeys = new HashMap<>();
        heuristicKeys.put("capitalized",
                new String[] { "CapitalizedWordHeuristic", "This heuristic extracts capitalized words" });
        heuristicKeys.put("acronyms", new String[] { "AcronymsHeuristic", "This heuristic extracts acronyms" });
        heuristicKeys.put("all", new String[] { "AllWordsHeuristic", "This heuristic extracts all words" });
        heuristicKeys.put("following",
                new String[] { "FollowingWordHeuristic", "This heuristic extracts words that follow a keyword" });
    }

    public Config handleInput(String[] args, List<FeedsData> feedsDataArray) {
        Boolean printFeed = false;
        Boolean computeNamedEntities = false;
        Boolean computeStats = false;
        String feedKey = null;
        String heuristicKey = null;
        String statsKey = null;

        for (int i = 0; i < args.length; i++) {
            String arg = args[i];
            switch (arg) {
                case "-h":
                case "--help":
                    printHelp(feedsDataArray);
                    System.exit(0);
                    break;
                case "-pf":
                case "--print-feed":
                    printFeed = true;
                    break;
                case "-ne":
                case "--named-entity":
                    computeNamedEntities = true;
                    if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
                        heuristicKey = args[++i];
                    } else {
                        System.out.println("Error: No heuristic specified for -ne option");
                        System.exit(0);
                    }
                    break;
                case "-sf":
                case "--stats-format":
                    computeStats = true;
                    if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
                        String key = args[++i];
                        if (key.equals("cat") || key.equals("topic")) {
                            statsKey = key;
                        } else {
                            System.out.println("Error: Invalid stats format specified");
                            System.exit(0);
                        }
                    } else {
                        statsKey = "cat";
                    }
                    break;
                case "-f":
                case "--feed":
                    if (i + 1 < args.length && !args[i + 1].startsWith("-")) {
                        feedKey = args[++i];
                    } else {
                        System.out.println("Error: No feed key specified for -f option");
                        System.exit(0);
                    }
                    break;
                default:
                    System.out.println("Error: Unknown option " + arg);
                    System.exit(0);
            }
        }

        if (!computeNamedEntities) {
            printFeed = true;
        }

        return new Config(printFeed, computeNamedEntities, computeStats, feedKey, heuristicKey, statsKey);
    }

    private static void printHelp(List<FeedsData> feedsDataArray) {
        System.out.println("Usage: make run ARGS=\"[OPTION]\"");
        System.out.println("Options:");
        System.out.println("  -h, --help: Show this help message and exit");
        System.out.println("  -f, --feed <feedKey>:                Fetch and process the feed with");
        System.out.println("                                       the specified key");
        System.out.println("                                       Available feed keys are: ");
        for (FeedsData feedData : feedsDataArray) {
            System.out.println("                                       " + feedData.getLabel());
        }
        System.out.println("  -ne, --named-entity <heuristicName>: Use the specified heuristic to extract");
        System.out.println("                                       named entities");
        System.out.println("                                       Available heuristic names are: ");
        for (String heuristicKey : heuristicKeys.keySet()) {
            String[] heuristicData = heuristicKeys.get(heuristicKey);
            System.out.println("                                       " + heuristicData[0] + ": " + heuristicData[1]);
        }
        System.out.println("                                       <name>: <description>");
        System.out.println("  -pf, --print-feed:                   Print the fetched feed");
        System.out.println("  -sf, --stats-format <format>:        Print the stats in the specified format");
        System.out.println("                                       Available formats are: ");
        System.out.println("                                       cat: Category-wise stats");
        System.out.println("                                       topic: Topic-wise stats");
    }
}