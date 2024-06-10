package utils;

public class Config {
    private boolean printFeed = false;
    private boolean computeNamedEntities = false;
    private boolean computeStats = false;
    private String feedKey;
    private String heuristicKey;
    private String statsKey;

    public Config(Boolean printFeed, Boolean computeNamedEntities, Boolean computeStats, String feedKey,
            String heuristicKey, String statsKey) {
        this.printFeed = printFeed;
        this.computeNamedEntities = computeNamedEntities;
        this.computeStats = computeStats;
        this.feedKey = feedKey;
        this.heuristicKey = heuristicKey;
        this.statsKey = statsKey;
    }

    public boolean getPrintFeed() {
        return printFeed;
    }

    public boolean getComputeNamedEntities() {
        return computeNamedEntities;
    }

    public boolean getComputeStats() {
        return computeStats;
    }

    public String getFeedKey() {
        return feedKey;
    }

    public String getHeuristicKey() {
        return heuristicKey;
    }

    public String getStatsKey() {
        return statsKey;
    }
}
