package feed;

public class Article {
    private String title;
    private String description;
    private String pubDate;
    private String link;
    private String feedKey;

    public Article(String title, String description, String pubDate, String link, String feedKey) {
        this.title = title;
        this.description = description;
        this.pubDate = pubDate;
        this.link = link;
        this.feedKey = feedKey;
    }

    public String getTitle() {
        return title;
    }

    public String getDescription() {
        return description;
    }

    public String getPubDate() {
        return pubDate;
    }

    public String getLink() {
        return link;
    }

    public String getFeedKey() {
        return feedKey;
    }

    public void print() {
        System.out.println("Title: " + title + "\n");
        System.out.println("Description: " + description + "\n");
        System.out.println("Publication Date: " + pubDate + "\n");
        System.out.println("Link: \n" + link + "\n");
        System.out.println("***********************************************");
    }

}