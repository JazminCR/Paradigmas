package namedEntities;

import java.util.List;

public abstract class NamedEntity {
  private String label;
  private List<String> topics;
  private int ocurrence;

  public NamedEntity(String label, List<String> topics) {
    this.label = label;
    this.topics = topics;
    this.ocurrence = 1;
  }

  public String getLabel() {
    return label;
  }

  public List<String> getTopics() {
    return topics;
  }

  public int getOcurrence() {
    return ocurrence;
  }

  public void incrementOcurrence() {
    ocurrence++;
  }

  public abstract String getCategory();

  public abstract void printSpecificInfo();

  @Override
  public String toString() {
    return "Label: " + label + ", Category: " + getCategory() + ", Topics: " + topics + ", Ocurrence: " + ocurrence;
  }
}