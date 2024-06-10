package namedEntities.category;

import namedEntities.NamedEntity;
import java.util.List;

public class Other extends NamedEntity {
  public Other(String label, List<String> topics) {
    super(label, topics);
  }

  @Override
  public String getCategory() {
    return "OTHER";
  }

  @Override
  public void printSpecificInfo() {
    // No specific info to print
  }

}
