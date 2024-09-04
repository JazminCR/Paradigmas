package namedEntities.category;

import namedEntities.NamedEntity;
import java.util.List;

public class Organization extends NamedEntity {

  private int yearFounded;
  private int numberOfEmployees;

  public Organization(String label, List<String> topics, int yearFounded, int numberOfEmployees) {
    super(label, topics);
    this.yearFounded = yearFounded;
    this.numberOfEmployees = numberOfEmployees;
  }

  @Override
  public String getCategory() {
    return "ORGANIZATION";
  }

  public int getYearFounded() {
    return yearFounded;
  }

  public int getNumberOfEmployees() {
    return numberOfEmployees;
  }

  @Override
  public void printSpecificInfo() {
    System.out.println("Year founded: " + yearFounded);
    System.out.println("Number of employees: " + numberOfEmployees);
  }

}
