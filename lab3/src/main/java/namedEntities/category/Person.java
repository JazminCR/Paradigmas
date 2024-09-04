package namedEntities.category;

import namedEntities.NamedEntity;
import java.util.List;

public class Person extends NamedEntity {

  private int age;
  private String gender;

  public Person(String label, List<String> topics, int age, String gender) {
    super(label, topics);
    this.age = age;
    this.gender = gender;
  }

  @Override
  public String getCategory() {
    return "PERSON";
  }

  public int getAge() {
    return age;
  }

  public String getGender() {
    return gender;
  }

  @Override
  public void printSpecificInfo() {
    System.out.println("Age: " + age);
    System.out.println("Gender: " + gender);
  }

}
