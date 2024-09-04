package namedEntities.category;

import namedEntities.NamedEntity;
import java.util.List;

public class Location extends NamedEntity {

  private double latitude;
  private double longitude;

  public Location(String label, List<String> topics, double latitude, double longitude) {
    super(label, topics);
    this.latitude = latitude;
    this.longitude = longitude;
  }

  @Override
  public String getCategory() {
    return "LOCATION";
  }

  public double getLatitude() {
    return latitude;
  }

  public double getLongitude() {
    return longitude;
  }

  @Override
  public void printSpecificInfo() {
    System.out.println("Latitude: " + latitude);
    System.out.println("Longitude: " + longitude);
  }

}
