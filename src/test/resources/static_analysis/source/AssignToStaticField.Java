class Person {

  static String defaultName = "Mr. Default";

  String name;

  public Person(String s) {
    this.name = s;
  }
}

class AssignToStaticField {
  public void setDefaultName(String defaultName) {
    Person.defaultName = defaultName;
  }
}