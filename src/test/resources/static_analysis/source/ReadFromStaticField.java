class Person {

  static String defaultName = "Mr. Default";

  String name;

  public Person(String s) {
    this.name = s;
  }
}

class ReadFromStaticField {
  public void method(Person p) {
    String newName = Person.defaultName;
    p.name = newName;
  }
}