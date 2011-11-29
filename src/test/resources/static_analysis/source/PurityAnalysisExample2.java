class Person {
    String name; 
    int age; 
    
    public Person(String name, int age) {
        this.name = name; 
        this.age = age;
    }
    
    public String getName() {
        String name = this.name;
        return name;
    }
}

class PersonModifier {
    
    void setName(Person p, String newName) {
        p.name = newName;
    }
        
    void swapNames(Person p1, Person p2) {
        String name1 = p1.name; // I feel like this should produce an outside edge!
        String name2 = p2.name;
        p1.name = name2;
        p2.name = name1;
    }

    public PersonModifier(){}
    
}

class ParameterToArgument {
    
    void existingAndModify(Person person) {
        PersonModifier pMod = new PersonModifier();
        pMod.setName(person, "Sdam");
    }
    
    void newAndModify() {
        Person person = new Person("Mads",22);
        PersonModifier pMod = new PersonModifier();
        pMod.setName(person, "Sdam");
    }
}

