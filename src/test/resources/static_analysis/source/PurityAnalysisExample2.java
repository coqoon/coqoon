class Person {
    String name; 
    int age; 
    
    public Person(String name, int age) {
        this.name = name; 
        this.age = age;
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

// Load node 'p' of PersonModifier.setName maps to inside node of pta and as such, no
// modifications should be recorded and this should be pure.
class ParameterToArgument {
    void pta() {
        Person person = new Person("Mads",22);
        PersonModifier pMod = new PersonModifier();
        pMod.setName(person, "Sdam");
    }
}

