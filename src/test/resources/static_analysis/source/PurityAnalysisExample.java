class Point {
    Point(float x, float y) {
        this.x = x ; 
        this.y = y;
    }
    float x, y;
    void flip() {
        float t = x; 
        x = y; 
        y = t;
    }
}

class Cell {
    Point data;
    Cell next;
    Cell(Point d, Cell n) {
        data = d; next = n;
    }
}

interface Iterator {
    boolean hasNext();
    Point next();
}

class ListItr implements Iterator {
    ListItr(Cell head) {
        cell = head;
    }
    Cell cell; 
    public boolean hasNext() {
        return cell != null;
    }
    public Point next() {
        Point result = cell.data;
        cell = cell.next;
        return result;
    }
}

class PurityAnalysisExample {
    
    static float sumX(List list) {
        float s = 0;
        Iterator it = list.iterator();
        while(it.hasNext()) {
            Point p = it.next();
            s += p.x;
        }
        return s;
    }

    static void flipAll(List list) {
        Iterator it = list.iterator();
        while(it.hasNext()) {
            Point p = it.next();
            p.flip();
        }
    }

    public static void main(String[] args) {
        List list = new List();
        list.add(new Point(1,2));
        list.add(new Point(2,3));
        sumX(list);
        flipAll(list);
    }
}