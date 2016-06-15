/* Name: Aland Kuang

   UID: 104157973

   Others With Whom I Discussed Things:

   Other Resources I Consulted:

*/

// import lists and other data structures from the Java standard library
import java.util.*;

// PROBLEM 1

// a type for arithmetic expressions
interface Exp {
     double eval(); 	                       // Problem 1a
     List<Instr> compile(); 	               // Problem 1c
}

class Num implements Exp {
    protected double val;

    public Num(double in_val){
        val = in_val;
    }

    public boolean equals(Object o) { return (o instanceof Num) && ((Num)o).val == this.val; }

    public String toString() { return "" + val; }

    @Override
    public double eval() {
        return val;
    }

    @Override
    public List<Instr> compile() {
        List<Instr> newlist = new ArrayList<Instr>();
        newlist.add(new Push(val));
        return newlist;
    }
}

class BinOp implements Exp {
    protected Exp left, right;
    protected Op op;

    public BinOp(Exp left_exp, Op the_op, Exp right_exp){
        left = left_exp;
        right = right_exp;
        op = the_op;
    }

    public boolean equals(Object o) {
        if(!(o instanceof BinOp))
            return false;
        BinOp b = (BinOp) o;
        return this.left.equals(b.left) && this.op.equals(b.op) &&
                this.right.equals(b.right);
    }

    public String toString() {
        return "BinOp(" + left + ", " + op + ", " + right + ")";
    }

    @Override
    public double eval() {
        //return 0;
        return op.calculate(left.eval(), right.eval());
    }

    @Override
    public List<Instr> compile() {
        List<Instr> newlist = new ArrayList<Instr>(left.compile());
        newlist.addAll(right.compile());
        newlist.add(new Calculate(op));
        return newlist;
    }
}

// a representation of four arithmetic operators
enum Op {
    PLUS { public double calculate(double a1, double a2) { return a1 + a2; } },
    MINUS { public double calculate(double a1, double a2) { return a1 - a2; } },
    TIMES { public double calculate(double a1, double a2) { return a1 * a2; } },
    DIVIDE { public double calculate(double a1, double a2) { return a1 / a2; } };

    abstract double calculate(double a1, double a2);
}

// a type for arithmetic instructions
interface Instr {
    void evaluate(Stack<Double> results);
}

class Push implements Instr {
    protected double val;

    public Push(double my_val){
        val = my_val;
    }

    public boolean equals(Object o) { return (o instanceof Push) && ((Push)o).val == this.val; }

    public String toString() {
        return "Push " + val;
    }

    @Override
    public void evaluate(Stack<Double> results) {
        results.push(val);
    }
}

class Calculate implements Instr {
    protected Op op;

    public Calculate(Op my_op){
        op = my_op;
    }

    public boolean equals(Object o) { return (o instanceof Calculate) &&
            ((Calculate)o).op.equals(this.op); }

    public String toString() {
        return "Calculate " + op;
    }

    @Override
    public void evaluate(Stack<Double> results) {
        double val1 = results.pop();
        double val2 = results.pop();
        results.push(op.calculate(val1, val2));

    }
}

class Instrs {
    protected List<Instr> instrs;

    public Instrs(List<Instr> instrs) { this.instrs = instrs; }

    public double execute() {
        Stack<Double>  result = new Stack<Double>();

        for ( Instr instruction : instrs) {
            instruction.evaluate(result);
        }
        return result.pop();
    }
}


class CalcTest {
    public static void main(String[] args) {
        //    // a test for Problem 1a
         Exp exp =
            	new BinOp(new BinOp(new Num(1.0), Op.PLUS, new Num(2.0)),
             	  	  Op.TIMES,
               	  new Num(3.0));
         assert(exp.eval() == 9.0);

        // // a test for Problem 1b
         List<Instr> is = new LinkedList<Instr>();
         is.add(new Push(1.0));
         is.add(new Push(2.0));
         is.add(new Calculate(Op.PLUS));
         is.add(new Push(3.0));
         is.add(new Calculate(Op.TIMES));
         Instrs instrs = new Instrs(is);
        System.out.println("Executing the instructions...");
        System.out.println(instrs.execute());
         //assert(instrs.execute() == 9.0);

        // // a test for Problem 1c
        if (exp.compile().equals(is))
            System.out.println("Yay!");
        else
            System.out.println("oh noes!");

        // assert(exp.compile().equals(is));
    }
}


// PROBLEM 2

// the type for a set of strings
interface StringSet {
     int size();
     boolean contains(String s);
     void add(String s);
}

// an implementation of StringSet using a linked list
class ListStringSet implements StringSet {
    protected SNode head;

   /* public ListStringSet(SNode a_head){
        head = a_head;
    }*/
    public ListStringSet(){
        head = new SEmpty(); //since the user is creating a new ListStringSet, it's only appropriate to create
                             // an empty head
    }

    @Override
    public int size() {
        return head.size();

    }

    @Override
    public boolean contains(String s) {
        return head.contains(s);

    }

    @Override
    public void add(String s) {
        head = head.add(s);
    }
}

// a type for the nodes of the linked list
interface SNode {
    int size();
    boolean contains(String s);
    SNode add(String s);
}

// represents an empty node (which ends a linked list)
class SEmpty implements SNode {
    @Override
    public int size() {
        return 0;
    }

    @Override
    public boolean contains(String s) {
        return false;
    }

    @Override
    public SNode add(String s) {
        SElement node_toadd = new SElement(s, this);
        return node_toadd;
    }
}

// represents a non-empty node
class SElement implements SNode {
    protected String elem;
    protected SNode next;

    public SElement(String the_elem, SNode nextone){
        elem = the_elem;
        next = nextone;
    }

    @Override
    public int size() {
        return 1 + next.size();
    }

    @Override
    public boolean contains(String s) {
        int compareval = elem.compareTo(s);
        if (compareval == 0 ){
            return true;
        }
        else if(compareval < 0){
            return next.contains(s);
        }
        else
            return false;
    }

    @Override
    public SNode add(String s) {
        int compareval = elem.compareTo(s);
        if(compareval > 0){
            SElement node_toadd = new SElement(s, this);
            return node_toadd;
        }
        else if(compareval == 0)
            return this;
        else {
            next = next.add(s);
            return this;
        }
    }
}

class ListStringSetTest {
    public static void main(String[] args){
        StringSet newset = new ListStringSet();
        System.out.println(newset.size());
        /*newset.add("hello");
        System.out.println(newset.size());
        newset.add("jack");
        System.out.println(newset.size());
        newset.add("loud");
        System.out.println(newset.size());
        newset.add("hh");
        System.out.println(newset.size());
        newset.add("aland");
        System.out.println(newset.size());
        if(newset.contains("loud")){
            System.out.println("Yay!");
        }
        newset.add("jack");
        System.out.println(newset.size());*/
        newset.add("loud");
        newset.add("jack");
        newset.add("hh");
        newset.add("hello");
        System.out.println(newset.size());
    }
}

interface Set<T>{
    int size();
    boolean contains(T s);
    void add(T s);
}

class ListSet<T> implements Set<T>{

    protected Comparator<T> m_comp;
    protected Node<T> head;

    public ListSet(Comparator<T> comp){
        m_comp = comp;
        head = new Empty<T>();
    }

    @Override
    public int size() {
        return head.size();
    }

    @Override
    public boolean contains(T s) {
        return head.contains(s, m_comp);
    }

    @Override
    public void add(T s) {
        head = head.add(s, m_comp);
    }


}

interface Node<T>{
    int size();
    boolean contains(T s, Comparator<T> comp_funct);
    Node<T> add(T s, Comparator<T> comp_funct);
}

class Empty<T> implements Node<T>{

    @Override
    public int size() {
        return 0;
    }

    @Override
    public boolean contains(T s, Comparator<T> comp_funct) {
        return false;
    }

    @Override
    public Node<T> add(T s, Comparator<T> comp_funct) {
        return new Element<T>(s,this);
    }
}

class Element<T> implements Node<T>{

    protected T elem;
    protected Node<T> next;

    public Element(T the_elem, Node<T> nextone){
        elem = the_elem;
        next = nextone;
    }

    @Override
    public int size() {
        return 1 + next.size();
    }

    @Override
    public boolean contains(T s, Comparator<T> comp_funct) {
        int comp_val = comp_funct.compare(elem, s);
        if(comp_val == 0){
            return true;
        }
        else if(comp_val < 0){
            return next.contains(s, comp_funct);
        }
        else
            return false;
    }

    @Override
    public Node<T> add(T s, Comparator<T> comp_funct) {
        int comp_val = comp_funct.compare(elem, s);
        if(comp_val > 0){
            return new Element<T>(s, this);
        }
        else if(comp_val == 0){
            return this;
        }
        else{
            next = next.add(s, comp_funct);
            return this;
        }
    }
}

class ListSetTest {

    public static void main(String[] args){
        /*ListSet<Integer> newset = new ListSet<Integer>((s1, s2) -> s2 - s1);
        newset.add(1);
        newset.add(3);
        newset.add(-6);
        newset.add(100);
        int j = 0;*/
        Set<String> s = new ListSet<String>((s1, s2) -> s2.compareTo(s1));
        s.add("F");
        s.add("D");
        s.add("E");
        System.out.println( s.size() );
    }


}
