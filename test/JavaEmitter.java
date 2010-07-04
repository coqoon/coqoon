/* Copyright (c) 2003 Andy Tripp */

package bob.harry.tom;

import antlr.*;
import antlr.collections.*;
import antlr.collections.impl.*;
import java.io.*;
import java.util.*;

/**
 * JavaEmitter: A class that can take an ANTLR Java AST and produce reasonably formatted
 * Java code from it. To use it, create a JavaEmitter object, call setOut()
 * if you want to print to something other than System.out, and then call
 * print(), passing the AST. Typically, the AST node that you pass would be the
 * root of a tree - the ROOT_ID node that represents a Java file. See
 * main() for example usage.
 */

/* To compile, place this file and IndentingPrintStream.java in your 
 * antlr-x.x.x/examples/java/java directory. Make sure this directory and ANTLR classes
 * are in your CLASSPATH. Run ANTLR on java.g and java.tree.g and then compile everything:
 * $ java antlr.Tool java.g
 * $ java antlr.Tool java.tree.g
 * $ javac *.java
 *
 * Test by running the main method here:
 * java JavaEmitter MyJavaFile.java
 * This will print both the AST as a tree (to stderr) and the formatted Java code (to stdout)
 *
 * This has been tested with ANTLR 2.7.1 and 2.7.2.
 */

public class JavaEmitter implements JavaTokenTypes
{
	private IndentingPrintStream out = new IndentingPrintStream(System.out);
	private PrintStream debug = System.err;
	private static int ALL = -1;
	private String indentString = "    ";
	private java.util.Stack stack = new java.util.Stack();
	private static String[] tokenNames;
	private final static int ROOT_ID = 0;
	static {
		setupTokenNames();
	}

	// Map each AST token type to a String
	private static void setupTokenNames() {
		tokenNames = new String[200];
		for (int i=0; i<tokenNames.length; i++) {
			tokenNames[i] = "ERROR:" + i;
		}

		tokenNames[POST_INC]="++";
		tokenNames[POST_DEC]="--";
		tokenNames[UNARY_MINUS]="-";
		tokenNames[UNARY_PLUS]="+";
		tokenNames[STAR]="*";
		tokenNames[ASSIGN]="=";
		tokenNames[PLUS_ASSIGN]="+=";
		tokenNames[MINUS_ASSIGN]="-=";
		tokenNames[STAR_ASSIGN]="*=";
		tokenNames[DIV_ASSIGN]="/=";
		tokenNames[MOD_ASSIGN]="%=";
		tokenNames[SR_ASSIGN]=">>=";
		tokenNames[BSR_ASSIGN]=">>>=";
		tokenNames[SL_ASSIGN]="<<=";
		tokenNames[BAND_ASSIGN]="&=";
		tokenNames[BXOR_ASSIGN]="^=";
		tokenNames[BOR_ASSIGN]="|=";
		tokenNames[QUESTION]="?";
		tokenNames[LOR]="||";
		tokenNames[LAND]="&&";
		tokenNames[BOR]="|";
		tokenNames[BXOR]="^";
		tokenNames[BAND]="&";
		tokenNames[NOT_EQUAL]="!=";
		tokenNames[EQUAL]="==";
		tokenNames[LT]="<";
		tokenNames[GT]=">";
		tokenNames[LE]="<=";
		tokenNames[GE]=">=";
		tokenNames[SL]="<<";
		tokenNames[SR]=">>";
		tokenNames[BSR]=">>>";
		tokenNames[PLUS]="+";
		tokenNames[MINUS]="-";
		tokenNames[DIV]="/";
		tokenNames[MOD]="%";
		tokenNames[INC]="++";
		tokenNames[DEC]="--";
		tokenNames[BNOT]="~";
		tokenNames[LNOT]="!";
		tokenNames[FINAL]="final";
		tokenNames[ABSTRACT]="abstract";
		tokenNames[LITERAL_package]="package";
		tokenNames[LITERAL_import]="import";
		tokenNames[LITERAL_void]="void";
		tokenNames[LITERAL_boolean]="boolean";
		tokenNames[LITERAL_byte]="byte";
		tokenNames[LITERAL_char]="char";
		tokenNames[LITERAL_short]="short";
		tokenNames[LITERAL_int]="int";
		tokenNames[LITERAL_float]="float";
		tokenNames[LITERAL_long]="long";
		tokenNames[LITERAL_double]="double";
		tokenNames[LITERAL_private]="private";
		tokenNames[LITERAL_public]="public";
		tokenNames[LITERAL_protected]="protected";
		tokenNames[LITERAL_static]="static";
		tokenNames[LITERAL_transient]="transient";
		tokenNames[LITERAL_native]="native";
		tokenNames[LITERAL_threadsafe]="threadsafe";
		tokenNames[LITERAL_synchronized]="synchronized";
		tokenNames[LITERAL_volatile]="volatile";
		tokenNames[LITERAL_class]="class";
		tokenNames[LITERAL_extends]="extends";
		tokenNames[LITERAL_interface]="interface";
		tokenNames[LITERAL_implements]="implements";
		tokenNames[LITERAL_throws]="throws";
		tokenNames[LITERAL_if]="if";
		tokenNames[LITERAL_else]="else";
		tokenNames[LITERAL_for]="for";
		tokenNames[LITERAL_while]="while";
		tokenNames[LITERAL_do]="do";
		tokenNames[LITERAL_break]="break";
		tokenNames[LITERAL_continue]="continue";
		tokenNames[LITERAL_return]="return";
		tokenNames[LITERAL_switch]="switch";
		tokenNames[LITERAL_throw]="throw";
		tokenNames[LITERAL_case]="case";
		tokenNames[LITERAL_default]="default";
		tokenNames[LITERAL_try]="try";
		tokenNames[LITERAL_finally]="finally";
		tokenNames[LITERAL_catch]="catch";
		tokenNames[LITERAL_instanceof]="instanceof";
		tokenNames[LITERAL_this]="this";
		tokenNames[LITERAL_super]="super";
		tokenNames[LITERAL_true]="true";
		tokenNames[LITERAL_false]="false";
		tokenNames[LITERAL_null]="null";
		tokenNames[LITERAL_new]="new";
	}

	/**
	 * Specify a PrintStream to print to. System.out is the default.
	 * @param out the PrintStream to print to
	 */
	public void setOut(PrintStream out) {
		this.out = new IndentingPrintStream(out);
	}
	private String name(AST ast) {
		return tokenNames[ast.getType()];
	}
	private String (int type) {
		return tokenNames[type];
	}

	/**
	 * Find a child of the given AST that has the given type
	 * @returns a child AST of the given type. If it can't find a child of the given type, return null.
	 */
	private AST getChild(AST ast, int childType) {
		AST child = ast.getFirstChild();
		while (child != null) {
			if (child.getType() == childType) {
				// debug.println("getChild: found:" + name(ast));
				return child;
			}
			child = child.getNextSibling();
		}
		return null;
	}

	/**
	 * Print the children of the given AST
	 * @param ast The AST to print
	 * @param separator The separator to use (typically space or newline)
	 * @returns true iff anything was printed
	 */
	private boolean printChildren(AST ast, String separator) {
		return printChildren(ast, separator, ALL);
	}

	/**
	 * Print all of the children of the given AST that are of the given type
	 * @param ast The AST to print
	 * @param separator The separator to use (typically space or newline)
	 * @param type The type of child AST to print
	 * @returns true iff anything was printed
	 */
	private boolean printChildren(AST ast, String separator, int type) {
		boolean ret = false;
		AST child = ast.getFirstChild();
		while (child != null) {
			if (type == ALL || child.getType() == type) {
				// print a separator before each printed child (except first)
				if (child != ast.getFirstChild()) {
					if (separator.endsWith("\n")) {
						out.print(separator.substring(0,separator.length()-1));
						out.println();
					}
					else {
						out.print(separator);
					}
				}
				ret = true;
				print(child);
			}
			child = child.getNextSibling();
		}
		return ret;
	}
	
	/**
	 * Tells whether an AST has any children or not.
	 * @returns true iff the AST has at least one child
	 */
	private boolean hasChildren(AST ast) {
		return (ast.getFirstChild() != null);
	}

	/**
	 * Prints a bunary operator
	 */
	private void printBinaryOperator(AST ast) {
		printWithParens(ast, ast.getFirstChild());
		out.print(" " + name(ast) + " ");
		printWithParens(ast, ast.getFirstChild().getNextSibling());
	}

	/**
	 * Prints an AST, adding parenthises if they are needed.
	 * Parens are needed inside an expression when the precendence of the
	 * parent AST is lower than the child AST.
	 */
	private void printWithParens(AST parent, AST ast) {
		boolean parensNeeded = (getPrecedence(parent) < getPrecedence(ast));
		if (parensNeeded) {
			out.print("(");
		}
		print(ast);
		if (parensNeeded) {
			out.print(")");
		}
	}

	/**
	 *  Starts a block by printing "{" and increasing the indent level.
	 */
	private void startBlock() {
		out.print("{");
		out.increaseIndent();
		out.println();
	}

	/**
	 *  Starts a block by decreasing the indent level and printing "}"
	 */
	private void endBlock() {
		out.decreaseIndent();
		out.print("}");
	}

	/**
	 * some (but not all) of the children of a SEMI node should be suffixed by ';'.
	 * for example, EXPR should be suffixed by ';' ( but only when its parent is a SLIST).
	 */
	private void printSemi(AST parent) {
		//System.out.println("semi: parent type=" + parent.getType() + " SLIST=" + SLIST);
		if (parent!= null && parent.getType() == SLIST) {
			out.print(";");
		}
	}

	/**
	 * Print the given AST. Call this function to print your Java code.
	 *
	 * <p> <b>Overall Approach</b><br>
	 * It works by making recursive calls to print children. For example,
	 * the root of the AST tree has type ROOT_ID. A Java AST node generated from the
	 * ANTLR java.g file will have type ROOT_ID and will have
	 * the following children, in this order:
	 *     0 or 1 children of type PACKAGE_DEF
	 *     0 or more children of type IMPORT
	 *     One child of type CLASS_DEF or type INTERFACE_DEF
     *<p>
	 * So the code below is one big "switch" statement on the passed AST type.
	 * In the "ROOT_ID" case, the code here does the following:
	 * <ol>
	 * <li>calls getChild(ast, PACKAGE_DEF) to get the
	 * child of type PACKAGE_DEF, and makes a recursive call to print() on that AST.
	 * If there is no "PACKAGE_DEF" child, getChild() would return null and the
	 * recursive print() call would print nothing.
	 * <li>Calls printChildren(), passing the AST, the "\n" separator, and
	 * the type "IMPORT". printChildren() will print all children of the AST,
	 * separating each by the "\n" separator.
	 * <li>Does the same thing will CLASS_DEF that it did with PACKAGE_DEF: calls
	 * getChild() to get the child of type CLASS_DEF (or null if there is none),
	 * and then makes a recursive call to print().
	 * <li>Does the same thing for INTEFACE_DEF: call getChild() to get the child
	 * of type INTEFACE_DEF, and make a recursive call.
	 *</ol>
     * 
	 * <p> <b>Indenting</b><br>
	 * One important issue is how to do proper indenting. The IndentingPrintStream
	 * class handles indenting. Here, we simply create an IndentingPrintStream from
	 * our normal PrintStream (either System.out or whatever was passed to setOut()).
	 * And then we call increaseIndent() and decreaseIndent() as we see "{" and "}"
	 * AST nodes.
	 *
	 * <p> <b>Adding Parentheses</b><br>
	 * 
	 * The only other tricky part here is in printing parentheses, which are not kept
	 * as AST nodes, but are built-in ("inherent") to the structure of the AST.
	 * The printWithParens() method is used to print all unary and binary operators.
	 * This method uses a precendence table to determine whether it needs to print
	 * parentheses or not.
	 */
	public void print (AST ast) {
		if (ast == null) {
			return;
		}

		AST parent = null;
		if (!stack.isEmpty()) {
			parent = (AST) stack.peek();
		}
		stack.push(ast);

		AST child1 = ast.getFirstChild();
		AST child2 = null;
		AST child3 = null;
		if (child1 != null) {
			child2 = child1.getNextSibling();
			if (child2 != null) {
				child3 = child2.getNextSibling();
			}
		}

		switch(ast.getType()) {
			// The top of the tree looks like this:
			//  ROOT_ID  "Whatever.java"
			//   package
			//   imports
			//   class definition
			case ROOT_ID:
				print(getChild(ast, PACKAGE_DEF));
				printChildren(ast, "\n",  IMPORT);
				out.println();
				out.println();
				print(getChild(ast, CLASS_DEF));    // only one of these...
				print(getChild(ast, INTERFACE_DEF));  // will print anything
				out.println();
				break;

			case PACKAGE_DEF:
				out.print("package ");
				print(ast.getFirstChild());
				out.print(";");
				out.println();
				out.println();
				break;

			// IMPORT has exactly one child
			case IMPORT:
				out.print("import ");
				print(ast.getFirstChild());
				out.print(";");
				break;

			case CLASS_DEF:
			case INTERFACE_DEF:
				print(getChild(ast, MODIFIERS));
				if (ast.getType() == CLASS_DEF) {
					out.print("class ");
				} else {
					out.print("interface ");
				}
				print(getChild(ast, IDENT));
				out.print(" ");
				print(getChild(ast, EXTENDS_CLAUSE));
				print(getChild(ast, IMPLEMENTS_CLAUSE));
				startBlock();
				print(getChild(ast, OBJBLOCK));
				endBlock();
				break;

			case MODIFIERS:
				if (hasChildren(ast)) {
					printChildren(ast, " ");
					out.print(" ");
				}
				break;

			case EXTENDS_CLAUSE:
				if (hasChildren(ast)) {
					out.print("extends ");
					printChildren(ast, ", ");
					out.print(" ");
				}
				break;

			case IMPLEMENTS_CLAUSE:
				if (hasChildren(ast)) {
					out.print("implements ");
					printChildren(ast, ", ");
					out.print(" ");
				}
				break;

			// DOT always has exactly two children.
			case DOT:
				print(child1);
				out.print(".");
				print(child2);
				break;

			// the typical order of things within a class is:
			// variable definitions  (no, we do not like these at the bottom, mr. C++ programmer)
			// static initialization block
			// instance initialization block
			// constructors
			// methods
			// inner classes
			case OBJBLOCK:
				if (printChildren(ast, "\n",  VARIABLE_DEF)) {
					out.println();
				}
				if (printChildren(ast, "\n",  STATIC_INIT)) {
					out.println();
				}
				if (printChildren(ast, "\n",  INSTANCE_INIT)) {
					out.println();
				}
				if (printChildren(ast, "\n",  CTOR_DEF)) {
					out.println();
				}
				if (printChildren(ast, "\n",  METHOD_DEF)) {
					out.println();
				}
				printChildren(ast, "\n",  CLASS_DEF);
				break;

			case CTOR_DEF:
			case METHOD_DEF:
				print(getChild(ast, MODIFIERS));
				if (ast.getType() != CTOR_DEF) {
					print(getChild(ast, TYPE));
					out.print(" ");
				}
				print(getChild(ast, IDENT));				
				print(getChild(ast, PARAMETERS));
				print(getChild(ast, LITERAL_throws));
				AST methodBody = getChild(ast, SLIST);
				if (methodBody == null) {
					out.print(";");
				} else {
					out.print(" ");
					print(methodBody);
				}
				break;

			case PARAMETERS:
				out.print("(");
				printChildren(ast, ", ");
				out.print(") ");
				break;

			case PARAMETER_DEF:
				print(getChild(ast, MODIFIERS));
				print(getChild(ast, TYPE));
				out.print(" ");
				print(getChild(ast, IDENT));				
				break;

			case VARIABLE_DEF:
				print(getChild(ast, MODIFIERS));
				print(getChild(ast, TYPE));
				out.print(" ");
				print(getChild(ast, IDENT));
				print(getChild(ast, ASSIGN));
				// don't always suffix with ';': example: "for (int i=0; i<l; i++)" the semi after the
				// 0 is put there by the FOR rule, not the variable_def rule.
				printSemi(parent);
				if (parent!= null && parent.getType() == OBJBLOCK) {
					out.print(";");
				}
				break;

			// TYPE has exactly one child.
			case TYPE:
				print(ast.getFirstChild());
				break;

			case ARRAY_DECLARATOR:
				if (child1 == null) {		// I'm not sure what this case is :(
					out.print("[]");
				}
				else if (child1.getType() == EXPR) {
					out.print("[");
					print(child1);
					out.print("]");
				}
				else {
					print(child1);
					out.print("[]");		// we prefer "int[] x" to "int x[]"
				}
				break;

			// if we have two children, it's of the form "a=0"
			// if just one child, it's of the form "=0" (where the
			// lhs is above this AST).
			case ASSIGN:
				if (child2 != null) {
					print(child1);
					out.print(" = ");
					print(child2);
				}
				else {
					out.print(" = ");
					print(child1);
				}
				break;


			case EXPR:
				print(child1);
				printSemi(parent);
				break;

			case ARRAY_INIT:
				out.print("{");
				printChildren(ast, ", ");
				out.print("}");
				break;

			case SLIST:
				startBlock();
				if (printChildren(ast, "\n")) {
					out.println();
				}
				endBlock();
				break;

			// binary operators:
			case PLUS:
			case MINUS:
			case DIV:
			case MOD:
			case NOT_EQUAL:
			case EQUAL:
			case LT:
			case GT:
			case LE:
			case GE:
			case LOR:
			case LAND:
			case BOR:	
			case BXOR:
			case BAND:
			case SL:
			case SR:
			case BSR:
			case LITERAL_instanceof: 
			case PLUS_ASSIGN:
			case MINUS_ASSIGN:
			case STAR_ASSIGN:
			case DIV_ASSIGN:
			case MOD_ASSIGN:
			case SR_ASSIGN:
			case BSR_ASSIGN:
			case SL_ASSIGN:
			case BAND_ASSIGN:
			case BXOR_ASSIGN:
			case BOR_ASSIGN:
				printBinaryOperator(ast);
				break;


			case LITERAL_for:
				out.print("for (");
				print(getChild(ast, FOR_INIT));
				out.print("; ");
				print(getChild(ast, FOR_CONDITION));
				out.print("; ");
				print(getChild(ast, FOR_ITERATOR));
				out.print(") ");
				print(getChild(ast, SLIST));
				break;

			case FOR_INIT:
			case FOR_CONDITION:
			case FOR_ITERATOR:
				print(child1);
				break;

			case ELIST:
				printChildren(ast, ", ");
				break;

			case POST_INC:
			case POST_DEC:
				print(child1);
				out.print(name(ast));
				break;

			// unary operators:
			case BNOT:
			case LNOT:
			case INC:
			case DEC:
			case UNARY_MINUS:
			case UNARY_PLUS:
				out.print(name(ast));
				printWithParens(ast, child1);
				break;

			case LITERAL_new:
				out.print("new ");
				print(child1);
				if (child2.getType() != ARRAY_DECLARATOR) {
					out.print("(");
				}
				print(child2);
				if (child2.getType() != ARRAY_DECLARATOR) {
					out.print(")");
				}
				// "new String[] {...}": the stuff in {} is child3
				if (child3 != null) {
					out.print(" ");
					print(child3);
				}
				break;

			case METHOD_CALL:
				print(child1);
				out.print("(");
				print(child2);
				out.print(")");
				break;

			case LITERAL_return:
				out.print("return ");
				print(child1);
				out.print(";");
				break;

			case INSTANCE_INIT:
				startBlock();
				print(child1);
				endBlock();
				break;

			case STATIC_INIT:
				out.print("static ");
				startBlock();
				print(child1);
				endBlock();
				break;

			case TYPECAST:
				out.print("(");
				print(child1);
				out.print(") ");
				print(child2);
				break;

			case LITERAL_switch:
				out.print("switch (");
				print(child1);	// the EXPR to switch on
				out.print(") ");
				startBlock();
				printChildren(ast, "",  CASE_GROUP);
				endBlock();
				break;

			case CASE_GROUP:
				printChildren(ast, "\n",  LITERAL_case);
				printChildren(ast, "\n",  LITERAL_default);
				printChildren(ast, "",  SLIST);
				break;

			case LITERAL_case:
				out.print("case ");
				print(child1);	// an EXPR
				out.print(":");
				break;

			case LITERAL_default:
				out.print("default:");
				print(child1);	// an EXPR
				break;


			case IDENT:
			case NUM_INT:
			case NUM_LONG:
			case CHAR_LITERAL:
			case STRING_LITERAL:
			case NUM_FLOAT:
			case NUM_DOUBLE:
				out.print(ast.getText());
				break;

			case LITERAL_private:
			case LITERAL_public:
			case LITERAL_protected:
			case LITERAL_static:
			case LITERAL_transient:
			case LITERAL_native:
			case LITERAL_threadsafe:
			case LITERAL_synchronized:
			case LITERAL_volatile:
			case FINAL:
			case ABSTRACT:
			case LITERAL_package:
			case LITERAL_void:
			case LITERAL_boolean:
			case LITERAL_byte:
			case LITERAL_char:
			case LITERAL_short:
			case LITERAL_int:
			case LITERAL_float:
			case LITERAL_long:
			case LITERAL_double:
			case LITERAL_true:
			case LITERAL_false:
			case LITERAL_null:
			case SEMI:
			case LITERAL_this:
			case LITERAL_super:
				out.print(name(ast));
				break;

			case LITERAL_continue:
			case LITERAL_break:
				out.print(name(ast));
				out.print(";");
				break;

			case INDEX_OP:
				print(child1);		// an IDENT
				out.print("[");
				print(child2);	// an EXPR
				out.print("]");
				break;

			case EMPTY_STAT:
				out.print(";");	// empty statement
				break;

			// yuck:  Distinguish between "import x.y.*" and "x = 1 * 3"
			case STAR:
				if (hasChildren(ast)) {	// the binary mult. operator
					printBinaryOperator(ast);
				}
				else {	// the special "*" in import:
					out.print("*");
				}
				break;

			case LITERAL_throws:
				out.print("throws ");
				printChildren(ast, ", ");
				break;

			case LITERAL_if:
				out.print("if (");
				print(child1);	// the "if" condition: an EXPR
				out.print(") ");
				print(child2);	// the "then" clause is an SLIST
				if (child3 != null) {
					out.print("else ");
					print(child3);	// optional "else" clause: an SLIST
				}
				break;

			case LITERAL_while:
				out.print("while (");
				print(child1);	// the "while" condition: an EXPR
				out.print(") ");
				print(child2);	// an SLIST
				break;

			case LITERAL_do:
				out.print("do ");
				print(child1);		// an SLIST
				out.print(" while (");
				print(child2);		// an EXPR
				out.print(");");
				break;

			case LITERAL_try:
				out.print("try ");
				print(child1);	// an SLIST
				printChildren(ast, " ", LITERAL_catch);
				break;

			case LITERAL_catch:
				out.print("catch (");
				print(child1);	// a PARAMETER_DEF
				out.print(") ");
				print(child2);	// an SLIST
				break;

			// the first child is the "try" and the second is the SLIST
			case LITERAL_finally:
				print(child1);
				out.print(" finally ");
				print(child2);	// an SLIST
				break;

			case LITERAL_throw:
				out.print("throw ");
				print(child1);
				out.print(";");
				break;

			// the dreaded trinary operator
			case QUESTION:
				print(child1);
				out.print(" ? ");
				print(child2);
				out.print(" : ");
				print(child3);
				break;

			// (note: the java.g provided by default skips comments)
			case SL_COMMENT:
				break;

			case ML_COMMENT:
				break;

			case LITERAL_class:
				out.print("class");
				break;
					
			case LITERAL_assert:
				out.print("assert ");
				print(child1);
				if (child2 != null) {
					out.print(" : ");
					print(child2);
				}
				break;
					
			default:
				debug.println("Invalid type:" + ast.getType());
				break;


/* The following are tokens, but I don't think JavaRecognizer 
   ever produces an AST with one of these types:
			case COMMA:
			case LITERAL_implements:
			case LITERAL_class:
			case LITERAL_extends:
			case EOF:
			case NULL_TREE_LOOKAHEAD:
			case BLOCK:
			case LABELED_STAT:	// refuse to implement on moral grounds :)
			case LITERAL_import:
			case LBRACK:
			case RBRACK:
			case LCURLY:
			case RCURLY:
			case LPAREN:
			case RPAREN:
			case LITERAL_else:	// else is a child of "if" AST
			case COLON:		// part of the trinary operator
			case WS:		// whitespace
			case ESC:
			case HEX_DIGIT:
			case VOCAB:

			case EXPONENT:	// exponents and float suffixes are left in the NUM_FLOAT
			case FLOAT_SUFFIX
*/

		}


		stack.pop();
	}



	// A Precendence table. Here are operator precendences (from java.g):
	//    lowest  (13)  = *= /= %= += -= <<= >>= >>>= &= ^= |=
	//            (12)  ?:
	//            (11)  ||
	//            (10)  &&
	//            ( 9)  |
	//            ( 8)  ^
	//            ( 7)  &
	//            ( 6)  == !=
	//            ( 5)  < <= > >=
	//            ( 4)  << >>
	//            ( 3)  +(binary) -(binary)
	//            ( 2)  * / %
	//            ( 1)  ++ -- +(unary) -(unary)  ~  !  (type)
	//                  []   () (method call)  . (dot -- identifier qualification)
	//                  new   ()  (explicit parenthesis)

	private static int getPrecedence(AST ast) {
		if (ast == null) {
			return -2;				
		}
		switch (ast.getType()) {
			case EXPR:
				return getPrecedence(ast.getFirstChild());

			case ASSIGN:
			case PLUS_ASSIGN:
			case MINUS_ASSIGN:
			case STAR_ASSIGN:
			case DIV_ASSIGN:
			case MOD_ASSIGN:
			case SR_ASSIGN:
			case BSR_ASSIGN:
			case SL_ASSIGN:
			case BAND_ASSIGN:
			case BXOR_ASSIGN:
			case BOR_ASSIGN:
				return 13;

			case QUESTION:
				return 12;
			case LOR:
				return 11;
			case LAND:
				return 10;
			case BOR:
				return 9;
			case BXOR:
				return 8;
			case BAND:
				return 7;

			case NOT_EQUAL:
			case EQUAL:
				return 6;

			case LT:
			case GT:
			case LE:
			case GE:
			case LITERAL_instanceof: 
				return 5;

			case SL:
			case SR:
			case BSR:	// not in chart above, but I would guess it goes here
				return 4;

			case PLUS:
			case MINUS:
				return 3;

			case DIV:
			case MOD:
			case STAR:
				return 2;

			case INC:
			case DEC:
			case POST_INC:
			case POST_DEC:
			case UNARY_PLUS:
			case UNARY_MINUS:
			case LNOT:
			case BNOT:
			case TYPE:
				return 1;

			case METHOD_CALL:
			case ARRAY_DECLARATOR:
			case DOT:
				return 0;

			case LITERAL_new:
				return -1;

		}
		// for any non-operator, return a value which will cause it to NOT need parens.
		return -2;				
	}

	/**
	 * This example takes a single filename on the command line, prints
	 * the AST to stderr and the Java code to stdout.
	 */
	public static void main(String[] args) {
		if (args.length != 1) {
			System.err.println("Usage: java JavaEmitter MyFile.java");
			System.exit(1);
		}
		String fileName = args[0];
		File file = new File(fileName);
		if (!file.exists()) {
			System.err.println("File does not exist:" + fileName);
			System.exit(1);
		}
		if (!file.isFile()) {
			System.err.println("File is not a regular file:" + fileName);
			System.exit(1);
		}
		try {
			FileInputStream fis = new FileInputStream(fileName);
			// Create a scanner that reads from the input stream passed to us
			JavaLexer lexer = new JavaLexer(fis);
			lexer.setFilename(fileName);

			// Create a parser that reads from the scanner
			JavaRecognizer parser = new JavaRecognizer(lexer);
			parser.setFilename(fileName);

			// start parsing at the compilationUnit rule
			parser.compilationUnit();
			
			// Create a root AST node with id 0, and its child is the AST produced by the parser:
			ASTFactory factory = new ASTFactory();
			AST root = factory.create(ROOT_ID,"AST ROOT");
			root.setFirstChild(parser.getAST());

			// Look at the AST if you want, by uncommenting the line below:
			showTree(root, 0);

			// Print the AST as nice Java code:
			JavaEmitter emitter = new JavaEmitter();
			emitter.print(root);
		}
		catch (Exception e) {
			System.err.println("parser exception: "+e);
			e.printStackTrace();   // so we can get stack trace		
		}
	}


	// A simple method to print out an AST as a tree:
	private static String SPACES = "                                              ";
	private static void showTree(AST t, int level) {
		if ( t==null ) return;
		System.err.print(SPACES.substring(0, level));
		System.err.println("text:" + t.getText() + " type=" + t.getType());
		AST child = t.getFirstChild();
		showTree(child, level+2);
		AST next = t.getNextSibling();
		showTree(next, level);
	}
}
