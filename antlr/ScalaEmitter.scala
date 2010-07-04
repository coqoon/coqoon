/* */

import org.antlr.runtime.tree._
import org.antlr.runtime._
import scala.collection.jcl._
import scala.collection.jcl.Conversions._

object JavaEmitter
{
	def main(args: Array[String]) = {
		if (args.length != 1) {
			System.err.println("Usage: scala ScalaEmitter MyFile.java");
			System.exit(1);
		}
		
		val fileName = args(0)
		val file = new File(fileName)
		
		if (!file.exists()) {
			System.err.println("File does not exist:" + fileName);
			System.exit(1);
		}
		if (!file.isFile()) {
			System.err.println("File is not a regular file:" + fileName);
			System.exit(1);
		}

		// val fis = new FileInputStream(fileName);
		// val fis = new ANTLRFileStream(fileName);
		val fis = new ANTLRFileStream("../TSParser.java");
		
		// Create a scanner that reads from the input stream passed to us
		val lexer = new JavaLexer(fis);
		// lexer.setFilename(fileName);

		// Create a parser that reads from the scanner
		val tokens = new CommonTokenStream(lexer)
		val javaTokenList = tokens.getTokens
		val tokenList = javaTokenList.asInstanceOf[java.util.List[Token]]
		
		// val tokens = new TokenRewriteStream(lexer)
		val parser = new JavaParser(tokens)
		// parser.setFilename(fileName);

		// JavaTreeParser.g
		val jReturn = parser.javaSource().asInstanceOf[JavaParser.javaSource_return]
		val tree = jReturn.getTree.asInstanceOf[CommonTree]
		val nodes = new CommonTreeNodeStream(tree)
		nodes.setTokenStream(tokens)
		val walker = new JavaTreeParser(nodes)
		walker.javaSource
		System.out.println(tokens.toString)

		// Print the AST as nice Java code:
		// val emitter = new ScalaEmitter
		// emitter.print(walker);
	}
}