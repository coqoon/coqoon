import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.StreamReader

import java.io.File
import java.io.FileInputStream
import java.io.InputStreamReader

object Productions extends Application with JavaParser
{
	val parserType = "scala.util.parsing.combinator.Parsers.Parser"
	val tpClass = this.getClass
	val tpMethods = tpClass.getMethods
	// val input = "foo bar baz"
	// val input = StreamReader(new InputStreamReader(new FileInputStream(new File("/tmp/baz")), "ISO-8859-1"))
	
	tpMethods foreach { x =>
		val returnType = x.getReturnType.getCanonicalName
		if (returnType == parserType) {
			println(x)

/*
			val pArgs = new Array[java.lang.Object](1)
			pArgs(0) = StreamReader(new InputStreamReader(new FileInputStream(new File("/tmp/baz")), "ISO-8859-1"))
			val result = x.invoke(this, pArgs)

			println(result) */
		}
	}
	
	// tp.phrase(block)(new lexical.Scanner(in)) match {
}