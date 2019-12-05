import java.io.File
import java.util.Scanner

import org.eclipse.jdt.core.dom.{AST, ASTParser, ASTVisitor, CompilationUnit, MethodDeclaration, SimpleName, VariableDeclarationFragment}

object test1 extends App {
    var theString = ""
    val file = new File("/Users/sachinmb/project_cs474/src/main/scala/AddTwoNumbers.java")
    val scanner = new Scanner(file)
    theString = scanner.nextLine()
    while (scanner.hasNextLine) {
      theString = theString + "\n" + scanner.nextLine()
    }
    val source: Array[Char] = theString.toCharArray
    val parser = ASTParser.newParser(AST.JLS11)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setSource(source)
    parser.setResolveBindings(true)
    val result = parser.createAST(null).asInstanceOf[CompilationUnit]

    result.accept(new ASTVisitor() {
        override def visit(node: VariableDeclarationFragment): Boolean = {
            val name: SimpleName = node.getName
            val lineNumber = result.getLineNumber(name.getStartPosition)

            println("Name: " + name.toString)
            println("Line: " + lineNumber)
            println("------------------------")
            false
        }

        override def visit(node: MethodDeclaration): Boolean = {
            
            false
        }
    })
}