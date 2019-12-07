import java.io.{BufferedReader, File, FileReader}
import java.util
import java.util.Scanner

import scala.util.control._
import org.eclipse.jdt.core.dom.rewrite.{ASTRewrite, ListRewrite}
import org.eclipse.jdt.core.dom.{AST, ASTNode, ASTParser, ASTVisitor, AssertStatement, Assignment, Block, CompilationUnit, ExpressionStatement, ForStatement, IfStatement, MethodDeclaration, MethodInvocation, ReturnStatement, SimpleName, Statement, SwitchStatement, TextElement, TypeDeclaration, VariableDeclarationStatement, WhileStatement}
import org.eclipse.jface.text.{Document, IDocument}
import org.eclipse.text.edits.TextEdit
import org.apache.commons.io.FileUtils

object test1 extends App {
    val file = new File("/Users/sachinmb/project_cs474/src/main/scala/AddTwoNumbers.java")
    //val scanner = new Scanner(file)
    //theString = scanner.nextLine()
    //while (scanner.hasNextLine) {
    //  theString = theString + "\n" + scanner.nextLine()
    //}

    val br = new BufferedReader(new FileReader(file))
    val theString = LazyList.continually(br.readLine()).takeWhile(_ != null).mkString("\n")


    val source: Array[Char] = theString.toCharArray
    val parser = ASTParser.newParser(AST.JLS11)
    parser.setKind(ASTParser.K_COMPILATION_UNIT)
    parser.setSource(source)
    parser.setResolveBindings(true)
    val result = parser.createAST(null).asInstanceOf[CompilationUnit]
    val all = new util.ArrayList[Statement]
    val loop = new Breaks

    class exp extends ASTVisitor {
        // expression
        val expression = new util.ArrayList[ExpressionStatement]
        override def visit(node: ExpressionStatement): Boolean = {
            expression.add(node)
            super.visit(node)
        }
        def getExpression: util.ArrayList[ExpressionStatement] = expression
    }

    val ex = new exp
    result.accept(ex)
    ex.getExpression.forEach{e =>
        all.add(e)
    }

    class vName extends ASTVisitor {
        // variable name
        val variableName = new util.ArrayList[VariableDeclarationStatement]
        override def visit(node: VariableDeclarationStatement): Boolean = {
            variableName.add(node)
            super.visit(node)
        }
        def getVariable: util.ArrayList[VariableDeclarationStatement] = variableName
    }

    val vn = new vName
    result.accept(vn)
    vn.getVariable.forEach{e =>
      all.add(e)
    }

    class ret extends ASTVisitor {
        // return statement
        val returnStatement = new util.ArrayList[ReturnStatement]
        override def visit(node: ReturnStatement): Boolean = {
            returnStatement.add(node)
            super.visit(node)
        }
        def getReturn: util.ArrayList[ReturnStatement] = returnStatement
    }

    val r = new ret
    result.accept(r)
    r.getReturn.forEach{e =>
      all.add(e)
    }

    class whileState extends ASTVisitor{
        // while statement
        val whileStatement = new util.ArrayList[WhileStatement]
        override def visit(node: WhileStatement): Boolean = {
            whileStatement.add(node)
            super.visit(node)
        }
        def getWhile: util.ArrayList[WhileStatement] = whileStatement
    }

    val whi = new whileState
    result.accept(whi)
    whi.getWhile.forEach{e =>
        all.add(e)
    }


    class Switching extends ASTVisitor {
        // switch statement
        val switchStatement = new util.ArrayList[SwitchStatement]

        override def visit(node: SwitchStatement): Boolean = {
            switchStatement.add(node)
            super.visit(node)
        }

        def getSwitch: util.ArrayList[SwitchStatement] = switchStatement
    }
    val switches = new Switching
    result.accept(switches)
    switches.getSwitch.forEach{e =>
        all.add(e)
    }

    class IF extends ASTVisitor {
        val ifStatement = new util.ArrayList[IfStatement]
        override def visit(node: IfStatement): Boolean = {
            ifStatement.add(node)
            super.visit(node)
        }
        def getIF: util.ArrayList[IfStatement] = ifStatement
    }
    val iffy = new IF
    result.accept(iffy)
    iffy.getIF.forEach{e =>
        all.add(e)
    }

    class FOR extends ASTVisitor {
        // for statement
        val forStatement = new util.ArrayList[ForStatement]
        override def visit(node: ForStatement): Boolean = {
            forStatement.add(node)
            super.visit(node)
        }
        def getFor: util.ArrayList[ForStatement] = forStatement
    }
    val fr = new FOR
    result.accept(fr)
    fr.getFor.forEach{e =>
        all.add(e)
    }

    class minvoc extends ASTVisitor {
        // method invocation statement
        val methodInvocation = new util.ArrayList[MethodInvocation]
        override def visit(node: MethodInvocation): Boolean = {
            methodInvocation.add(node)
            super.visit(node)
        }
        def getInvoc: util.ArrayList[MethodInvocation] = methodInvocation
    }
    val mi = new minvoc
    result.accept(mi)

    class sName extends ASTVisitor {
        // simple name
        val simpleName = new util.ArrayList[SimpleName]
        override def visit(node: SimpleName): Boolean = {
            simpleName.add(node)
            super.visit(node)
        }
        def getSName: util.ArrayList[SimpleName] = simpleName
    }
    val sname = new sName
    result.accept(sname)

    // adding new nodes using AST rewrite
    val rewriter = ASTRewrite.create(result.getAST)
    val scope: Block = result.getAST.newBlock()

    all.forEach{state: Statement =>
      val lineNumber: Int = result.getLineNumber(state.getStartPosition)
      var c: Int = 0
      sname.getSName.forEach{sn: SimpleName =>
        if(result.getLineNumber(sn.getStartPosition) == lineNumber) {
            c += 1
            mi.getInvoc.forEach{meth: MethodInvocation =>
              if(result.getLineNumber(meth.getStartPosition) == lineNumber && sn.getIdentifier.equals(meth.getName.toString)) {
                  c -= 1

              }
            }
        }
      }

      if(c > 0) {
          var template: String = "template.instrum(\"" + result.getLineNumber(state.getStartPosition) + "\""
          val clas: String = state.getClass.getName
          if(clas == "org.eclipse.jdt.core.dom.VariableDeclarationStatement") {
              template = template + ", \"Variable Declaration\""
          } else if(clas == "org.eclipse.jdt.core.dom.ExpressionStatement") {
              template = template + ", \"Assignment Statement\""
          } else if(clas == "org.eclipse.jdt.core.dom.IfStatement") {
              template = template + ", \"If Statement\""
          } else if(clas == "org.eclipse.jdt.core.dom.ForStatement") {
              template = template + ", \"For Statement\""
          } else if(clas == "org.eclipse.jdt.core.dom.SwitchStatement") {
              template = template + ", \"Switch Statement\""
          } else if(clas == "org.eclipse.jdt.core.dom.WhileStatement") {
              template = template + ", \"While Statement\""
          }

          var check: Int = 0
          var count: Int = 0
          var state1: ASTNode = state.getParent
          var state2: String = state1.getClass.getName
          //print("Outside the loop (State 1): "+state1)
          //print("Outside the loop (State 2): "+state2)
          while(state2 != "org.eclipse.jdt.core.dom.MethodDeclaration") {
              if(state2.equals("org.eclipse.jdt.core.dom.TypeDeclaration")) {
                  check = 1

              }
              state1 = state1.getParent
              state2 = state1.getClass.getName
              //print("Inside the loop (State 1): "+state1)
              //print("Inside the loop (State 2): "+state2)

          }

          if(check == 0) {
              val md = state1.asInstanceOf[MethodDeclaration]
              val td = md.getParent.asInstanceOf[TypeDeclaration]

              sname.getSName.forEach{sn: SimpleName =>
                if(result.getLineNumber(sn.getStartPosition) == lineNumber) {
                    count = 0
                    mi.getInvoc.forEach{meth: MethodInvocation =>
                        if(result.getLineNumber(meth.getStartPosition) == lineNumber && sn.getIdentifier.equals(meth.getName.toString)) {
                            count = 1

                        }
                    }

                    if(count == 0) {
                        template = template + ", \"" + td.getName + "." + md.getName + "()." + sn.getIdentifier + ": \"," + sn.getIdentifier
                    }
                }
              }
          }

          else {
              val td = state1.isInstanceOf[TypeDeclaration]
              sname.getSName.forEach{sn: SimpleName =>
                  if(result.getLineNumber(sn.getStartPosition) == lineNumber) {
                      count = 0
                      mi.getInvoc.forEach{meth: MethodInvocation =>
                          if(result.getLineNumber(meth.getStartPosition) == lineNumber && sn.getIdentifier.equals(meth.getName.toString)) {
                              count = 1

                          }
                      }

                      if(count == 0) {
                          template = template + ", \"" + td.getClass.getName + "." + sn.getIdentifier
                      }
                  }
              }
          }

          template = template + ")"
          //println(template)
          val text: TextElement = result.getAST.newTextElement
          text.setText(template)

          val list1: ListRewrite = rewriter.getListRewrite(state.getParent, Block.STATEMENTS_PROPERTY)
          //println(clas)
          if(clas.equals("org.eclipse.jdt.core.dom.IfStatement") || clas.equals("org.eclipse.jdt.core.dom.ForStatement") || clas.equals("org.eclipse.jdt.core.dom.WhileStatement") || clas.equals("org.eclipse.jdt.core.dom.SwitchStatement")) {
              list1.insertBefore(text, state, null)
          }
          else {
              list1.insertAfter(text, state, null)
          }
      }
    }

    val doc: IDocument = new Document(FileUtils.readFileToString(new File(file.getAbsolutePath), "UTF-8"))
    val edits: TextEdit = rewriter.rewriteAST(doc, null)
    edits.apply(doc)
    val getValue: String = doc.get
    println(doc.get)
    //FileUtils.writeStringToFile(new File("/Users/sachinmb/project_cs474/src/main/scala/new.java"), getValue, "UTF-8")
}