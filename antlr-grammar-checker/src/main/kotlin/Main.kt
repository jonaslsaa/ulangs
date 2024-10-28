package org.example

import server.GrammarProcessor
import java.nio.file.Files
import java.nio.file.Path
import kotlin.system.exitProcess

fun main(args: Array<String>) {
    if (args.size != 3 && args.size != 4) {
        println(
            """
            Usage: grammar-checker <parserFilePath> <lexerFilePath> <inputFilePath>
            
            Arguments:
              <parserFilePath>   Absolute path to the parser file (e.g., "MyParser.g4")
              <lexerFilePath>    Absolute path to the lexer file (e.g., "MyLexer.g4")
              <inputFilePath>    Absolute path to the input code file (e.g., "some_code.py")
              [startRule]        Optional. The start rule of the parser. Default is "program"
            """.trimIndent()
        )
        exitProcess(1)
    }

    val parserFilePath = args[0]
    val lexerFilePath = args[1]
    val inputFilePath = args[2]
    val startRule = if (args.size == 4) args[3].trim() else "program"

    fun assertPathExists(path: String) {
        if (!Files.exists(Path.of(path))) {
            println("File not found: $path")
            exitProcess(1)
        }
    }

    assertPathExists(parserFilePath)
    assertPathExists(lexerFilePath)
    assertPathExists(inputFilePath)

    try {
        val parserContent = Files.readString(Path.of(parserFilePath))
        val lexerContent = Files.readString(Path.of(lexerFilePath))
        val inputContent = Files.readString(Path.of(inputFilePath))

        val json = GrammarProcessor.interp(parserContent, lexerContent, inputContent, startRule)
        println(json)
    } catch (e: Exception) {
        println("Error reading files: ${e.message}")
        exitProcess(1)
    }
}