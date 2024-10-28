package org.example
import server.GrammarProcessor
import java.nio.file.Files
import java.nio.file.Path

fun main() {
    val base = "/Users/jonassilva/Desktop/simple-lang/grammar/"
    val codeBase = "/Users/jonassilva/Desktop/simple-lang/example-code/py-like/"
    val parserContent = Files.readString(Path.of(base + "SimpleLangParser.g4"))
    val lexerContent = Files.readString(Path.of(base + "SimpleLangLexer.g4"))
    val input = Files.readString(Path.of(codeBase + "func_def_call.pyl"))
    val json = GrammarProcessor.interp(parserContent, lexerContent, input, "program")

    println(json)
}