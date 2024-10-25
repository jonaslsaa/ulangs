import { CharStream, CommonTokenStream }  from 'antlr4';
import SimpleLangLexer from './grammar/SimpleLangLexer';
import SimpleLangParser from './grammar/SimpleLangParser';

const input = `
def add(a, b):
    ret a + b

a = []

a.push(add(2, 4))
`.trim();
const chars = new CharStream(input); // replace this with a FileStream as required
const lexer = new SimpleLangLexer(chars);
const tokens = new CommonTokenStream(lexer);
const parser = new SimpleLangParser(tokens);
const tree = parser.program();



console.log(tree.toStringTree(null, parser));