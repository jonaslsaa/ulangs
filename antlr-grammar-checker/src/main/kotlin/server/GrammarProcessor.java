package server;

import SimpleLogger.LOGGER;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;
import org.antlr.runtime.RecognitionException;
import org.antlr.v4.Tool;
import org.antlr.v4.gui.Interpreter;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.misc.IntegerList;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.Tree;
import org.antlr.v4.tool.*;

import static org.antlr.v4.gui.Interpreter.profilerColumnNames;

import java.io.*;
import java.util.Arrays;


public class GrammarProcessor {
    public static final int MAX_PARSE_TIME_MS = 10 * 1000; // 10 seconds
    public static final int MAX_TREE_SIZE_IN_NODES = 50_000;

    private static class KillableGrammarParserInterpreter extends GrammarParserInterpreter {
        private final long creationTime = System.currentTimeMillis();
        protected String startRule;

        public KillableGrammarParserInterpreter(Grammar g,
                                                ATN deserializedATN,
                                                String startRule,
                                                TokenStream tokenStream) {
            super(g, deserializedATN, tokenStream);
            this.startRule = startRule;
        }

        @Override
        protected void visitState(ATNState p) {
            super.visitState(p);
            long now = System.currentTimeMillis();
            long runTimeMs = now - creationTime;
            if (runTimeMs > MAX_PARSE_TIME_MS) {
                String msg = "Parser timeout (" + MAX_PARSE_TIME_MS + "ms) in rule " + startRule;
                throw new ParseCancellationException(msg);
            }
        }
    }

    /**
     * Interpret the input according to the grammar, starting at the start rule, and return a JSON object
     * with errors, tokens, rule names, and the parse tree.
     */
    public static JsonObject interp(String grammar, String lexGrammar, String input, String startRule)
            throws IOException {
        startRule = startRule.strip();
        Grammar g = null;
        LexerGrammar lg = null;
        Tool antlrTool = new Tool();
        ErrorManager errMgr = new ErrorManager(antlrTool);
        errMgr.setFormat("antlr");
        CollectGrammarErrorsAndWarnings parselistener = new CollectParserGrammarErrorsAndWarnings(errMgr);
        CollectGrammarErrorsAndWarnings lexlistener = new CollectLexerGrammarErrorsAndWarnings(errMgr);
        final JsonArray warnings = new JsonArray();
        try {
            if (lexGrammar != null && !lexGrammar.isBlank()) {
                lg = new LexerGrammar(lexGrammar, lexlistener);
                g = new IgnoreTokenVocabGrammar(null, grammar, lg, parselistener);
            }
            else {
                g = new IgnoreTokenVocabGrammar(null, grammar, null, parselistener);
            }

            warnings.addAll(lexlistener.warnings);
            warnings.addAll(parselistener.warnings);
        }
        catch (RecognitionException re) {
            // shouldn't get here.
            LOGGER.info("Can't parse grammar");
        }

        JsonObject result = new JsonObject();

        Rule r = g.rules.get(startRule);
        if (r == null) {
            String w = "No such start rule: " + startRule;
            LOGGER.error(w);
            final JsonObject jsonError = new JsonObject();
            jsonError.addProperty("msg", w);
            warnings.add(jsonError);
        }
        else {
            if (lexlistener.errors.isEmpty() && parselistener.errors.isEmpty()) {
                result = parseAndGetJSON(g, lg, startRule, input);
            }
        }

        final JsonObject jsonResponse = new JsonObject();
        jsonResponse.add("warnings", warnings);
        jsonResponse.add("parser_grammar_errors", parselistener.errors);
        jsonResponse.add("lexer_grammar_errors", lexlistener.errors);
        jsonResponse.add("result", result);
        return jsonResponse;
    }

    private static JsonObject parseAndGetJSON(Grammar g, LexerGrammar lg, String startRule, String input)
            throws IOException
    {
        CharStream charStream = CharStreams.fromStream(new StringBufferInputStream(input));

        LexerInterpreter lexEngine = (lg != null) ?
                lg.createLexerInterpreter(charStream) :
                g.createLexerInterpreter(charStream);

        CollectLexOrParseSyntaxErrors lexListener = new CollectLexOrParseSyntaxErrors();
        lexEngine.removeErrorListeners();
        lexEngine.addErrorListener(lexListener);

        CommonTokenStream tokens = new CommonTokenStream(lexEngine);

        tokens.fill();

        KillableGrammarParserInterpreter parser = createGrammarParserInterpreter(g, startRule, tokens);

        CollectLexOrParseSyntaxErrors parseListener = new CollectLexOrParseSyntaxErrors();
        parser.removeErrorListeners();
        parser.addErrorListener(parseListener);
        parser.setProfile(true);

        Rule r = g.rules.get(startRule);
        ParseTree t = parser.parse(r.index);
        ParseInfo parseInfo = parser.getParseInfo();

        int n = nodeCount(t);
        if ( n > MAX_TREE_SIZE_IN_NODES ) {
            String msg = "Tree size "+n+" nodes > max of "+MAX_TREE_SIZE_IN_NODES;
            throw new ParseCancellationException(msg);
        }

        long now = System.currentTimeMillis();
//        LOGGER.info("PARSE TIME: "+(now - parser.creationTime)+"ms");

//        System.out.println("lex msgs" + lexListener.msgs);
//        System.out.println("parse msgs" + parseListener.msgs);
//
//        System.out.println(t.toStringTree(parser));
        String[][] profileData = getProfilerTable(parser, parseInfo);

        TokenStream tokenStream = parser.getInputStream();
//        CharStream inputStream = tokenStream.getTokenSource().getInputStream();
        CharStream inputStream = null; // don't send input back to client (they have it and it can be big)
        return JsonSerializer.toJSON(
                t,
                Arrays.asList(parser.getRuleNames()),
                parser.getVocabulary(),
                tokenStream,
                inputStream,
                lexListener.msgs,
                parseListener.msgs,
                profileData);
    }

    /**
     * Copy this function from {@link Grammar} so we can override {@link ParserInterpreter#visitState(ATNState)}
     */
    public static KillableGrammarParserInterpreter createGrammarParserInterpreter(Grammar g,
                                                                                  String startRule,
                                                                                  TokenStream tokenStream) {
        if (g.isLexer()) {
            throw new IllegalStateException("A parser interpreter can only be created for a parser or combined grammar.");
        }
        // must run ATN through serializer to set some state flags
        IntegerList serialized = ATNSerializer.getSerialized(g.getATN());
        ATN deserializedATN = new ATNDeserializer().deserialize(serialized.toArray());

        return new KillableGrammarParserInterpreter(g, deserializedATN, startRule, tokenStream);
    }


    private static String[][] getProfilerTable(GrammarParserInterpreter parser, ParseInfo parseInfo) {
        String[] ruleNamesByDecision = new String[parser.getATN().decisionToState.size()];
        for (int i = 0; i < ruleNamesByDecision.length; i++) {
            ruleNamesByDecision[i] = parser.getRuleNames()[parser.getATN().getDecisionState(i).ruleIndex];
        }

        DecisionInfo[] decisionInfo = parseInfo.getDecisionInfo();
        String[][] table = new String[decisionInfo.length][profilerColumnNames.length];

        for (int decision = 0; decision < decisionInfo.length; decision++) {
            for (int col = 0; col < profilerColumnNames.length; col++) {
                Object colVal = Interpreter.getValue(decisionInfo[decision], ruleNamesByDecision, decision, col);
                table[decision][col] = colVal.toString();
            }
        }

        return table;
    }

    public static int nodeCount(Tree t) {
        if (t == null) {
            return 0;
        }
        int n = 1;
        for (int i = 0; i < t.getChildCount(); i++) {
            n += nodeCount(t.getChild(i));
        }
        return n;
    }
}
