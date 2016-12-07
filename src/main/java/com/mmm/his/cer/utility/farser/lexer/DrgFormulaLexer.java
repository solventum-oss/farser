package com.mmm.his.cer.utility.farser.lexer;

import java.util.*;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

/**
 *
 *
 * @author a30w4zz
 */
public class DrgFormulaLexer {
    private static ArrayList<String> operators = new ArrayList<>();
    private static ArrayList<Character> specialCaseChars = new ArrayList<>();
    static {
        Collections.addAll(operators, "~", "&", "|", "-", "*", ":", "(", ")", "$0", "$1");
        Collections.addAll(specialCaseChars, ':', '~', '-', '*', '_');
    }
    private static Pattern prefixDelimiter = Pattern.compile(":");

    /**
     * Major types of tokens that we need to be concerned with when Lexing a DRG formula
     */
    public enum Type {
        LPAREN, RPAREN, ATOM, AND, OR
    }

    /**
     * Token class that can be used to represent the pieces of a DRG formula.
     */
    public static class Token {
        public final Type type;
        public final String value;
        public final Optional<String> prefix;

        public Token(Type type, String value) {
            this.type = type;
            this.value = value;
            this.prefix = Optional.empty();
        }

        public Token(Type type, String value, Optional<String> prefix) {
            this.type = type;
            this.value = value;
            this.prefix = prefix;
        }

        public String toString() {
            if (type == Type.ATOM) {
                return "ATOM<" + value + ">" + (prefix.isPresent() ? " with prefix: " + prefix.get() : "");
            }
            return type.toString();
        }
    }

    /**
     * Given a String and an Index, get the value of the string. Skip known operator values.
     * @param s {@link String} from which to extract the substring.
     * @param i the index at which we will start the substring process.
     * @return the value of the substring
     */
    private static String getAtom(String s, int i) {
        int j = i;
        for (; j < s.length(); ) {
            if (Character.isLetterOrDigit(s.charAt(j)) || specialCaseChars.contains(s.charAt(j))) {
                j++;
            } else {
                return s.substring(i, j);
            }
        }
        return s.substring(i, j);
    }

    /**
     * Method to perform our Lexical analysis.
     * @param input {@link String} to seperate out into tokens.
     * @return List of {@link Token} that were created from the input string.
     */
    public static List<Token> lex(String input) {
        List<Token> result = new ArrayList<>();
        for (int i = 0; i < input.length(); ) {
            switch (input.charAt(i)) {
                case '(':
                    result.add(new Token(Type.LPAREN, "("));
                    i++;
                    break;
                case ')':
                    result.add(new Token(Type.RPAREN, ")"));
                    i++;
                    break;
                case '|':
                    result.add(new Token(Type.OR, "|"));
                    i++;
                    break;
                case '&':
                    result.add(new Token(Type.AND, "&"));
                    i++;
                    break;
                default:
                    if (Character.isWhitespace(input.charAt(i))) {
                        i++;
                    } else {
                        String atom = getAtom(input, i);
                        i += atom.length();
                        result.add(new Token(Type.ATOM, parseListName(atom).get(), parsePrefix(atom)));
                    }
                    break;
            }
        }
        return result;
    }

    /**
     * Given an string, figure out and return the prefix.
     * @param atom {@link String} to analyze for prefix
     * @return Optional String value for the prefix
     */
    private static Optional<String> parsePrefix(String atom) {
        if (!atom.contains(":")) {
            if (atom.startsWith("~")) {
                return Optional.of("negated");
            } else {
                return Optional.empty();
            }
        }
        return prefixDelimiter.splitAsStream(atom).findFirst();
    }

    /**
     * Parse the list name from a given atom. Ignores any prefix if present
     * @param atom {@link String} to get the list name from.
     * @return Optional String value for the list name
     */
    private static Optional<String> parseListName(String atom) {
        if (!atom.contains(":")) {
            if (atom.startsWith("~")) {
                return Optional.of(atom.substring(1));
            } else {
                return Optional.of(atom);
            }
        }
        return Optional.ofNullable(prefixDelimiter.splitAsStream(atom).reduce((a, b) -> b).orElse(null));
    }

    /**
     * Get only the list names from a list of Tokens, this ignores all other types of tokens aside from 'ATOM'
     * @param tokens the List of tokens to filter
     * @return List of strings that only contain list names.
     */
    public static List<String> getListNames(List<Token> tokens) {
        return tokens.stream()
                .filter(token -> token.type == DrgFormulaLexer.Type.ATOM)
                .map(token -> token.value)
                .collect(Collectors.toList());
    }
}
