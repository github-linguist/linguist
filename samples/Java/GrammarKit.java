// This is a generated file. Not intended for manual editing.
package org.intellij.grammar.parser;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilder.Marker;
import static org.intellij.grammar.psi.BnfTypes.*;
import static org.intellij.grammar.parser.GeneratedParserUtilBase.*;
import com.intellij.psi.tree.IElementType;
import com.intellij.lang.ASTNode;
import com.intellij.psi.tree.TokenSet;
import com.intellij.lang.PsiParser;
import com.intellij.lang.LightPsiParser;

@SuppressWarnings({"SimplifiableIfStatement", "UnusedAssignment"})
public class GrammarParser implements PsiParser, LightPsiParser {

  public ASTNode parse(IElementType t, PsiBuilder b) {
    parseLight(t, b);
    return b.getTreeBuilt();
  }

  public void parseLight(IElementType t, PsiBuilder b) {
    boolean r;
    b = adapt_builder_(t, b, this, EXTENDS_SETS_);
    Marker m = enter_section_(b, 0, _COLLAPSE_, null);
    if (t == BNF_ATTR) {
      r = attr(b, 0);
    }
    else if (t == BNF_ATTR_PATTERN) {
      r = attr_pattern(b, 0);
    }
    else if (t == BNF_ATTR_VALUE) {
      r = attr_value(b, 0);
    }
    else if (t == BNF_ATTRS) {
      r = attrs(b, 0);
    }
    else if (t == BNF_CHOICE) {
      r = choice(b, 0);
    }
    else if (t == BNF_EXPRESSION) {
      r = expression(b, 0);
    }
    else if (t == BNF_LITERAL_EXPRESSION) {
      r = literal_expression(b, 0);
    }
    else if (t == BNF_MODIFIER) {
      r = modifier(b, 0);
    }
    else if (t == BNF_PAREN_EXPRESSION) {
      r = paren_expression(b, 0);
    }
    else if (t == BNF_PREDICATE) {
      r = predicate(b, 0);
    }
    else if (t == BNF_PREDICATE_SIGN) {
      r = predicate_sign(b, 0);
    }
    else if (t == BNF_QUANTIFIED) {
      r = quantified(b, 0);
    }
    else if (t == BNF_QUANTIFIER) {
      r = quantifier(b, 0);
    }
    else if (t == BNF_REFERENCE_OR_TOKEN) {
      r = reference_or_token(b, 0);
    }
    else if (t == BNF_RULE) {
      r = rule(b, 0);
    }
    else if (t == BNF_SEQUENCE) {
      r = sequence(b, 0);
    }
    else if (t == BNF_STRING_LITERAL_EXPRESSION) {
      r = string_literal_expression(b, 0);
    }
    else {
      r = parse_root_(t, b, 0);
    }
    exit_section_(b, 0, m, t, r, true, TRUE_CONDITION);
  }

  protected boolean parse_root_(IElementType t, PsiBuilder b, int l) {
    return grammar(b, l + 1);
  }

  public static final TokenSet[] EXTENDS_SETS_ = new TokenSet[] {
    create_token_set_(BNF_LITERAL_EXPRESSION, BNF_STRING_LITERAL_EXPRESSION),
    create_token_set_(BNF_CHOICE, BNF_EXPRESSION, BNF_LITERAL_EXPRESSION, BNF_PAREN_EXPRESSION,
      BNF_PREDICATE, BNF_QUANTIFIED, BNF_REFERENCE_OR_TOKEN, BNF_SEQUENCE,
      BNF_STRING_LITERAL_EXPRESSION),
  };

  /* ********************************************************** */
  // id attr_pattern? '=' attr_value ';'?
  public static boolean attr(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attr")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<attr>");
    r = consumeToken(b, BNF_ID);
    p = r; // pin = 1
    r = r && report_error_(b, attr_1(b, l + 1));
    r = p && report_error_(b, consumeToken(b, BNF_OP_EQ)) && r;
    r = p && report_error_(b, attr_value(b, l + 1)) && r;
    r = p && attr_4(b, l + 1) && r;
    exit_section_(b, l, m, BNF_ATTR, r, p, attr_recover_until_parser_);
    return r || p;
  }

  // attr_pattern?
  private static boolean attr_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attr_1")) return false;
    attr_pattern(b, l + 1);
    return true;
  }

  // ';'?
  private static boolean attr_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attr_4")) return false;
    consumeToken(b, BNF_SEMICOLON);
    return true;
  }

  /* ********************************************************** */
  // '(' string ')'
  public static boolean attr_pattern(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attr_pattern")) return false;
    if (!nextTokenIs(b, BNF_LEFT_PAREN)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BNF_LEFT_PAREN);
    r = r && consumeToken(b, BNF_STRING);
    r = r && consumeToken(b, BNF_RIGHT_PAREN);
    exit_section_(b, m, BNF_ATTR_PATTERN, r);
    return r;
  }

  /* ********************************************************** */
  // !'}'
  static boolean attr_recover_until(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attr_recover_until")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !consumeToken(b, BNF_RIGHT_BRACE);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // (reference_or_token | literal_expression) !'='
  public static boolean attr_value(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attr_value")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<attr value>");
    r = attr_value_0(b, l + 1);
    r = r && attr_value_1(b, l + 1);
    exit_section_(b, l, m, BNF_ATTR_VALUE, r, false, null);
    return r;
  }

  // reference_or_token | literal_expression
  private static boolean attr_value_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attr_value_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = reference_or_token(b, l + 1);
    if (!r) r = literal_expression(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // !'='
  private static boolean attr_value_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attr_value_1")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !consumeToken(b, BNF_OP_EQ);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '{' attr* '}'
  public static boolean attrs(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attrs")) return false;
    if (!nextTokenIs(b, BNF_LEFT_BRACE)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, BNF_LEFT_BRACE);
    p = r; // pin = 1
    r = r && report_error_(b, attrs_1(b, l + 1));
    r = p && consumeToken(b, BNF_RIGHT_BRACE) && r;
    exit_section_(b, l, m, BNF_ATTRS, r, p, null);
    return r || p;
  }

  // attr*
  private static boolean attrs_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "attrs_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!attr(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "attrs_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // '{' sequence ('|' sequence)* '}' | sequence choice_tail*
  public static boolean choice(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "choice")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, "<choice>");
    r = choice_0(b, l + 1);
    if (!r) r = choice_1(b, l + 1);
    exit_section_(b, l, m, BNF_CHOICE, r, false, null);
    return r;
  }

  // '{' sequence ('|' sequence)* '}'
  private static boolean choice_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "choice_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BNF_LEFT_BRACE);
    r = r && sequence(b, l + 1);
    r = r && choice_0_2(b, l + 1);
    r = r && consumeToken(b, BNF_RIGHT_BRACE);
    exit_section_(b, m, null, r);
    return r;
  }

  // ('|' sequence)*
  private static boolean choice_0_2(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "choice_0_2")) return false;
    int c = current_position_(b);
    while (true) {
      if (!choice_0_2_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "choice_0_2", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // '|' sequence
  private static boolean choice_0_2_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "choice_0_2_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BNF_OP_OR);
    r = r && sequence(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // sequence choice_tail*
  private static boolean choice_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "choice_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = sequence(b, l + 1);
    r = r && choice_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // choice_tail*
  private static boolean choice_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "choice_1_1")) return false;
    int c = current_position_(b);
    while (true) {
      if (!choice_tail(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "choice_1_1", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // '|' sequence
  static boolean choice_tail(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "choice_tail")) return false;
    if (!nextTokenIs(b, BNF_OP_OR)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, BNF_OP_OR);
    p = r; // pin = 1
    r = r && sequence(b, l + 1);
    exit_section_(b, l, m, null, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // choice?
  public static boolean expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "expression")) return false;
    Marker m = enter_section_(b, l, _COLLAPSE_, "<expression>");
    choice(b, l + 1);
    exit_section_(b, l, m, BNF_EXPRESSION, true, false, null);
    return true;
  }

  /* ********************************************************** */
  // (attrs | rule) *
  static boolean grammar(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "grammar")) return false;
    int c = current_position_(b);
    while (true) {
      if (!grammar_0(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "grammar", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // attrs | rule
  private static boolean grammar_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "grammar_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = attrs(b, l + 1);
    if (!r) r = rule(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // string_literal_expression | number
  public static boolean literal_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "literal_expression")) return false;
    if (!nextTokenIs(b, "<literal expression>", BNF_NUMBER, BNF_STRING)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, "<literal expression>");
    r = string_literal_expression(b, l + 1);
    if (!r) r = consumeToken(b, BNF_NUMBER);
    exit_section_(b, l, m, BNF_LITERAL_EXPRESSION, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // 'private' | 'external' | 'wrapped'
  public static boolean modifier(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "modifier")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<modifier>");
    r = consumeToken(b, "private");
    if (!r) r = consumeToken(b, "external");
    if (!r) r = consumeToken(b, "wrapped");
    exit_section_(b, l, m, BNF_MODIFIER, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // quantified | predicate
  static boolean option(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "option")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = quantified(b, l + 1);
    if (!r) r = predicate(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  /* ********************************************************** */
  // '(' expression ')'
  public static boolean paren_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "paren_expression")) return false;
    if (!nextTokenIs(b, BNF_LEFT_PAREN)) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, null);
    r = consumeToken(b, BNF_LEFT_PAREN);
    p = r; // pin = 1
    r = r && report_error_(b, expression(b, l + 1));
    r = p && consumeToken(b, BNF_RIGHT_PAREN) && r;
    exit_section_(b, l, m, BNF_PAREN_EXPRESSION, r, p, null);
    return r || p;
  }

  /* ********************************************************** */
  // predicate_sign  simple
  public static boolean predicate(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "predicate")) return false;
    if (!nextTokenIs(b, "<predicate>", BNF_OP_NOT, BNF_OP_AND)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<predicate>");
    r = predicate_sign(b, l + 1);
    r = r && simple(b, l + 1);
    exit_section_(b, l, m, BNF_PREDICATE, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '&' | '!'
  public static boolean predicate_sign(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "predicate_sign")) return false;
    if (!nextTokenIs(b, "<predicate sign>", BNF_OP_NOT, BNF_OP_AND)) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<predicate sign>");
    r = consumeToken(b, BNF_OP_AND);
    if (!r) r = consumeToken(b, BNF_OP_NOT);
    exit_section_(b, l, m, BNF_PREDICATE_SIGN, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // '[' expression ']' | simple quantifier?
  public static boolean quantified(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "quantified")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, "<quantified>");
    r = quantified_0(b, l + 1);
    if (!r) r = quantified_1(b, l + 1);
    exit_section_(b, l, m, BNF_QUANTIFIED, r, false, null);
    return r;
  }

  // '[' expression ']'
  private static boolean quantified_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "quantified_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BNF_LEFT_BRACKET);
    r = r && expression(b, l + 1);
    r = r && consumeToken(b, BNF_RIGHT_BRACKET);
    exit_section_(b, m, null, r);
    return r;
  }

  // simple quantifier?
  private static boolean quantified_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "quantified_1")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = simple(b, l + 1);
    r = r && quantified_1_1(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // quantifier?
  private static boolean quantified_1_1(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "quantified_1_1")) return false;
    quantifier(b, l + 1);
    return true;
  }

  /* ********************************************************** */
  // '?' | '+' | '*'
  public static boolean quantifier(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "quantifier")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NONE_, "<quantifier>");
    r = consumeToken(b, BNF_OP_OPT);
    if (!r) r = consumeToken(b, BNF_OP_ONEMORE);
    if (!r) r = consumeToken(b, BNF_OP_ZEROMORE);
    exit_section_(b, l, m, BNF_QUANTIFIER, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // id
  public static boolean reference_or_token(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "reference_or_token")) return false;
    if (!nextTokenIs(b, BNF_ID)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BNF_ID);
    exit_section_(b, m, BNF_REFERENCE_OR_TOKEN, r);
    return r;
  }

  /* ********************************************************** */
  // modifier* id '::=' expression attrs? ';'?
  public static boolean rule(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rule")) return false;
    boolean r, p;
    Marker m = enter_section_(b, l, _NONE_, "<rule>");
    r = rule_0(b, l + 1);
    r = r && consumeToken(b, BNF_ID);
    r = r && consumeToken(b, BNF_OP_IS);
    p = r; // pin = 3
    r = r && report_error_(b, expression(b, l + 1));
    r = p && report_error_(b, rule_4(b, l + 1)) && r;
    r = p && rule_5(b, l + 1) && r;
    exit_section_(b, l, m, BNF_RULE, r, p, rule_recover_until_parser_);
    return r || p;
  }

  // modifier*
  private static boolean rule_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rule_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!modifier(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "rule_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  // attrs?
  private static boolean rule_4(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rule_4")) return false;
    attrs(b, l + 1);
    return true;
  }

  // ';'?
  private static boolean rule_5(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rule_5")) return false;
    consumeToken(b, BNF_SEMICOLON);
    return true;
  }

  /* ********************************************************** */
  // !'{'
  static boolean rule_recover_until(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "rule_recover_until")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !consumeToken(b, BNF_LEFT_BRACE);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // option +
  public static boolean sequence(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "sequence")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _COLLAPSE_, "<sequence>");
    r = option(b, l + 1);
    int c = current_position_(b);
    while (r) {
      if (!option(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "sequence", c)) break;
      c = current_position_(b);
    }
    exit_section_(b, l, m, BNF_SEQUENCE, r, false, null);
    return r;
  }

  /* ********************************************************** */
  // !(modifier* id '::=' ) reference_or_token | literal_expression | paren_expression
  static boolean simple(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simple")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = simple_0(b, l + 1);
    if (!r) r = literal_expression(b, l + 1);
    if (!r) r = paren_expression(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // !(modifier* id '::=' ) reference_or_token
  private static boolean simple_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simple_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = simple_0_0(b, l + 1);
    r = r && reference_or_token(b, l + 1);
    exit_section_(b, m, null, r);
    return r;
  }

  // !(modifier* id '::=' )
  private static boolean simple_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simple_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b, l, _NOT_, null);
    r = !simple_0_0_0(b, l + 1);
    exit_section_(b, l, m, null, r, false, null);
    return r;
  }

  // modifier* id '::='
  private static boolean simple_0_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simple_0_0_0")) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = simple_0_0_0_0(b, l + 1);
    r = r && consumeToken(b, BNF_ID);
    r = r && consumeToken(b, BNF_OP_IS);
    exit_section_(b, m, null, r);
    return r;
  }

  // modifier*
  private static boolean simple_0_0_0_0(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "simple_0_0_0_0")) return false;
    int c = current_position_(b);
    while (true) {
      if (!modifier(b, l + 1)) break;
      if (!empty_element_parsed_guard_(b, "simple_0_0_0_0", c)) break;
      c = current_position_(b);
    }
    return true;
  }

  /* ********************************************************** */
  // string
  public static boolean string_literal_expression(PsiBuilder b, int l) {
    if (!recursion_guard_(b, l, "string_literal_expression")) return false;
    if (!nextTokenIs(b, BNF_STRING)) return false;
    boolean r;
    Marker m = enter_section_(b);
    r = consumeToken(b, BNF_STRING);
    exit_section_(b, m, BNF_STRING_LITERAL_EXPRESSION, r);
    return r;
  }

  final static Parser attr_recover_until_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return attr_recover_until(b, l + 1);
    }
  };
  final static Parser rule_recover_until_parser_ = new Parser() {
    public boolean parse(PsiBuilder b, int l) {
      return rule_recover_until(b, l + 1);
    }
  };
}
