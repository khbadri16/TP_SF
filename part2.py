import ply.lex as lex
import ply.yacc as yacc


tokens = ("STARTUML", "ENDUML", "COLON", "RIGHT_ARROW_1", "RIGHT_ARROW_2", "ACTOR", "ID", "AS", "USECASE", "STRING",
          "PACKAGE", "LBRACE", "RBRACE", "INHERIT", "STEREO", "INCLUDES", "EXTENDS", "ACTOR_TXT", "USE_CASE_TXT", "EOL")

reserved = {"actor": "ACTOR", "as": "AS", "usecase": "USECASE",
            "package": "PACKAGE", "includes": "INCLUDES", "extends": "EXTENDS"}

t_STARTUML = "@startuml"
t_ENDUML = "@enduml"
t_COLON = ":"
t_RIGHT_ARROW_1 = "-+>"
t_RIGHT_ARROW_2 = r"\.+>"
t_LBRACE = r"\{"
t_RBRACE = r"\}"
t_INHERIT = r"<\|--"
t_EOL = r"\n"


def t_STRING(t):
    r'"[^"]*"'
    t.value = t.value[1:-1]
    return t


def t_STEREO(t):
    # We added a - in the regular expression according to your first example: :Main Admin: as Admin <<Not-a-machine>>
    r"<< [a-zA-Z_][a-zA-Z_0-9-] *>>"
    t.value = t.value[3:-3]
    return t


def t_ID(t):
    r"[a-zA-Z_][a-zA-Z_0-9]*"
    if t.value in reserved.keys():
        t.type = reserved[t.value]
    return t


def t_ACTOR_TXT(t):
    ":[^ :\n][^\n:]*:"
    t.value = t.value[1:-1]
    return t


def t_USE_CASE_TXT(t):
    r"\([^ \(\n][^)\n:]*\)"
    t.value = t.value[1:-1]
    return t


t_ignore = " \t"


def t_error(t):
    raise ValueError(f"Unexpected symbol {t}")


lexer = lex.lex()


def p_start(p):
    """start : eols STARTUML name EOL defs ENDUML eols"""
    p[0] = ('Start =>', p[2], p[3], p[5], p[6])


def p_eols(p):
    """eols : EOL eols
            | empty"""
    if len(p) == 3:
        p[0] = ('eols = >', p[1])
    else:
        p[0] = 'epsilon'


def p_name(p):
    """name : ID
             | empty"""
    p[0] = p[1]


def p_defs(p):
    """defs : one_def EOL
             | defs one_def EOL"""
    if len(p) == 3:
        # [] Pour dire que tout est dans  système {e.g  nom du diagramme}
        p[0] = ['defs =>', p[1]]
    else:
        p[0] = p[1] + [p[2]]


def p_one_def(p):
    """one_def : ACTOR def_act alias stereo
                | ACTOR_TXT alias stereo
                | USECASE def_uc alias stereo
                | USE_CASE_TXT alias stereo
                | var arrow var ucl_link
                | var INHERIT var
                | PACKAGE ID LBRACE defs RBRACE
                | empty"""
    if len(p) == 5:
        p[0] = ('one_def =>', p[1], p[2], p[3], p[4])
    elif len(p) == 6 and p[1] == 'PACKAGE':
        p[0] = ('package', p[2], p[4])
    elif len(p) == 5:
        p[0] = ('link', p[1], p[2], p[3], p[4])
    elif len(p) == 4:
        p[0] = ('inherit', p[1], p[2], p[3])


def p_stereo(p):
    """stereo : STEREO
               | empty"""
    p[0] = ('stereo =>', p[1])


def p_def_act(p):
    """def_act : ID
                | ACTOR_TXT
                | STRING"""
    p[0] = ('def_act =>', p[1])


def p_def_uc(p):
    """def_uc : ID
               | USE_CASE_TXT
               | STRING"""
    p[0] = ('def_uc =>', p[1])


def p_ucl_link(p):
    """ucl_link : COLON EXTENDS
                 | COLON INCLUDES
                 | COLON ID
                 | empty"""
    if len(p) == 3:
        p[0] = ('ucl_link =>', p[1], p[2])
    else:
        p[0] = 'epsilon'


def p_arrow(p):
    """arrow : RIGHT_ARROW_1
              | RIGHT_ARROW_2"""
    p[0] = ('arrow =>', p[1])


def p_var(p):
    """var : ID
            | USE_CASE_TXT
            | ACTOR_TXT"""
    p[0] = ('var =>', p[1])


def p_alias(p):
    """alias : AS ID
              | empty"""
    if len(p) == 3:
        p[0] = ('alias =>', p[2])
    else:
        p[0] = 'epsilon'


def p_empty(p):
    """empty :"""
    pass


def p_error(p):
    raise SyntaxError(
        f"Syntax error at '{p.value}', line {
            p.lexer.lineno}, position {p.lexpos}"
    )


parser = yacc.yacc()

lexer.input("""@startuml System
actor :User:
usecase (Define travel)
usecase (Set VIP options)
usecase (Authentication)
:User: --> (Define travel)
:User: --> (Set VIP options)
(Define travel) --> (Authentication) : includes
(Set VIP options) --> (Define travel) : extends
package Administration {
actor :Admin:
:User: <|-- :Admin:
usecase (Remove travel)
:Admin: --> (Remove travel)
(Remove travel) --> (Authentication) : includes
}
@enduml""")

print(f' arbre concret: \n {parser.parse()}')
