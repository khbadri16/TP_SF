import ply.lex as lex
import ply.yacc as yacc


reserved = {
    'actor': 'ACTOR',
    'as': 'AS',
    'usecase': 'USECASE',
    'package': 'PACKAGE',
    'includes': 'INCLUDES',
    'extends': 'EXTENDS',
    # '@startuml': 'STARTUML',
    # '@enduml': 'ENDUML',
    # We can't define @startuml and @enduml in the reserved dictionary and override their type with STARTUML and ENDUML in the ID function.
    # This is because the ID regular expression needs to match @startuml and @enduml,
    #  which isn't happening due to the @ character. While other solutions are possible,
    #  we chose to define them directly as separate tokens below.
}

tokens = (
    'STARTUML',
    'ENDUML',
    'COLON',
    'RIGHT_ARROW_1',
    'RIGHT_ARROW_2',
    'LBRACE',
    'RBRACE',
    'INHERIT',
    'EOL',
    'STRING',
    'STEREO',
    'ACTOR_TXT',
    'USE_CASE_TXT',
    'ID',
) + tuple(reserved.values())


t_STARTUML = r'@startuml'
t_ENDUML = r'@enduml'
t_RIGHT_ARROW_1 = r'\-{1,2}\>'
t_RIGHT_ARROW_2 = r'\.{1,2}\>'
t_LBRACE = r'\{'
t_RBRACE = r'\}'
t_INHERIT = r'<\|--'
t_EOL = r'\n'


def t_STRING(t):
    r'"[^"]*"'
    t.value = t.value[1:-1]  # To return the string value without the quotes
    return t


def t_STEREO(t):
    # Since the use of stereo is for defining an actor, we did not allow << >>
    r'<<[^<>]+>>'
    t.value = t.value[2:-2]
    return t


# In the case of ACTOR_TEXT, it is intended to represent text describing an actor.
#  While our current implementation allows only alphabetic characters, numbers, and spaces,
#  it’s possible to include additional characters in the regular expression. However,
#  the choice of allowed characters ultimately depends on the language designer's specifications.
# For now, we’ll proceed with the current lexical analyzer solution, as it meets the requirements you've outlined.
def t_ACTOR_TXT(t):
    r':[a-zA-Z ][a-zA-Z0-9 ]+:'
    # reduce space number :main      actor: = :main actor:
    t.value = ' '.join(t.value[1:-1].split())
    return t


# We define COLON as a separate function before the ACTOR_TEXT function to eliminate conflicts.
#  This ensures that standalone colons (:) are correctly identified as a COLON token,
#  while ACTOR_TEXT handles actor names enclosed in colons (e.g., :Main Admin:)
def t_COLON(t):
    r':'
    return t


def t_USE_CASE_TXT(t):
    # Allows letters, numbers, and spaces, but doesn't start with a number
    r'\([a-zA-Z ][a-zA-Z0-9 ]*\)'
    t.value = ' '.join(t.value[1:-1].split())
    return t


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z0-9_]*'
    t.type = reserved.get(t.value, 'ID')
    return t


t_ignore = ' \t'


def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)


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
    if p:
        raise SyntaxError(f"Syntax error at '{p.value}'")
    else:
        raise SyntaxError("Syntax error at EOF")


parser = yacc.yacc()

lexer.input("""@startuml System
actor :User: as act
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
