import ply.lex as lex

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
    'ACTOR_TEXT',
    'USE_CASE_TEXT',
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
def t_ACTOR_TEXT(t):
    r':[a-zA-Z][a-zA-Z0-9 ]+:'
    # reduce space number :main      actor: = :main actor:
    t.value = ' '.join(t.value[1:-1].split())
    return t


# We define COLON as a separate function before the ACTOR_TEXT function to eliminate conflicts.
#  This ensures that standalone colons (:) are correctly identified as a COLON token,
#  while ACTOR_TEXT handles actor names enclosed in colons (e.g., :Main Admin:)
def t_COLON(t):
    r':'
    return t


def t_USE_CASE_TEXT(t):
    r'\([^\(\)]+\)'
    t.value = t.value[1:-1]
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

lexer.input(""" @startuml System
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
@enduml """)


while True:
    tok = lexer.token()
    if not tok:
        break
    print(tok)
