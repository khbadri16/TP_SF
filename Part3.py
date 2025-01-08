from __future__ import annotations
import ply.lex as lex
import ply.yacc as yacc


class SemanticError(Exception):
    """
    Custom exception for semantic errors.
    """

    def __init__(self, message="A semantic error occurred."):
        super().__init__(message)


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
    # Do you mean [2:-2] in the solution ?
    # Making it [3:-3] will cause a loss in information: a letter at the beginning and a letter at the end. 'Not-a-machine' will become 'ot-a-machin'
    t.value = t.value[2:-2]
    return t


def t_ID(t):
    r"[a-zA-Z_][a-zA-Z_0-9]*"
    if t.value in reserved.keys():
        t.type = reserved[t.value]
    return t


def t_ACTOR_TXT(t):
    ":[^ :\n][^\n:]*:"
    t.value = ("ACTOR_TXT", t.value[1:-1])
    return t


def t_USE_CASE_TXT(t):
    r"\([^ \(\n][^)\n:]*\)"
    t.value = ("USE_CASE_TXT", t.value[1:-1])
    return t


t_ignore = " \t"


def t_error(t):
    raise ValueError(f"Unexpected symbol {t}")


table_of_ID = dict()
table_of_declaration_actor = dict()
table_of_declaration_usecase = dict()


class Start:
    def __init__(self, startuml: str, name: str, one_defs: list, enduml: str):
        self.start = startuml
        self.name = name
        self.one_defs = one_defs
        self.end = enduml

        if name in table_of_ID:
            raise SemanticError(f"Name conflict: ID '{name}'.")

        table_of_ID[name] = None

    def __repr__(self):

        return f'{self.start} {self.name} \n' + '\n'.join(map(str, self.one_defs)) + f' {self.end}\n'


class One_def:
    def __init__(self, entity: 'Entity' = None, relations: 'Relations' = None, package: 'Package' = None):
        self.entity = entity
        self.relations = relations
        self.package = package

    def __repr__(self):
        return f'{self.entity or ""}{self.relations or ""}{self.package or ""}'


class Entity:
    def __init__(self, txt: str, alias, stereo, keyword: str = None, use_case=None):
        self.txt = txt
        self.alias = alias
        self.stereo = stereo
        self.keyword = keyword

        if alias and alias in table_of_ID:
            raise SemanticError(f"Name conflict: Alias '{alias}'.")

        if alias:
            table_of_ID[alias] = None

        if use_case:
            if txt in table_of_declaration_usecase:
                raise SemanticError(f"Use case '{txt}' is already declared.")
            else:
                table_of_declaration_usecase[txt] = None
        else:
            if txt in table_of_declaration_actor:
                raise SemanticError(f"Actor '{txt}' is already declared.")
            else:
                table_of_declaration_actor[txt] = None

    def __repr__(self):
        if self.keyword:
            return f'{self.keyword} {self.txt} {self.alias or ""} {self.stereo or ""}'
        else:
            return f'{self.txt} {self.alias or ""} {self.stereo or ""}'


class Relations:
    def __init__(self, INHERIT=None, var1: str = None, arrow=None, var2: str = None, ucl_link=None):
        self.INHERIT = INHERIT
        self.var1 = var1
        self.var2 = var2
        self.arrow = arrow
        self.ucl_link = ucl_link

        if var1 not in table_of_ID and var1 not in table_of_declaration_actor and var1 not in table_of_declaration_usecase:
            raise SemanticError(f"Variable '{var1}' is used but not declared.")
        if var2 not in table_of_ID and var2 not in table_of_declaration_actor and var2 not in table_of_declaration_usecase:
            raise SemanticError(f"Variable '{var2}' is used but not declared.")

    def __repr__(self):
        if self.INHERIT:
            return f'{self.var1} {self.INHERIT} {self.var2} '
        elif self.arrow:
            return f'{self.var1} {self.arrow} {self.var2} {self.ucl_link or ""} '
        return ''


class Package:
    def __init__(self, keyword_package: str, ID: str, one_defs: list):
        self.keyword_package = keyword_package
        self.ID = ID
        self.one_defs = one_defs

        if ID in table_of_ID:
            raise SemanticError(f"Name conflict: ID : '{ID}'.")
        table_of_ID[ID] = None

    def __repr__(self):

        return f'{self.keyword_package} {self.ID} \n {{ ' + '\n'.join(map(str, self.one_defs)) + ' }} \n'


def p_start(p):
    """start : eols STARTUML name EOL defs ENDUML eols"""
    p[0] = Start(p[2], p[3], p[5], p[6])


def p_eols(p):
    """eols : EOL eols
            | empty"""
    if len(p) == 3:
        p[0] = p[1]
    else:
        p[0] = p[1]


def p_name(p):
    """name : ID
             | empty"""
    p[0] = p[1]


def p_defs(p):
    """defs : one_def EOL
             | defs one_def EOL"""
    if len(p) == 3:
        p[0] = [p[1]]
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
        if p[1] == "actor":
            p[0] = One_def(entity=Entity(
                txt=p[2], alias=p[3], stereo=p[4], keyword=p[1]))
        elif p[1] == "usecase":
            p[0] = One_def(entity=Entity(
                txt=p[2], alias=p[3], stereo=p[4], keyword=p[1], use_case=True))
        else:
            p[0] = One_def(relations=Relations(
                var1=p[1], arrow=p[2], var2=p[3], ucl_link=p[4]))
    if len(p) == 6:
        p[0] = One_def(package=Package(
            keyword_package=p[1], ID=p[2], one_defs=p[4]))
    if len(p) == 4:
        if p[2] == "<|--":
            p[0] = One_def(relations=Relations(
                var1=p[1], INHERIT=p[2], var2=p[3]))
        elif isinstance(p[1], tuple):
            token_type, token_value = p[1]
            if token_type == "USE_CASE_TXT":
                p[0] = One_def(entity=Entity(
                    txt=token_value, alias=p[2], stereo=p[3], use_case=True))
            elif token_type == "ACTOR_TXT":
                p[0] = One_def(entity=Entity(txt=token_value,
                                             alias=p[2], stereo=p[3]))


def p_stereo(p):
    """stereo : STEREO
               | empty"""
    p[0] = p[1]


def p_def_act(p):
    """def_act : ID
                | ACTOR_TXT
                | STRING"""
    if isinstance(p[1], tuple):
        _, token_value = p[1]
        p[0] = token_value
    else:
        p[0] = p[1]


def p_def_uc(p):
    """def_uc : ID
               | USE_CASE_TXT
               | STRING"""
    if isinstance(p[1], tuple):
        _, token_value = p[1]
        p[0] = token_value
    else:
        p[0] = p[1]


def p_ucl_link(p):
    """ucl_link : COLON EXTENDS
                 | COLON INCLUDES
                 | COLON ID
                 | empty"""
    if len(p) == 3:
        p[0] = p[1], p[2]
    else:
        p[0] = p[1]


def p_arrow(p):
    """arrow : RIGHT_ARROW_1
              | RIGHT_ARROW_2"""
    p[0] = p[1]


def p_var(p):
    """var : ID
            | USE_CASE_TXT
            | ACTOR_TXT"""
    if isinstance(p[1], tuple):
        _, token_value = p[1]
        p[0] = token_value
    else:
        p[0] = p[1]


def p_alias(p):
    """alias : AS ID
              | empty"""
    if len(p) == 3:
        p[0] = p[2]
    else:
        p[0] = p[1]


def p_empty(p):
    """empty :"""
    pass


def p_error(p):
    raise SyntaxError(
        f"Syntax error at '{p.value}', line {
            p.lexer.lineno}, position {p.lexpos}"
    )


lexer = lex.lex()
parser = yacc.yacc()

lexer.input("""@startuml Administration
actor :System:
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

print(parser.parse())
