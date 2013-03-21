module Token(Token(..),display) where

import StringTab

data Token = 
        MULT                  |
        INHERITS              |
        POOL                  |
        CASE                  |
        LPAREN                |
        SEMI                  |
        MINUS                 |
        STR_CONST String      |
        RPAREN                |
        NOT                   |
        TYPEID TypeId         |
        LT                    |
        IN                    |
        COMMA                 |
        CLASS                 |
        FI                    |
        DIV                   |
        LOOP                  |
        PLUS                  |
        ASSIGN                |
        IF                    |
        DOT                   |
        LE                    |
        OF                    |
        EOF                   |
        INT_CONST IntId       |
        NEW                   |
        Error                 |
        ISVOID                |
        EQ                    |
        ERROR String          |
        COLON                 |
        NEG                   |
        LBRACE                |
        ELSE                  |
        DARROW                |
        WHILE                 |
        ESAC                  |
        LET                   |
        RBRACE                |
        LET_STMT              |
        THEN                  |
        BOOL_CONST Bool       |
        OBJECTID ObjectId     |
        AT
              deriving (Eq,Show)

display :: Tables -> Token -> String
display _ MULT = "'*'"
display _ LPAREN = "'('"
display _ SEMI = "';'"
display _ MINUS = "'-'"
display _ RPAREN = "')'"
display t (TYPEID tid) =  "TYPEID " ++ (getType t tid)
display _ Token.LT = "'<'"
display _ COMMA = "','"
display _ DIV = "'/'"
display _ PLUS = "'+'"
display _ DOT = "'.'"
display t (INT_CONST iid) = "INT_CONST " ++ (getInt t iid)
display _ Token.EQ = "'='"
display _ COLON = "':'"
display _ NEG = "'~'"
display _ LBRACE = "'{'"
display _ RBRACE = "'}'"
display _ (BOOL_CONST True) = "BOOL_CONST true"
display _ (BOOL_CONST False) = "BOOL_CONST false"
display t (OBJECTID oid) = "OBJECTID " ++ (getObject t oid)
display _ AT = "'@'"
display _ (STR_CONST s) = "STR_CONST \"" ++  (reescape s) ++ "\""
display _ tok = show tok

--Fix escaping to match reflexer on some escapings
reescape :: String -> String
reescape [] = []
reescape ('\\':rest) = '\\':'\\':reescape rest
reescape ('\t':rest) = '\\':'t':reescape rest
reescape ('\r':rest) = '\\':'0':'1':'5':reescape rest
reescape ('\ESC':rest) = '\\':'0':'3':'3':reescape rest
reescape ('\f':rest) = '\\':'f':reescape rest
reescape ('\b':rest) = '\\':'b':reescape rest
reescape ('\n':rest) = '\\':'n':reescape rest
reescape ('\"':rest) = '\\':'\"':reescape rest
reescape ('\v':rest) = '\\':'0':'1':'3':reescape rest
reescape ('\DC2':rest) = '\\':'0':'2':'2':reescape rest
reescape (x:rest) = x:reescape rest