{
module Language.Java.Parser (
    compilationUnit, packageDecl, importDecl, typeDecl,

    classDecl, interfaceDecl,

    memberDecl, fieldDecl, methodDecl, constrDecl,
    interfaceMemberDecl, absMethodDecl,

    formalParams, formalParam,

    modifier,

    varDecls, varDecl,

    block, blockStmt, stmt,

    stmtExp, exp, primary, literal,

    ttype, primType, refType, classType, resultType,

    lambdaExp, methodRef,

    typeParams, typeParam,

    name, ident

    ) where

import Language.Java.Lexer ( L(..), Token(..), lexer)
import Language.Java.Syntax
import Language.Java.Pretty (pretty)

}

%name compilationUnit CompilationUnit
%tokentype Token
%error { parseError }
%lexer { lexer } { EOF }
%monad { PM }

%token

    abstract          { KW_Abstract }
    '@interface'      { KW_AnnInterface }
    assert            { KW_Assert }
    boolean           { KW_Boolean }
    break             { KW_Break }
    byte              { KW_Byte }
    case              { KW_Case }
    catch             { KW_Catch }
    default           { KW_Char }
    class             { KW_Class }
    const             { KW_Const }
    continue          { KW_Continue }
    defualt           { KW_Default }
    do                { KW_Do }
    double            { KW_Double }
    else              { KW_Else }
    enum              { KW_Enum }
    extends           { KW_Extends }
    final             { KW_Final }
    finally           { KW_Finally }
    float             { KW_Float }
    for               { KW_For }
    goto              { KW_Goto }
    if                { KW_If }
    implements        { KW_Implements }
    import            { KW_Import }
    instanceof        { KW_Instanceof }
    int               { KW_Int }
    interface         { KW_Interface }
    long              { KW_Long }
    native            { KW_Native }
    new               { KW_New }
    package           { KW_Package }
    private           { KW_Private }
    protected         { KW_Protected }
    public            { KW_Public }
    return            { KW_Return }
    short             { KW_Short }
    static            { KW_Static }
    strictpf          { KW_Strictfp }
    super             { KW_Super }
    switch            { KW_Switch }
    synchronized      { KW_Synchronized }
    this              { KW_This }
    throw             { KW_Throw }
    throws            { KW_Throws }
    transient         { KW_Transient }
    try               { KW_Try }
    void              { KW_Void }
    volatile          { KW_Volatile }
    while             { KW_While }

    '('               { OpenParen }
    ')'               { CloseParen }
    '['               { OpenSquare }
    ']'               { CloseSquare }
    '{'               { OpenCurly }
    '}'               { CloseCurly }
    ';'               { SemiColon }
    ','               { Comma }
    '.'               { Period }
    '->'              { LambdaArrow }
    '::'              { MethodRefSep }

    -- Literals
    IntVal            { IntTok $$ }
    LongVal           { LongTok $$ }
    DoubleVal         { DoubleTok $$ }
    FloatVal          { FloatTok $$ }
    CharVal           { CharTok $$ }
    StringVal         { StringTok $$ }
    BoolVal           { BoolTok $$ }
    null              { NullTok }

    -- Identifiers
    ident             { IdentTok $$ }

    -- Operators
    '='               { Op_Equal }
    '>'               { Op_GThan }
    '<'               { Op_LThan }
    '!'               { Op_Bang }
    '~'               { Op_Tilde }
    '?'               { Op_Query }
    ':'               { Op_Colon }
    '=='              { Op_Equals }
    '<='              { Op_LThanE }
    '>='              { Op_GThanE }
    '!='              { Op_BangE }
    '&&'              { Op_AAnd }
    '||'              { Op_OOr }
    '++'              { Op_PPlus }
    '--'              { Op_MMinus }
    '+'               { Op_Plus }
    '-'               { Op_Minus }
    '*'               { Op_Star }
    '/'               { Op_Slash }
    '&'               { Op_And }
    '|'               { Op_Or }
    '^'               { Op_Caret }
    '%'               { Op_Percent }
    '<<'              { Op_LShift }
    '+='              { Op_PlusE }
    '-='              { Op_MinusE }
    '*='              { Op_StarE }
    '/='              { Op_SlashE }
    '&='              { Op_AndE }
    '|='              { Op_OrE }
    '^='              { Op_CaretE }
    '%='              { Op_PercentE }
    '<<='             { Op_LShiftE }
    '>>='             { Op_RShiftE }
    '>>>='            { Op_RRShiftE }
    '@'               { Op_AtSign }

opt(p) : p { Just $1 }
       |   { Nothing }

braced(p) : '{' p '}' { $2 }
parens(p) : '(' p ')' { $2 }
bracketed(p) : '[' p ']' { $2 }

many(p) : p many(p) { $1 : $2 }
        |           { [] }

many_sep1(p, sep)
    : p sep many_sep(p, sep) { $1 : $3 }
    | p                      { [$1] }

many_sep(p, sep)
    : many_sep1(p, sep) { $1 }
    |                   { [] }

CompilationUnit
    : opt(PackageDecl) many(ImportDecl) many_sep(TypeDecl, ';')
        { CompilationUnit $1 $2 (catMaybes $3) }

PackageDecl
    : package ident ';' { PackageDecl $2 }

ImportDecl
    : import opt(static) QualIdent opt('.' '*') ';'
        { ImportDecl (isJust $2) $3 (isJust $4) }

QualIdent
    : many_sep1(ident, '.') { Name $1 }

-- Sort this out later, these have set ordering and not all are allowed everywhere
Modifiers : many(Modifier)

Modifier
    : abstract        { Public }
    | static          { Protected }
    | final           { Final }
    | transient       { Transient }
    | synchronized    { Synchronized_ }
    | public          { Public }
    | private         { Private }
    | protected       { Protected }
    | native          { Native }
    | strictfp        { StrictFP }
    | volatile        { Volatile }
    | Anntoation      { Annotation $$ }

TypeDecl
    : Modifiers TypeDecl { $2 $1 }

TypeDecl'
    : NormalClassDecl { $1 }
    | EnumClassDecl   { $1 }
    | InterfaceDecl   { $1 }

NormalClassDecl
    : class ident TypeParams opt(Extends) opt(Implements) braced(many(ClassBodyDecl))
         { \ms -> ClassDecl ms $2 $3 $4 $5 $6 }

Extends
    : extends ClassType { $2 }

Implements
    : implements many_sep1(ClassType, ',') { $2 }

EnumClassDecl
    : enum ident opt(Implements) braced(EnumBody)
        { \ms -> EnumDecl ms $2 $3 $4 }


EnumBody
    : many_sep1(EnumConstant, ',') opt(',') many(ClassBodyDecl)
        { EnumBody $1 $3 }

EnumConstant
    : ident opt(Args) opt(braced(ClassBody)) { EnumConstant $1 $2 $3 }

InterfaceDecl
    : Interface ident TypeParams opt(Extends) many(InterfaceBodyDecl)
        { \ms -> InterfaceDecl $0 ms $2 $3 $4 (InterfaceBody $5) }

Interface
    : '@interface' { InterfaceAnnotation }
    | interface    { InterfaceNormal }

ClassBodyDecl
    : Modifiers ClassMemberDecl { MemberDecl ($2 $1) }
    | opt(static) Block         { InitDecl (isJust $1) $2 }
    | ConstructorDeclaration    { $1 }

ClassMemberDeclaration
    : FieldDecl        { $1 }
    | MethodDeclaraion { $1 }
    | ClassDecl        { \ms -> MemberClassDecl ($1 ms) }
    | InterfaceDecl    { \ms -> MemberInterfaceDecl ($1 ms) }

FieldDecl
    : Type VarDecls ';' { \ms -> FieldDecl ms $1 $2 }

TypeParams
    : '<' many_sep(TypeParam, ',') '>' { $2 }

TypeParam
    : ident opt(Bounds) { $2 }

Bounds
    : extends many_sep1(RefType, '&') { $2 }

MethodDecl
    : opt(TypeParams) ResultType ident FormalParams opt(ThrowDecl) MethodBody
        { \ms -> MethodDecl ms $1 $2 $3 $4 $5 Nothing $6 }

MethodBody
    : ';' { Nothing }
    | Block { $1 }

ConstrDecl
    : opt(TypeParams) ident FormalParams opt(ThrowDecl) ConstrBody
        { \ms -> ConstructorDecl ms $1 $2 $3 $4 $5 }

ConstrBody
    : opt(ExplConstrInv) braced(many(BlockStmt))
        { ConstructorBody $1 $2 }

ExplConstrInv
    : Primary '.' opt(RefTypeArgs) super Args
        { PrimarySuperInvoke $1 $3 $5 }
    | RefTypeArgs ConstrInvTarget Args
        { $2 $1 $3 }

ConstrInvTarget
    : super { SuperInvoke }
    | this  { ThisInvoke }

InterfaceBodyDecl
    : Modifers InterfaceMemberDecl { $2 $1 }

InterfaceMemberDecl
    : ClassDecl     { \ms -> MemberClassDecl ($1 ms) }
    | InterfaceDecl { \ms -> MemberInterfaceDecl ($1 ms) }
    | FieldDecl     { $1 }
    | AbsMethodDec  { $1 }

AbsMethodDecl
    : opt(TypeParams) ResultType ident FormalParams opt(ThrowDecl) opt(DefaultValue) ';'
        { \ms -> MethodDecl ms $1 $2 3 $4 $5 $6 }

DefaultValue
    : default Exp { $2 }

ThrowDecl
    : throws many_sep(RefTypeList, ',') { $2 }

FormalParams
    : parens(many_sep(FormalParam, ',')) { validateFPs $1 }

FormalaParam
    : Modifiers Type opt(Ellipsis) VarDeclId
        { FormalParam $1 $2 (isJust $3) $4 }

Ellipsis
    : '.' '.' '.' { () }

Annotation
    : '@' Name opt(parens(AnnotationArgs))
        { case $2 of
              Nothing -> MarkerAnnotation
              Just (Left args) -> NormalAnnotation args
              Just (Right val) -> SingleElementAnnotation val
        }

AnnotationArgs
    : many_sep(AnnotationArg, ',') { Left $1 }
    | ElementValue                 { Right $1 }

AnnotationArg
    : ident '=' ElementVal { ($1, $2) }

ElementVal
    : ArrayInit  { EVVal (InitArray $1) }
    | InitExp    { EVVal (InitExp $1) }
    | Annotation { EVAnn $1 }


varDecls :: P [VarDecl]
varDecls = seplist1 varDecl comma

varDecl :: P VarDecl
varDecl = do
    vid <- varDeclId
    mvi <- opt $ tok Op_Equal >> varInit
    return $ VarDecl vid mvi

varDeclId :: P VarDeclId
varDeclId = do
    id  <- ident
    abs <- list arrBrackets
    return $ foldl (\f _ -> VarDeclArray . f) VarId abs id

arrBrackets :: P ()
arrBrackets = brackets $ return ()

localVarDecl :: P ([Modifier], Type, [VarDecl])
localVarDecl = do
    ms  <- list modifier
    typ <- ttype
    vds <- varDecls
    return (ms, typ, vds)

varInit :: P VarInit
varInit =
    InitArray <$> arrayInit <|>
    InitExp   <$> exp

arrayInit :: P ArrayInit
arrayInit = braces $ do
    vis <- seplist varInit comma
    opt comma
    return $ ArrayInit vis

----------------------------------------------------------------------------
-- Statements

block :: P Block
block = braces $ Block <$> list blockStmt

blockStmt :: P BlockStmt
blockStmt =
    (try $ do
        ms  <- list modifier
        cd  <- classDecl
        return $ LocalClass (cd ms)) <|>
    (try $ do
        (m,t,vds) <- endSemi $ localVarDecl
        return $ LocalVars m t vds) <|>
    BlockStmt <$> stmt

stmt :: P Stmt
stmt = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e   <- parens exp
        (try $
            do th <- stmtNSI
               tok KW_Else
               el <- stmt
               return $ IfThenElse e th el) <|>
           (do th <- stmt
               return $ IfThen e th)
    whileStmt = do
        tok KW_While
        e   <- parens exp
        s   <- stmt
        return $ While e s
    forStmt = do
        tok KW_For
        f <- parens $
            (try $ do
                fi <- opt forInit
                semiColon
                e  <- opt exp
                semiColon
                fu <- opt forUp
                return $ BasicFor fi e fu) <|>
            (do ms <- list modifier
                t  <- ttype
                i  <- ident
                colon
                e  <- exp
                return $ EnhancedFor ms t i e)
        s <- stmt
        return $ f s
    labeledStmt = try $ do
        lbl <- ident
        colon
        s   <- stmt
        return $ Labeled lbl s

stmtNSI :: P Stmt
stmtNSI = ifStmt <|> whileStmt <|> forStmt <|> labeledStmt <|> stmtNoTrail
  where
    ifStmt = do
        tok KW_If
        e  <- parens exp
        th <- stmtNSI
        tok KW_Else
        el <- stmtNSI
        return $ IfThenElse e th el
    whileStmt = do
        tok KW_While
        e <- parens exp
        s <- stmtNSI
        return $ While e s
    forStmt = do
        tok KW_For
        f <- parens $ (try $ do
            fi <- opt forInit
            semiColon
            e  <- opt exp
            semiColon
            fu <- opt forUp
            return $ BasicFor fi e fu)
            <|> (do
            ms <- list modifier
            t  <- ttype
            i  <- ident
            colon
            e  <- exp
            return $ EnhancedFor ms t i e)
        s <- stmtNSI
        return $ f s
    labeledStmt = try $ do
        i <- ident
        colon
        s <- stmtNSI
        return $ Labeled i s

stmtNoTrail :: P Stmt
stmtNoTrail =
    -- empty statement
    const Empty <$> semiColon <|>
    -- inner block
    StmtBlock <$> block <|>
    -- assertions
    (endSemi $ do
        tok KW_Assert
        e   <- exp
        me2 <- opt $ colon >> exp
        return $ Assert e me2) <|>
    -- switch stmts
    (do tok KW_Switch
        e  <- parens exp
        sb <- switchBlock
        return $ Switch e sb) <|>
    -- do-while loops
    (endSemi $ do
        tok KW_Do
        s <- stmt
        tok KW_While
        e <- parens exp
        return $ Do s e) <|>
    -- break
    (endSemi $ do
        tok KW_Break
        mi <- opt ident
        return $ Break mi) <|>
    -- continue
    (endSemi $ do
        tok KW_Continue
        mi <- opt ident
        return $ Continue mi) <|>
    -- return
    (endSemi $ do
        tok KW_Return
        me <- opt exp
        return $ Return me) <|>
    -- synchronized
    (do tok KW_Synchronized
        e <- parens exp
        b <- block
        return $ Synchronized e b) <|>
    -- throw
    (endSemi $ do
        tok KW_Throw
        e <- exp
        return $ Throw e) <|>
    -- try-catch, both with and without a finally clause
    (do tok KW_Try
        b <- block
        c <- list catch
        mf <- opt $ tok KW_Finally >> block
        -- TODO: here we should check that there exists at
        -- least one catch or finally clause
        return $ Try b c mf) <|>
    -- expressions as stmts
    ExpStmt <$> endSemi stmtExp

-- For loops

forInit :: P ForInit
forInit = (do
    try (do (m,t,vds) <- localVarDecl
            return $ ForLocalVars m t vds)) <|>
    (seplist1 stmtExp comma >>= return . ForInitExps)

forUp :: P [Exp]
forUp = seplist1 stmtExp comma

-- Switches

switchBlock :: P [SwitchBlock]
switchBlock = braces $ list switchStmt

switchStmt :: P SwitchBlock
switchStmt = do
    lbl <- switchLabel
    bss <- list blockStmt
    return $ SwitchBlock lbl bss

switchLabel :: P SwitchLabel
switchLabel = (tok KW_Default >> colon >> return Default) <|>
    (do tok KW_Case
        e <- exp
        colon
        return $ SwitchCase e)

-- Try-catch clauses

catch :: P Catch
catch = do
    tok KW_Catch
    fp <- parens formalParam
    b  <- block
    return $ Catch fp b

----------------------------------------------------------------------------
-- Expressions

stmtExp :: P Exp
stmtExp = try preIncDec
    <|> try postIncDec
    <|> try assignment
    -- There are sharing gains to be made by unifying these two
    <|> try methodInvocationExp
    <|> try lambdaExp
    <|> try methodRef
    <|> instanceCreation

preIncDec :: P Exp
preIncDec = do
    op <- preIncDecOp
    e <- unaryExp
    return $ op e

postIncDec :: P Exp
postIncDec = do
    e <- postfixExpNES
    ops <- list1 postfixOp
    return $ foldl (\a s -> s a) e ops

assignment :: P Exp
assignment = do
    lh <- lhs
    op <- assignOp
    e  <- assignExp
    return $ Assign lh op e

lhs :: P Lhs
lhs = try (FieldLhs <$> fieldAccess)
    <|> try (ArrayLhs <$> arrayAccess)
    <|> NameLhs <$> name



exp :: P Exp
exp = assignExp

assignExp :: P Exp
assignExp = try methodRef <|> try lambdaExp <|> try assignment <|> condExp

condExp :: P Exp
condExp = do
    ie <- infixExp
    ces <- list condExpSuffix
    return $ foldl (\a s -> s a) ie ces

condExpSuffix :: P (Exp -> Exp)
condExpSuffix = do
    tok Op_Query
    th <- exp
    colon
    el <- condExp
    return $ \ce -> Cond ce th el

infixExp :: P Exp
infixExp = do
    ue <- unaryExp
    ies <- list infixExpSuffix
    return $ foldl (\a s -> s a) ue ies

infixExpSuffix :: P (Exp -> Exp)
infixExpSuffix =
    (do
      op <- infixCombineOp
      ie2 <- infixExp
      return $ \ie1 -> BinOp ie1 op ie2) <|>
    (do op <- infixOp
        e2 <- unaryExp
        return $ \e1 -> BinOp e1 op e2) <|>
    (do tok KW_Instanceof
        t  <- refType
        return $ \e1 -> InstanceOf e1 t)

unaryExp :: P Exp
unaryExp = try preIncDec <|>
    try (do
        op <- prefixOp
        ue <- unaryExp
        return $ op ue) <|>
    try (do
        t <- parens ttype
        e <- unaryExp
        return $ Cast t e) <|>
    postfixExp

postfixExpNES :: P Exp
postfixExpNES = -- try postIncDec <|>
    try primary <|>
    ExpName <$> name

postfixExp :: P Exp
postfixExp = do
    pe <- postfixExpNES
    ops <- list postfixOp
    return $ foldl (\a s -> s a) pe ops


primary :: P Exp
primary = primaryNPS |>> primarySuffix

primaryNPS :: P Exp
primaryNPS = try arrayCreation <|> primaryNoNewArrayNPS

primaryNoNewArray = startSuff primaryNoNewArrayNPS primarySuffix

primaryNoNewArrayNPS :: P Exp
primaryNoNewArrayNPS =
    Lit <$> literal <|>
    const This <$> tok KW_This <|>
    parens exp <|>
    -- TODO: These two following should probably be merged more
    (try $ do
        rt <- resultType
        period >> tok KW_Class
        return $ ClassLit rt) <|>
    (try $ do
        n <- name
        period >> tok KW_This
        return $ ThisClass n) <|>
    try instanceCreationNPS <|>
    try (MethodInv <$> methodInvocationNPS) <|>
    try (FieldAccess <$> fieldAccessNPS) <|>
    ArrayAccess <$> arrayAccessNPS

primarySuffix :: P (Exp -> Exp)
primarySuffix = try instanceCreationSuffix <|>
    try ((ArrayAccess .) <$> arrayAccessSuffix) <|>
    try ((MethodInv .) <$> methodInvocationSuffix) <|>
    (FieldAccess .) <$> fieldAccessSuffix


instanceCreationNPS :: P Exp
instanceCreationNPS =
    do tok KW_New
       tas <- lopt typeArgs
       tds <- typeDeclSpecifier
       as  <- args
       mcb <- opt classBody
       return $ InstanceCreation tas tds as mcb

typeDeclSpecifier :: P TypeDeclSpecifier
typeDeclSpecifier =
    (try $ do ct <- classType
              period
              i <- ident
              tok Op_LThan
              tok Op_GThan
              return $ TypeDeclSpecifierWithDiamond ct i Diamond
    ) <|>
    (try $ do i <- ident
              tok Op_LThan
              tok Op_GThan
              return $ TypeDeclSpecifierUnqualifiedWithDiamond i Diamond
    ) <|>
    (do ct <- classType
        return $ TypeDeclSpecifier ct
    )

instanceCreationSuffix :: P (Exp -> Exp)
instanceCreationSuffix =
     do period >> tok KW_New
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
        return $ \p -> QualInstanceCreation p tas i as mcb

instanceCreation :: P Exp
instanceCreation = try instanceCreationNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let icp = foldl (\a s -> s a) p ss
    case icp of
     QualInstanceCreation {} -> return icp
     _ -> fail ""


lambdaParams :: P LambdaParams
lambdaParams = try (LambdaSingleParam <$> ident)
               <|> try (parens $ LambdaFormalParams <$> (seplist formalParam comma))
               <|> (parens $ LambdaInferredParams <$> (seplist ident comma))

lambdaExp :: P Exp
lambdaExp = Lambda
            <$> (lambdaParams <* (tok LambdaArrow))
            <*> ((LambdaBlock <$> (try block))
                 <|> (LambdaExpression <$> exp))

methodRef :: P Exp
methodRef = MethodRef
            <$> (name <*  (tok MethodRefSep))
            <*> ident

{-
instanceCreation =
    (do tok KW_New
        tas <- lopt typeArgs
        ct  <- classType
        as  <- args
        mcb <- opt classBody
        return $ InstanceCreation tas ct as mcb) <|>
    (do p   <- primary
        period >> tok KW_New
        tas <- lopt typeArgs
        i   <- ident
        as  <- args
        mcb <- opt classBody
        return $ QualInstanceCreation p tas i as mcb)
-}

fieldAccessNPS :: P FieldAccess
fieldAccessNPS =
    (do tok KW_Super >> period
        i <- ident
        return $ SuperFieldAccess i) <|>
    (do n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccess n i)

fieldAccessSuffix :: P (Exp -> FieldAccess)
fieldAccessSuffix = do
    period
    i <- ident
    return $ \p -> PrimaryFieldAccess p i

fieldAccess :: P FieldAccess
fieldAccess = try fieldAccessNPS <|> do
    p <- primaryNPS
    ss <- list primarySuffix
    let fap = foldl (\a s -> s a) p ss
    case fap of
     FieldAccess fa -> return fa
     _ -> fail ""

{-
fieldAccess :: P FieldAccess
fieldAccess = try fieldAccessNPS <|> do
    p <- primary
    fs <- fieldAccessSuffix
    return (fs p)
-}

{-
fieldAccess :: P FieldAccess
fieldAccess =
    (do tok KW_Super >> period
        i <- ident
        return $ SuperFieldAccess i) <|>
    (try $ do
        n <- name
        period >> tok KW_Super >> period
        i <- ident
        return $ ClassFieldAccess n i) <|>
    (do p <- primary
        period
        i <- ident
        return $ PrimaryFieldAccess p i)
-}

methodInvocationNPS :: P MethodInvocation
methodInvocationNPS =
    (do tok KW_Super >> period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ SuperMethodCall rts i as) <|>
    (do n <- name
        f <- (do as <- args
                 return $ \n -> MethodCall n as) <|>
             (period >> do
                msp <- opt (tok KW_Super >> period)
                rts <- lopt refTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                return $ \n -> mc n rts i as)
        return $ f n)

methodInvocationSuffix :: P (Exp -> MethodInvocation)
methodInvocationSuffix = do
        period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ \p -> PrimaryMethodCall p [] i as

methodInvocationExp :: P Exp
methodInvocationExp = try (do
    p <- primaryNPS
    ss <- list primarySuffix
    let mip = foldl (\a s -> s a) p ss
    case mip of
     MethodInv _ -> return mip
     _ -> fail "") <|>
     (MethodInv <$> methodInvocationNPS)

{-
methodInvocation :: P MethodInvocation
methodInvocation =
    (do tok KW_Super >> period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ SuperMethodCall rts i as) <|>
    (do p <- primary
        period
        rts <- lopt refTypeArgs
        i   <- ident
        as  <- args
        return $ PrimaryMethodCall p rts i as) <|>
    (do n <- name
        f <- (do as <- args
                 return $ \n -> MethodCall n as) <|>
             (period >> do
                msp <- opt (tok KW_Super >> period)
                rts <- lopt refTypeArgs
                i   <- ident
                as  <- args
                let mc = maybe TypeMethodCall (const ClassMethodCall) msp
                return $ \n -> mc n rts i as)
        return $ f n)
-}

args :: P [Argument]
args = parens $ seplist exp comma

-- Arrays

arrayAccessNPS :: P ArrayIndex
arrayAccessNPS = do
    n <- name
    e <- list1 $ brackets exp
    return $ ArrayIndex (ExpName n) e

arrayAccessSuffix :: P (Exp -> ArrayIndex)
arrayAccessSuffix = do
    e <- list1 $ brackets exp
    return $ \ref -> ArrayIndex ref e

arrayAccess = try arrayAccessNPS <|> do
    p <- primaryNoNewArrayNPS
    ss <- list primarySuffix
    let aap = foldl (\a s -> s a) p ss
    case aap of
     ArrayAccess ain -> return ain
     _ -> fail ""

{-
arrayAccess :: P (Exp, Exp)
arrayAccess = do
    ref <- arrayRef
    e   <- brackets exp
    return (ref, e)

arrayRef :: P Exp
arrayRef = ExpName <$> name <|> primaryNoNewArray
-}

arrayCreation :: P Exp
arrayCreation = do
    tok KW_New
    t <- nonArrayType
    f <- (try $ do
             ds <- list1 $ brackets empty
             ai <- arrayInit
             return $ \t -> ArrayCreateInit t (length ds) ai) <|>
         (do des <- list1 $ try $ brackets exp
             ds  <- list  $ brackets empty
             return $ \t -> ArrayCreate t des (length ds))
    return $ f t

literal :: P Literal
literal =
    javaToken $ \t -> case t of
        IntTok     i -> Just (Int i)
        LongTok    l -> Just (Word l)
        DoubleTok  d -> Just (Double d)
        FloatTok   f -> Just (Float f)
        CharTok    c -> Just (Char c)
        StringTok  s -> Just (String s)
        BoolTok    b -> Just (Boolean b)
        NullTok      -> Just Null
        _ -> Nothing

-- Operators

preIncDecOp, prefixOp, postfixOp :: P (Exp -> Exp)
preIncDecOp =
    (tok Op_PPlus >> return PreIncrement) <|>
    (tok Op_MMinus >> return PreDecrement)
prefixOp =
    (tok Op_Bang  >> return PreNot      ) <|>
    (tok Op_Tilde >> return PreBitCompl ) <|>
    (tok Op_Plus  >> return PrePlus     ) <|>
    (tok Op_Minus >> return PreMinus    )
postfixOp =
    (tok Op_PPlus  >> return PostIncrement) <|>
    (tok Op_MMinus >> return PostDecrement)

assignOp :: P AssignOp
assignOp =
    (tok Op_Equal    >> return EqualA   ) <|>
    (tok Op_StarE    >> return MultA    ) <|>
    (tok Op_SlashE   >> return DivA     ) <|>
    (tok Op_PercentE >> return RemA     ) <|>
    (tok Op_PlusE    >> return AddA     ) <|>
    (tok Op_MinusE   >> return SubA     ) <|>
    (tok Op_LShiftE  >> return LShiftA  ) <|>
    (tok Op_RShiftE  >> return RShiftA  ) <|>
    (tok Op_RRShiftE >> return RRShiftA ) <|>
    (tok Op_AndE     >> return AndA     ) <|>
    (tok Op_CaretE   >> return XorA     ) <|>
    (tok Op_OrE      >> return OrA      )

infixCombineOp :: P Op
infixCombineOp =
    (tok Op_And     >> return And       ) <|>
    (tok Op_Caret   >> return Xor       ) <|>
    (tok Op_Or      >> return Or        ) <|>
    (tok Op_AAnd    >> return CAnd      ) <|>
    (tok Op_OOr     >> return COr       )


infixOp :: P Op
infixOp =
    (tok Op_Star    >> return Mult      ) <|>
    (tok Op_Slash   >> return Div       ) <|>
    (tok Op_Percent >> return Rem       ) <|>
    (tok Op_Plus    >> return Add       ) <|>
    (tok Op_Minus   >> return Sub       ) <|>
    (tok Op_LShift  >> return LShift    ) <|>
    (tok Op_LThan   >> return LThan     ) <|>
    (try $ do
       tok Op_GThan
       tok Op_GThan
       tok Op_GThan
       return RRShift   ) <|>

    (try $ do
       tok Op_GThan
       tok Op_GThan
       return RShift    ) <|>

    (tok Op_GThan   >> return GThan     ) <|>
    (tok Op_LThanE  >> return LThanE    ) <|>
    (tok Op_GThanE  >> return GThanE    ) <|>
    (tok Op_Equals  >> return Equal     ) <|>
    (tok Op_BangE   >> return NotEq     )


----------------------------------------------------------------------------
-- Types

ttype :: P Type
ttype = try (RefType <$> refType) <|> PrimType <$> primType

primType :: P PrimType
primType =
    tok KW_Boolean >> return BooleanT  <|>
    tok KW_Byte    >> return ByteT     <|>
    tok KW_Short   >> return ShortT    <|>
    tok KW_Int     >> return IntT      <|>
    tok KW_Long    >> return LongT     <|>
    tok KW_Char    >> return CharT     <|>
    tok KW_Float   >> return FloatT    <|>
    tok KW_Double  >> return DoubleT

refType :: P RefType
refType =
    (do pt <- primType
        (_:bs) <- list1 arrBrackets
        return $ foldl (\f _ -> ArrayType . RefType . f)
                        (ArrayType . PrimType) bs pt) <|>
    (do ct <- classType
        bs <- list arrBrackets
        return $ foldl (\f _ -> ArrayType . RefType . f)
                            ClassRefType bs ct) <?> "refType"

nonArrayType :: P Type
nonArrayType = PrimType <$> primType <|>
    RefType <$> ClassRefType <$> classType

classType :: P ClassType
classType = ClassType <$> seplist1 classTypeSpec period

classTypeSpec :: P (Ident, [TypeArgument])
classTypeSpec = do
    i   <- ident
    tas <- lopt typeArgs
    return (i, tas)

resultType :: P (Maybe Type)
resultType = tok KW_Void >> return Nothing <|> Just <$> ttype <?> "resultType"

refTypeList :: P [RefType]
refTypeList = seplist1 refType comma

----------------------------------------------------------------------------
-- Type parameters and arguments

typeParams :: P [TypeParam]
typeParams = angles $ seplist1 typeParam comma

typeParam :: P TypeParam
typeParam = do
    i  <- ident
    bs <- lopt bounds
    return $ TypeParam i bs

bounds :: P [RefType]
bounds = tok KW_Extends >> seplist1 refType (tok Op_And)

typeArgs :: P [TypeArgument]
typeArgs = angles $ seplist1 typeArg comma

typeArg :: P TypeArgument
typeArg = tok Op_Query >> Wildcard <$> opt wildcardBound
    <|> ActualType <$> refType

wildcardBound :: P WildcardBound
wildcardBound = tok KW_Extends >> ExtendsBound <$> refType
    <|> tok KW_Super >> SuperBound <$> refType

refTypeArgs :: P [RefType]
refTypeArgs = angles refTypeList

----------------------------------------------------------------------------
-- Names

name :: P Name
name = Name <$> seplist1 ident period

ident :: P Ident
ident = javaToken $ \t -> case t of
    IdentTok s -> Just $ Ident s
    _ -> Nothing

------------------------------------------------------------

empty :: P ()
empty = return ()

opt :: P a -> P (Maybe a)
opt = optionMaybe

bopt :: P a -> P Bool
bopt p = opt p >>= \ma -> return $ isJust ma

lopt :: P [a] -> P [a]
lopt p = do mas <- opt p
            case mas of
             Nothing -> return []
             Just as -> return as

list :: P a -> P [a]
list = option [] . list1

list1 :: P a -> P [a]
list1 = many1

seplist :: P a -> P sep -> P [a]
--seplist = sepBy
seplist p sep = option [] $ seplist1 p sep

seplist1 :: P a -> P sep -> P [a]
--seplist1 = sepBy1
seplist1 p sep =
    p >>= \a ->
        try (do sep
                as <- seplist1 p sep
                return (a:as))
        <|> return [a]

startSuff, (|>>) :: P a -> P (a -> a) -> P a
startSuff start suffix = do
    x <- start
    ss <- list suffix
    return $ foldl (\a s -> s a) x ss

(|>>) = startSuff

------------------------------------------------------------

javaToken :: (Token -> Maybe a) -> P a
javaToken test = token showT posT testT
  where showT (L _ t) = show t
        posT  (L p _) = pos2sourcePos p
        testT (L _ t) = test t

tok, matchToken :: Token -> P ()
tok = matchToken
matchToken t = javaToken (\r -> if r == t then Just () else Nothing)

pos2sourcePos :: (Int, Int) -> SourcePos
pos2sourcePos (l,c) = newPos "" l c

type Mod a = [Modifier] -> a

parens, braces, brackets, angles :: P a -> P a
parens   = between (tok OpenParen)  (tok CloseParen)
braces   = between (tok OpenCurly)  (tok CloseCurly)
brackets = between (tok OpenSquare) (tok CloseSquare)
angles   = between (tok Op_LThan)   (tok Op_GThan)

endSemi :: P a -> P a
endSemi p = p >>= \a -> semiColon >> return a

comma, colon, semiColon, period :: P ()
comma     = tok Comma
colon     = tok Op_Colon
semiColon = tok SemiColon
period    = tok Period

------------------------------------------------------------

test = "public class Foo { }"
testFile file = do
  i <- readFile file
  let r = parseCompilationUnit i
  putStrLn$ either (("Parsing error:\n"++) . show) (show . pretty) r
