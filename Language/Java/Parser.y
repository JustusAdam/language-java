{
{-# LANGUAGE TupleSections #-}
module Language.Java.Parser (
    compilationUnit

    ) where

import Language.Java.Lexer ( LexerM, getPosition, Token(..), lexer, runLexerM)
import Language.Java.Syntax
import Language.Java.Pretty (pretty)
import Data.Maybe (isJust, fromMaybe, catMaybes)
import qualified Data.ByteString.Lazy as BS

}

%name compilationUnit CompilationUnit
%tokentype { Token }
%error { parseError }
%lexer { lexer } { EOF }
%monad { ParserM }

%left '->'
%left '=' '*=' '/=' '%=' '+='  '-='  '<<='  '>>='  '>>>='  '&='  '^='  '|='
%right '?' ':'
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '==' '!='
%left '<' '>' '<=' '>=' instanceof
%left '<<' '>>' '>>>'
%left '+' '-'
%left '*' '/' '%'
%left UNARY

%token

    abstract          { KW_Abstract }
    '@interface'      { KW_AnnInterface }
    assert            { KW_Assert }
    boolean           { KW_Boolean }
    break             { KW_Break }
    byte              { KW_Byte }
    case              { KW_Case }
    catch             { KW_Catch }
    char              { KW_Char }
    class             { KW_Class }
    const             { KW_Const }
    continue          { KW_Continue }
    default           { KW_Default }
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
    strictfp          { KW_Strictfp }
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


%%

opt(p) : p { Just $1 }
       |   { Nothing }

or(p1, p2)
       : p1 { Left $1 }
       | p2 { Right $1 }

braces(p) : '{' p '}' { $2 }
parens(p) : '(' p ')' { $2 }
brackets(p) : '[' p ']' { $2 }

many(p) : many(p) p { $1 ++ [$2] }
        |           { [] }

many1(p)
    : many(p) p   { $1 ++ [$2] }

many_sep1(p, sep)
    : many_sep(p, sep) sep p { $1 ++ [$3] }
    | p                      { [$1] }

many_sep(p, sep)
    : many_sep1(p, sep) { $1 }
    |                   { [] }

CompilationUnit :: { CompilationUnitA () }
    : opt(PackageDecl) many(ImportDecl) many_sep(TypeDecl, ';')
        { CompilationUnit $1 $2 $3 }

PackageDecl :: { PackageDecl }
    : package Name ';' { PackageDecl $2 }

WildcardSuffix :: { () }
    : '.' '*' { () }

ImportDecl :: { ImportDecl }
    : import opt(static) Name opt(WildcardSuffix) ';'
        { ImportDecl (isJust $2) $3 (isJust $4) }

QualIdent :: { [Ident] }
    : many_sep1(Ident, '.') { $1 }

-- Sort this out later, these have set ordering and not all are allowed everywhere
Modifiers :: { [Modifier] }
    : many(Modifier) { $1 }

Modifier :: { Modifier }
    : abstract        { Abstract }
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
    | Annotation      { Annotation $1 }

TypeDecl :: { TypeDecl }
    : Modifiers TypeDecl0 { $2 $1 }

TypeDecl0 :: { [Modifier] -> TypeDecl }
    : ClassDecl       { ClassTypeDecl . $1 }
    | InterfaceDecl   { InterfaceTypeDecl . $1 }

ClassDecl :: { [Modifier] -> ClassDecl }
    : NormalClassDecl { $1 }
    | EnumClassDecl   { $1 }

NormalClassDecl :: { [Modifier] -> ClassDecl }
    : class Ident OptTypeParams opt(Extends) opt(Implements) ClassBody
         { \ms -> ClassDecl ms $2 $3 $4 (fromMaybe [] $5) $6 }

ClassBody :: { ClassBody }
    : braces(many(ClassBodyDecl)) { $1 }

Extends :: { RefType }
    : extends RefType { $2 }

Implements :: { [RefType] }
    : implements many_sep1(ClassType, ',') { $2 }

EnumClassDecl :: { [Modifier] -> ClassDecl }
    : enum Ident opt(Implements) braces(EnumBody)
        { \ms -> EnumDecl ms $2 $3 $4 }

EnumBody :: { EnumBody }
    : many_sep1(EnumConstant, ',') opt(',') many(ClassBodyDecl)
        { EnumBody $1 $3 }

EnumConstant :: { EnumConstant }
    : Ident opt(Args) opt(braces(ClassBody)) { EnumConstant $1 (fromMaybe [] $2 ) $3 }

InterfaceDecl :: { [Modifier] -> InterfaceDecl }
    : Interface Ident OptTypeParams opt(Extends) many(InterfaceBodyDecl)
        { \ms -> InterfaceDecl $1 ms $2 $3 $4 (InterfaceBody $5) }

Interface :: { InterfaceKind }
    : '@interface' { InterfaceAnnotation }
    | interface    { InterfaceNormal }

ClassBodyDecl :: { Decl }
    : Modifiers ClassMemberDecl { MemberDecl ($2 $1) }
    | opt(static) Block         { InitDecl (isJust $1) $2 }

ClassMemberDecl :: { [Modifier] -> MemberDecl }
    : FieldDecl        { $1 }
    | MethodDecl       { $1 }
    | ClassDecl        { MemberClassDecl . $1 }
    | InterfaceDecl    { MemberInterfaceDecl . $1 }
    | ConstructorDecl  { $1 }

FieldDecl :: { [Modifier] -> MemberDecl }
    : Type VarDecls ';' { \ms -> FieldDecl ms $1 $2 }

OptTypeParams :: { [TypeParam] }
    : opt(TypeParams) { fromMaybe [] $1 }

TypeParams :: { [TypeParam] }
    : '<' many_sep(TypeParam, ',') '>' { $2 }

TypeParam :: { TypeParam }
    : Ident opt(Bounds) { TypeParam $1 (fromMaybe [] $2) }

Bounds :: { [Type] }
    : extends many_sep1(RefType, '&') { $2 }

Name :: { Name }
    : QualIdent { Name $1 }

ExpName :: { Exp }
    : Name { ExpName $1 }

MethodDecl :: { [Modifier] -> MemberDecl }
    : OptTypeParams ResultType Ident FormalParams OptThrowDecl MethodBody
        { \ms -> MethodDecl ms $1 $2 $3 $4 $5 None (MethodBody $6 ) }

ResultType :: { Maybe Type }
    : void  { Nothing }
    | Type  { Just $1 }

MethodBody :: { Maybe Block }
    : ';' { Nothing }
    | Block { Just $1 }

ConstructorDecl :: { [Modifier] -> MemberDecl }
    : OptTypeParams Ident FormalParams OptThrowDecl ConstrBody
        { \ms -> ConstructorDecl ms $1 $2 $3 $4 $5 }

ConstrBody :: { ConstructorBody }
    : opt(ExplConstrInv) braces(many(BlockStmt))
        { ConstructorBody $1 $2 }

ExplConstrInv :: { ExplConstrInv }
    : Primary '.' opt(RefTypeArgs) super Args
        { PrimarySuperInvoke $1 (fromMaybe [] $3 ) $5 }
    | opt(RefTypeArgs) ConstrInvTarget Args
        { $2 (fromMaybe [] $1 ) $3 }

RefTypeArgs :: { [RefType] }
    : '<' many_sep(RefType, ',') '>' { $2 }

ConstrInvTarget :: { [RefType] -> [Argument] -> ExplConstrInv }
    : super { SuperInvoke }
    | this  { ThisInvoke }

TypeArgs :: { [TypeArgument] }
    : '<' many_sep(TypeArg, ',') '>' { $2 }

TypeArg :: { TypeArgument }
    : '?' opt(WildcardBound) { Wildcard $2 }
    | RefType                { ActualType $1 }

WildcardBound :: { WildcardBound }
    : WildcardBoundType RefType { $1 $2 }

WildcardBoundType :: { RefType -> WildcardBound }
    : super   { SuperBound }
    | extends { ExtendsBound }

Args :: { [Exp] }
    : parens(many_sep(Exp, ',')) { $1 }

InterfaceBodyDecl :: { MemberDecl }
    : Modifiers InterfaceMemberDecl { $2 $1 }

InterfaceMemberDecl :: { [Modifier] -> MemberDecl }
    : ClassDecl     { MemberClassDecl . $1 }
    | InterfaceDecl { MemberInterfaceDecl . $1 }
    | FieldDecl     { $1 }
    | AbsMethodDecl { $1 }

AbsMethodDecl :: { [Modifier] -> MemberDecl }
    : OptTypeParams ResultType Ident FormalParams OptThrowDecl opt(DefaultValue) ';'
        { \ms -> MethodDecl ms $1 $2 $3 $4 $5 (fromMaybe None $6) (MethodBody Nothing) }

DefaultValue :: { DefaultValue }
    : default DefaultVal { $2 }

DefaultVal
    : braces(many_sep(Exp, ',')) { Array $1 }
    | Exp                        { Single $1 }

OptThrowDecl :: { [RefType] }
    : opt(ThrowDecl) { fromMaybe [] $1 }

ThrowDecl :: { [RefType] }
    : throws many_sep(RefType, ',') { $2 }

FormalParams :: { [FormalParam] }
    : parens(FormalParams0) { $1 }

FormalParams0 :: { [FormalParam] }
    : FormalParam ',' FormalParams0 { $1 : $3 }
    | VarArgParam                   { [$1] }
    | FormalParam                   { [$1] }

VarArgParam :: { FormalParam }
    : VarMod Type Ellipsis VarDeclId
        { FormalParam $1 $2 True $4 }

FormalParam :: { FormalParam }
    : VarMod Type VarDeclId
        { FormalParam $1 $2 False $3 }

VarMod :: { [Modifier] }
    : many(Annotation) VarMod0 { map Annotation $1 ++ $2 }

VarMod0 :: { [Modifier] }
    : final many(Annotation) { Final:map Annotation $2 }
    |                        { [] }

Ellipsis :: { () }
    : '.' '.' '.' { () }

RefType :: { RefType }
    : ClassType        { ClassRefType $1 }
    | Type dims(Empty) { ArrayType $1 }

Type :: { Type }
    : PrimType { PrimType $1 }
    | RefType  { RefType $1 }

Empty :: { () }
    : { () }

PrimType :: { PrimType }
    : boolean { BooleanT }
    | byte    { ByteT }
    | short   { ShortT }
    | int     { IntT }
    | long    { LongT }
    | char    { CharT }
    | float   { FloatT }
    | double  { DoubleT }

Annotation :: { Annotation }
    : '@' Name opt(parens(AnnotationArgs))
        { case $3 of
              Nothing -> MarkerAnnotation $2
              Just (Left args) -> NormalAnnotation $2 args
              Just (Right val) -> SingleElementAnnotation $2 val
        }

AnnotationArgs :: { Either [(Ident,ElementValue)] ElementValue }
    : many_sep(AnnotationArg, ',') { Left $1 }
    | ElementVal                   { Right $1 }

AnnotationArg :: { (Ident, ElementValue) }
    : Ident '=' ElementVal { ($1, $3) }

ElementVal :: { ElementValue }
    : InitExp    { EVVal $1 }
    | Annotation { EVAnn $1 }

InitExp :: { VarInit }
    : ArrayInit { InitArray $1 }
    | Exp       { InitExp $1 }

ArrayInit :: { ArrayInit }
    : braces(many_sep(VarInit, ',')) { ArrayInit $1 }

VarDecls :: { [VarDecl] }
    : many_sep(VarDecl, ',') { $1 }

VarDecl :: { VarDecl }
    : VarDeclId opt(VarInitAssign) { VarDecl $1 $2 }

VarInitAssign :: { VarInit }
    : '=' VarInit { $2 }

VarDeclId :: { VarDeclId }
    : Ident many(brackets(Empty)) { foldl (\f _ -> VarDeclArray . f) VarId $2 $1 }

LocalVarDecl :: { (Type, [VarDecl]) }
    : Type VarDecls { ($1, $2) }


VarInit :: { VarInit }
    : ArrayInit { InitArray $1 }
    | Exp       { InitExp $1 }

Block :: { Block }
    : braces(many(BlockStmt)) { Block $1 }

BlockStmt :: { BlockStmt }
    : Modifiers ModifiedDecl { ($2 $1) }
    | Stmt                   { BlockStmt $1 }

ModifiedDecl :: { [Modifier] -> BlockStmt }
    : ClassDecl         { LocalClass . $1 }
    | LocalVarDecl ';'  { \m -> uncurry (LocalVars m) $1 }

IfCont :: { Exp -> Stmt }
    : Stmt                  { \cond -> IfThen cond $1 }
    | StmtNSI else ElseCont { \cond -> IfThenElse cond $1 $3 }

ElseCont :: { Stmt }
    : StmtNSI   { $1 }
    | Stmt      { $1 }

StmtExpList :: { [Stmt] }
    : many_sep(StmtExp, ',') { $1 }

ForHeader :: { Stmt -> Stmt }
    : Modifiers Type Ident ':' Exp  { EnhancedFor $1 $2 $3 $5 }
    | opt(ForInit) ';' opt(Exp) ';' opt(ForUpdate) { BasicFor $1 $3 $5 }

whileStmt(stmt)
    : while parens(Exp) stmt { While $2 $3 }

forStmt(stmt)
    : for parens(ForHeader) stmt { $2 $3 }

Ident :: { Ident }
    : ident { Ident $1 }

labeled(stmt)
    : Ident ':' stmt { Labeled $1 $3 }

Stmt :: { Stmt }
    : if parens(Exp) IfCont      { $3 $2 }
    | whileStmt(Stmt)            { $1 }
    | forStmt(Stmt)              { $1 }
    | labeled(Stmt)              { $1 }
    | StmtNoTrail                { $1 }

StmtNSI :: { Stmt }
    : if parens(Exp) StmtNSI else StmtNSI { IfThenElse $2 $3 $5 }
    | whileStmt(StmtNSI)                  { $1 }
    | forStmt(StmtNSI)                    { $1 }
    | labeled(StmtNSI)                    { $1 }
    | StmtNoTrail                         { $1 }

AssertCont :: { Maybe Exp }
    : ':' Exp { Just $2 }
    |         { Nothing }

TryCont :: { Block -> Stmt }
    : many1(Catch) opt(Finally) { \block -> Try block $1 $2 }
    | Finally { \block -> Try block [] (Just $1) }

Finally :: { Block }
    : finally Block { $2 }

StmtNoTrail :: { Stmt }
    : ';'                                         { Empty }
    | Block                                       { StmtBlock $1 }
    | assert Exp AssertCont ';'                   { Assert $2 $3 }
    | switch parens(Exp) braces(many(SwitchStmt)) { Switch $2 $3 }
    | do Stmt while parens(Exp) ';'               { Do $2 $4 }
    | break opt(Ident) ';'                        { Break $2 }
    | continue opt(Ident) ';'                     { Continue $2 }
    | return opt(Exp) ';'                         { Return $2 }
    | synchronized parens(Exp) Block              { Synchronized $2 $3 }
    | throw Exp ';'                               { Throw $2 }
    | try Block TryCont                           { $3 $2 }
    | StmtExp ';'                                 { ExpStmt $1 }

ForInit :: { ForInit }
    : Modifiers LocalVarDecl { uncurry (ForLocalVars $1) $2 }
    | many_sep(StmtExp, ',') { ForInitExps $1 }

ForUpdate :: { [Exp] }
    : many_sep(StmtExp, ',') { $1 }

SwitchStmt :: { SwitchBlock }
    : SwitchLabel ':' many(BlockStmt) { SwitchBlock $1 $3 }

SwitchLabel :: { SwitchLabel }
    : default { Default }
    | Exp     { SwitchCase $1 }

Catch :: { Catch }
    : catch parens(FormalParam) Block { Catch $2 $3 }

StmtExp :: { Exp }
    : InstanceCreationExp { $1 }
    | MethodInvocation { MethodInv $1 }
    | UnaryExp         { $1 }

Assignment :: { Exp }
    : Lhs AssignOp Exp { Assign $1 $2 $3 }

AssignOp :: { AssignOp }
    : '=' { EqualA }
    | '*=' { MultA }
    | '/=' { DivA }
    | '%=' { RemA }
    | '+=' { AddA }
    | '-=' { SubA }
    | '<<=' { LShiftA }
    | '>>=' { RShiftA }
    | '>>>=' { RRShiftA }
    | '&=' { AndA }
    | '|=' { XorA }

Lhs :: { Lhs }
    : FieldAccess   { FieldLhs $1 }
    | ArrayIndex    { ArrayLhs $1 }
    | Name          { NameLhs $1 }

LamExp :: { Exp }
    : LambdaParams '->' or(Exp, Block) { Lambda $1 (either LambdaExpression LambdaBlock $3) }

LambdaParams :: { LambdaParams }
    : Ident { LambdaSingleParam $1 }
    | parens(or(many_sep(Ident, ','), FormalParams0))
        { either LambdaInferredParams LambdaFormalParams $1 }

ArrayIndex :: { ArrayIndex }
    : or(PrimaryNoArr, ExpName) many1(brackets(Exp)) { ArrayIndex (either id id $1) $2 }

Exp :: { Exp }
    : LamExp    { $1 }
    | Assignment { $1 }
    | Exp '?' Exp ':' Exp { Cond $1 $3 $5 }
    | Exp BinOp Exp { BinOp $1 $2 $3 }

BinOp :: { Op }
    : '||' { Or }
    | '&&' { And }
    | '|'  { COr }
    | '^'  { Xor }
    | '&'  { CAnd }
    | '==' { Equal }
    | '!=' { NotEq }
    | '*'  { Mult }
    | '+'  { Add }
    | '-'  { Sub }
    | '/'  { Div }
    | '%'  { Rem }
    | '<'  { LThan }
    | '>'  { GThan }
    | '>=' { GThanE }
    | '<=' { LThanE }
    | '>' '>' { RShift }
    | '<<' { LShift }
    | '>' '>' '>' { RRShift }

UnaryExp :: { Exp }
    : '+' UnaryExp %prec UNARY  { PrePlus $2 }
    | '-' UnaryExp %prec UNARY  { PreMinus $2 }
    | '++' UnaryExp %prec UNARY { PreIncrement $2 }
    | '--' UnaryExp %prec UNARY { PreDecrement $2 }
    | PostIncExp                { $1 }
    | PostDecExp                { $1 }
    | '~' UnaryExp              { PreBitCompl $2 }
    | '!' UnaryExp              { PreNot $2 }
    | PostfixExp                { $1 }
    | CastExp                   { $1 }

CastExp :: { Exp }
    : '(' Type opt(AdditionalBound) ')' or(UnaryExp, LamExp)
        { if isJust $3 then error "Additional bounds not implemented" else Cast $2 (either id id $5) }

AdditionalBound :: { ClassType }
    : '&' ClassType { $2 }

PostIncExp :: { Exp }
    : UnaryExp '++' %prec UNARY { PostIncrement $1 }

PostDecExp :: { Exp }
    : UnaryExp '--' %prec UNARY { PostDecrement $1 }

PostfixExp :: { Exp }
    : Primary { $1 }
    | ExpName { $1 }
    | PostIncExp { $1 }
    | PostDecExp { $1 }

Primary :: { Exp }
    : PrimaryNoArr { $1 }
    | ArrayCreation { $1 }

ArrayCreation :: { Exp }
    : Type DimsOrInit { $2 $1 }

DimsOrInit :: { Type -> Exp }
    : dims(Exp) opt(dims(Empty)) { \t -> ArrayCreate t $1 (maybe 1 length $2) }
    | dims(Empty) braces(many_sep(Exp, ',')) { \t -> ArrayCreateInit t (length $1) $2 }

dims(exp)
    : many1(annotated(brackets(exp))) { $1 }

annotated(p)
    : many(Annotation) p { ($1, $2) }

PrimaryNoArr :: { Exp }
    : Lit   { Lit $1 }
    | ClassLitTarget '.' class { ClassLit $1 }
    | this { This }
    | TypeName '.' this { ThisClass $1 }
    | parens(Exp) { $1 }
    | InstanceCreationExp { $1 }
    | FieldAccess { FieldAccess $1 }
    | ArrayIndex  { ArrayAccess $1 }
    | MethodInvocation { MethodInv $1 }
    | MethodRefTarget '::' opt(TypeArgs) or(Ident, new)
        { MethodRef $1 (fromMaybe [] $3 ) (either Just (const Nothing) $4) }

MethodInvocation :: { MethodInvocation }
    : Name Args { MethodCall $1 $2 }
    | MethodSelect '.' opt(TypeArgs) Ident Args { $1 (fromMaybe [] $3 ) $4 $5 }

MethodSelect :: { [RefType] -> Ident -> [Argument] -> MethodInvocation }
    : Primary { PrimaryMethodCall $1 }
    | super   { SuperMethodCall }
    | TypeName '.' super { ClassMethodCall $1 }
    | TypeName { TypeMethodCall $1 }

ClassLitTarget :: { Maybe Type }
    : void { Nothing }
    | ClassLitTarget0 opt(brackets(Empty))
      { Just $ if isJust $2
          then $1
          else RefType $ ArrayType $1
      }

ClassLitTarget0 :: { Type }
    : PrimType { PrimType $1 }
    | QualIdent { RefType $ ClassRefType $ ClassType $ map (,[]) $1 }

TypeName :: { Name }
    : Name { $1 }

FieldAccess :: { FieldAccess }
    : FieldAccessPrefix '.' Ident { $1 $3 }
    | Ident                       { ClassFieldAccess Nothing $1 }

FieldAccessPrefix :: { Ident -> FieldAccess }
    : Name              { ClassFieldAccess (Just $1 ) }
    | Primary           { PrimaryFieldAccess $1 }
    | super             { SuperFieldAccess Nothing }
    | Name '.' super    { SuperFieldAccess (Just $1) }

MethodRefTarget :: { MethodRefTarget }
    : super               { SuperTarget Nothing }
    | ClassType '.' super { SuperTarget (Just $ RefType $ ClassRefType $1) }
    | ClassType           { TypeTarget $ RefType $ ClassRefType $1 }

Lit :: { Literal }
    : IntVal    { Int $1 }
    | LongVal   { Int $1 }
    | DoubleVal { Double $1 }
    | FloatVal  { Float $1 }
    | DoubleVal { Double $1 }
    | CharVal   { Char $1 }
    | StringVal { String $1 }
    | BoolVal   { Boolean $1 }
    | null      { Null }

InstanceCreationExp :: { Exp }
    : Name '.' UnqualInstanceCreationExp    { notImplemented }
    | Primary '.' UnqualInstanceCreationExp { notImplemented }
    | UnqualInstanceCreationExp { $1 }

UnqualInstanceCreationExp :: { Exp }
    : new opt(TypeArgs) TypeToInstantiate parens(many_sep(Exp, ',')) opt(ClassBody)
        { InstanceCreation (fromMaybe [] $2) $3 $4 $5 }

ClassType :: { ClassType }
    : many_sep1(AnnParamIdent, '.') { ClassType $1 }

-- Currently just ignores the annotations, because the AST does not support it FIXIT!
AnnParamIdent :: { (Ident, [TypeParam]) }
    : many(Annotation) Ident OptTypeParams { ($2, $3) }

-- TODO check that there are not simultaneously typearguments and a diamond
TypeToInstantiate :: { TypeDeclSpecifier }
    : ClassType opt(Diamond) { TypeDeclSpecifier $1 (isJust $2) }

Diamond :: { () }
    : '<' '>'   { () }

{

type ParserM = LexerM


parseError :: Token -> ParserM a
parseError tok = do
    loc <- getPosition
    error $ "Parse error at location " ++ show loc ++ " (token " ++ show tok ++ ")"

test = "public class Foo { }"
testFile file = do
  i <- BS.readFile file
  let r = runLexerM i compilationUnit
  putStrLn$ either (("Parsing error:\n"++) . show) (show . pretty) r

notImplemented :: a
notImplemented = error "Not implemented"

}