{
{-# LANGUAGE TupleSections #-}
module Language.Java.Parser (
    compilationUnit

    ) where

import Language.Java.Lexer ( Alex, getPosition, Token(..), lexer)
import Language.Java.Syntax
import Language.Java.Pretty (pretty)
import Data.Maybe (isJust, fromMaybe)

}

%name compilationUnit CompilationUnit
%tokentype { Token }
%error { parseError }
%lexer { lexer } { EOF }
%monad { Alex }

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

many(p) : many(p) p { $1 : $2 }
        |           { [] }

many1(p)
    : many(p) p   { $1 : $2 }

many_sep1(p, sep)
    : many_sep(p, sep) sep p { $1 : $3 }
    | p                      { [$1] }

many_sep(p, sep)
    : many_sep1(p, sep) { $1 }
    |                   { [] }

CompilationUnit :: { CompilationUnitA () }
CompilationUnit
    : opt(PackageDecl) many(ImportDecl) many_sep(TypeDecl, ';')
        { CompilationUnitA $1 $2 (catMaybes $3) () }

PackageDecl :: { PackageDecl }
PackageDecl : package ExpName ';' { PackageDecl $2 }

WildcardSuffix :: { () }
WildcardSuffix
    : '.' '*' { () }

ImportDecl :: { ImportDecl }
ImportDecl
    : import opt(static) QualIdent opt(WildcardSuffix) ';'
        { ImportDecl (isJust $2) $3 (isJust $4) }

QualIdent :: { Name }
QualIdent
    : many_sep1(ident, '.') { Name $1 }

-- Sort this out later, these have set ordering and not all are allowed everywhere
Modifiers :: { [Modifier] }
Modifiers : many(Modifier) { $1 }

Modifier :: { Modifier }
Modifier
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
    | Annotation      { Annotation $$ }

TypeDecl :: { TypeDecl }
TypeDecl
    : Modifiers TypeDecl0 { $2 $1 }

TypeDecl0 :: { [Modifier] -> TypeDecl }
TypeDecl0
    : ClassDecl       { $1 }
    | InterfaceDecl   { $1 }

ClassDecl :: { [Modifier] -> TypeDecl }
ClassDecl
    : NormalClassDecl { $1 }
    | EnumClassDecl   { $1 }

NormalClassDecl :: { [Modifier] -> TypeDecl }
NormalClassDecl
    : class ident OptTypeParams opt(Extends) opt(Implements) ClassBody
         { \ms -> ClassDecl ms $2 $3 $4 $5 $6 }

ClassBody :: { ClassBody }
ClassBody : braces(many(ClassBodyDecl)) { $1 }

Extends :: { RefType }
Extends
    : extends ClassType { $2 }

Implements :: { [RefType] }
Implements
    : implements many_sep1(ClassType, ',') { $2 }

EnumClassDecl :: { ClassDecl }
EnumClassDecl
    : enum ident opt(Implements) braces(EnumBody)
        { \ms -> EnumDecl ms $2 $3 $4 }

EnumBody :: { EnumBody }
EnumBody
    : many_sep1(EnumConstant, ',') opt(',') many(ClassBodyDecl)
        { EnumBody $1 $3 }

EnumConstant :: { EnumConstant }
EnumConstant
    : ident opt(Args) opt(braces(ClassBody)) { EnumConstant $1 $2 $3 }

InterfaceDecl :: { InterfaceDecl }
InterfaceDecl
    : Interface ident OptTypeParams opt(Extends) many(InterfaceBodyDecl)
        { \ms -> InterfaceDecl $0 ms $2 $3 $4 (InterfaceBody $5) }

Interface :: { InterfaceKind }
Interface
    : '@interface' { InterfaceAnnotation }
    | interface    { InterfaceNormal }

ClassBodyDecl :: { Decl }
ClassBodyDecl
    : Modifiers ClassMemberDecl { MemberDecl ($2 $1) }
    | opt(static) Block         { InitDecl (isJust $1) $2 }
    | ConstructorDecl           { $1 }

ClassMemberDecl :: { MemberDecl }
ClassMemberDecl
    : FieldDecl        { $1 }
    | MethodDecl       { $1 }
    | ClassDecl        { \ms -> MemberClassDecl ($1 ms) }
    | InterfaceDecl    { \ms -> MemberInterfaceDecl ($1 ms) }

FieldDecl :: { MemberDecl }
FieldDecl
    : Type VarDecls ';' { \ms -> FieldDecl ms $1 $2 }

OptTypeParams :: { [TypeParam] }
OptTypeParams
    : opt(TypeParams) { fromMaybe [] $1 }

TypeParams :: { [TypeParam] }
TypeParams
    : '<' many_sep(TypeParam, ',') '>' { $2 }

TypeParam :: { TypeParam }
TypeParam
    : ident opt(Bounds) { $2 }

Bounds :: { [Type] }
Bounds
    : extends many_sep1(RefType, '&') { $2 }

ExpName :: { Name }
ExpName
    : many_sep1(ident,'.') { Name $1 }

MethodDecl :: { MemberDecl }
MethodDecl
    : OptTypeParams ResultType ident FormalParams opt(ThrowDecl) MethodBody
        { \ms -> MethodDecl ms $1 $2 $3 $4 (fromMaybe [] $5 ) Nothing $6 }

ResultType :: { Maybe Type }
ResultType
    : void  { Nothing }
    | Type  { Just $1 }

MethodBody :: { Maybe Block }
MethodBody
    : ';' { Nothing }
    | Block { $1 }

ConstructorDecl :: { Decl }
ConstructorDecl
    : OptTypeParams ident FormalParams opt(ThrowDecl) ConstrBody
        { \ms -> ConstructorDecl ms $1 $2 $3 $4 $5 }

ConstrBody :: { ConstructorBody }
ConstrBody
    : opt(ExplConstrInv) braces(many(BlockStmt))
        { ConstructorBody $1 $2 }

ExplConstrInv :: { ExplConstrInv }
ExplConstrInv
    : Primary '.' opt(RefTypeArgs) super Args
        { PrimarySuperInvoke $1 (fromMaybe [] $3 ) $5 }
    | opt(TypeArgs) ConstrInvTarget Args
        { $2 (fromMaybe [] $1 ) $3 }

RefTypeArgs :: { [Type] }
RefTypeArgs
    : '<' many_sep(Type, ',') '>' { $2 }

ConstrInvTarget :: { [RefType] -> [Argument] -> ExplConstrInv }
ConstrInvTarget
    : super { SuperInvoke }
    | this  { ThisInvoke }

TypeArgs :: { [TypeArgument] }
TypeArgs : '<' many_sep(TypeArg, ',') '>' { $2 }

TypeArg :: { TypeArgument }
TypeArg
    : '?' opt(WildcardBound) { Wildcard $2 }
    | Type                   { ActualType $1 }

WildcardBound :: { WildcardBound }
WildcardBound
    : or(extends, super) Type { either (const ExtendsBound) (const SuperBound) $1 $2 }

Args :: { [Exp] }
Args : parens(many_sep(Exp, ',')) { $1 }

InterfaceBodyDecl :: { MemberDecl }
InterfaceBodyDecl
    : Modifiers InterfaceMemberDecl { $2 $1 }

InterfaceMemberDecl :: { [Modifier] -> MemberDecl }
InterfaceMemberDecl
    : ClassDecl     { \ms -> MemberClassDecl ($1 ms) }
    | InterfaceDecl { \ms -> MemberInterfaceDecl ($1 ms) }
    | FieldDecl     { $1 }
    | AbsMethodDecl { $1 }

AbsMethodDecl :: { [Modifier] -> MemberDecl }
AbsMethodDecl
    : OptTypeParams ResultType ident FormalParams opt(ThrowDecl) DefaultValue ';'
        { \ms -> MethodDecl ms $1 $2 3 $4 $5 (fromMaybe None $6) }

DefaultValue :: { DefaultValue }
DefaultValue
    : default DefaultVal { $2 }

DefaultVal
    : braces(many_sep(Exp, ',')) { Single $1 }
    | Exp                        { Array $1 }

ThrowDecl :: { [Type] }
ThrowDecl
    : throws many_sep(RefType, ',') { $2 }

FormalParams :: { [FormalParam] }
FormalParams
    : parens(FormalParams0) { $1 }

FormalParams0 :: { [FormalParam] }
FormalParams0
    : FormalParam ',' FormalParams0 { $1 : $3 }
    | VarArgParam                   { [$1] }
    | FormalParam                   { [$1] }

VarArgParam :: { FormalParam }
VarArgParam
    : many(VarMod) Type Ellipsis VarDeclId
        { FormalParam $1 $2 True $4 }

FormalParam :: { FormalParam }
FormalParam
    : VarMod Type VarDeclId
        { FormalParam $1 $2 False $3 }

VarMod :: { Modifier }
VarMod
    : many(Annotation) final many(Annotation)
        { map Annotation $1 ++ maybe [] (const [Final]) $2 ++ map Annotation $3 }

Ellipsis :: { () }
Ellipsis
    : '.' '.' '.' { () }

RefType :: { Type }
RefType : ClassType { $1 }

Type :: { Type }
Type
    : or(ClassType, PrimType) opt(dims(Empty))
        { (if isJust $2 then RefType . ArrayType else id) $
             case $1 of
                 Left e -> RefType $ ClassRefType e
                 Right e -> PrimType e
        }
Empty :: { () }
Empty : { () }

PrimType :: { PrimType }
PrimType
    : boolean { BooleanT }
    | byte    { ByteT }
    | short   { ShortT }
    | int     { IntT }
    | long    { LongT }
    | char    { CharT }
    | float   { FloatT }
    | double  { DoubleT }

Annotation :: { Annotation }
Annotation
    : '@' ExpName opt(parens(AnnotationArgs))
        { case $3 of
              Nothing -> MarkerAnnotation
              Just (Left args) -> NormalAnnotation $2 args
              Just (Right val) -> SingleElementAnnotation $2 val
        }

AnnotationArgs :: { Either [(Ident,ElementValue)] ElementValue }
AnnotationArgs
    : many_sep(AnnotationArg, ',') { Left $1 }
    | ElementVal                   { Right $1 }

AnnotationArg :: { (Ident, ElementValue) }
AnnotationArg
    : ident '=' ElementVal { ($1, $2) }

ElementVal :: { ElementValue }
ElementVal
    : InitExp    { EVVal $1 }
    | Annotation { EVAnn $1 }

InitExp :: { VarInit }
InitExp
    : ArrayInit { InitArray $1 }
    | Exp       { InitExp $1 }

ArrayInit :: { [VarInit] }
ArrayInit
    : braces(many_sep(VarInit, ',')) { $1 }

VarDecls :: { [VarDecl] }
VarDecls
    : many_sep(VarDecl, ',') { $1 }

VarDecl :: { VarDecl }
VarDecl
    : VarDeclId opt(VarInitAssign) { VarDecl $1 $2 }

VarInitAssign :: { VarInit }
VarInitAssign
    : '=' VarInit { $2 }

VarDeclId :: { VarDecl }
VarDeclId
    : ident many(brackets(Empty)) { foldl (\f _ -> VarDeclArray . f) VarId $2 $1 }

LocalVarDecl :: { (Type, [VarDecl]) }
LocalVarDecl
    : Type VarDecls { ($1, $2) }


VarInit :: { VarInit }
VarInit
    : ArrayInit { InitArray $1 }
    | Exp       { InitExp $1 }


----------------------------------------------------------------------------
-- Statements

Block :: { Block }
Block : braces(many(BlockStmt)) { Block $1 }

BlockStmt :: { BlockStmt }
BlockStmt
    : Modifiers ModifiedDecl { ($2 $1) }
    | Stmt                   { BlockStmt $1 }

ModifiedDecl :: { [Modifier] -> BlockStmt }
ModifiedDecl
    : ClassDecl         { \m -> LocalClass m $1 }
    | LocalVarDecl ';'  { \m -> uncurry (LocalVars m) $1 }

IfCont :: { Exp -> Stmt }
IfCont
    : Stmt                  { \cond -> IfThen cond $1 }
    | StmtNSI else ElseCont { \cond -> IfThenElse cond $1 $3 }

ElseCont :: { Stmt }
ElseCont
    : StmtNSI   { $1 }
    | Stmt      { $1 }

StmtExpList :: { [Stmt] }
StmtExpList
    : many_sep(StmtExp, ',') { $1 }

ForHeader :: { Stmt }
ForHeader
    : Modifiers Type ident ':' Exp  { EnhancedFor $1 $2 $3 $5 }
    | ForInit ';' Exp ';' ForUpdate { BasicFor $1 $3 $5 }

whileStmt(stmt)
    : while parens(Exp) stmt { While $2 $3 }

forStmt(stmt)
    : for parens(ForHeader) stmt { $2 $3 }

Ident :: { Ident }
    : ident { Ident $1 }

labeled(stmt)
    : Ident ':' stmt { Labeled $1 $3 }

Stmt :: { Stmt }
Stmt
    : if parens(Exp) IfCont      { $3 $2 }
    | whileStmt(Stmt)            { $1 }
    | forStmt(Stmt)              { $1 }
    | labeled(Stmt)              { $1 }
    | StmtNoTrail                { $1 }

StmtNSI :: { Stmt }
StmtNSI
    : if parens(Exp) StmtNSI else StmtNSI { IfThenElse $2 $3 $5 }
    | whileStmt(StmtNSI)                  { $1 }
    | forStmt(StmtNSI)                    { $1 }
    | labeled(StmtNSI)                    { $1 }
    | StmtNoTrail                         { $1 }

AssertCont :: { Maybe Exp }
AssertCont
    : ':' Exp { Just $2 }
    |         { Nothing }

TryCont :: { Stmt }
TryCont
    : many1(Catch) opt(Finally) { Try $1 $2 }
    | Finally { Try [] $1 }

Finally :: { Block }
Finally
    : finally Block { $2 }

StmtNoTrail :: { Stmt }
    : ';'                                         { Empty }
    | Block                                       { StmtBlock $1 }
    | assert Exp AssertCont ';'                   { Assert $2 $3 }
    | switch parens(Exp) braces(many(SwitchStmt)) { Switch $2 $3 }
    | do Stmt while parens(Exp) ';'               { Do $2 $4 }
    | break opt(ident) ';'                        { Break $2 }
    | continue opt(ident) ';'                     { Continue $2 }
    | return opt(Exp) ';'                         { Return $2 }
    | synchronized parens(Exp) Block              { Synchronized $2 $3 }
    | throw Exp ';'                               { Throw $2 }
    | try Block TryCont                           { $3 $2 }
    | StmtExp ';'                                 { $1 }

ForInit :: { ForInit }
    : Modifiers LocalVarDecl { uncurry (ForLocalVars $1) $2 }
    | many_sep(StmtExp, ',') { ForInitExps $1 }

ForUpdate :: { [Exp] }
    : many_sep(StmtExp, ',') { $1 }

SwitchStmt :: { SwitchBlock }
    : SwitchLabel ':' many(BlockStmt) { SwitchBlock $1 $2 }

SwitchLabel :: { SwitchLabel }
    : default { Default }
    | Exp     { SwitchCase }

Catch :: { Catch }
    : catch parens(FormalParam) Block { Catch $2 $3 }

StmtExp :: { Exp }
    : InstanceCreationExp { $1 }
    | MethodInvocation { $1 }
    | UnaryExp         { $1 }

Assignment :: { Stmt }
    : Lhs AssignOp Exp { Assign $1 $2 $3 }

AssignOp :: { AssignOp }
    : '=' { EqualA }
    | '*=' { MultT }
    | '/=' { DivA }
    | '%=' { RemA }
    | '+=' { AddA }
    | '-=' { SubA }
    | '<<=' { LShiftA }
    | '>>=' { RshiftA }
    | '>>>=' { RRShiftA }
    | '&=' { AndA }
    | '|=' { XorA }

Lhs :: { Lhs }
Lhs
    : FieldAccess   { FieldLhs $1 }
    | ArrayIndex    { ArrayLhs $1 }
    | ident         { NameLhs $1 }

LamExp :: { Exp }
LamExp
    : LambdaParams '->' or(Exp, Block) { Lambda $1 (either LambdaExpression LambdaBlock $2) }

LambdaParams :: { LambdaParams }
LambdaParams
    : ident { LambdaSingleParam $1 }
    | parens(or(many_sep(ident, ','), FormalParams0))
        { either LambdaInferredParams LambdaFormalParams $1 }

ArrayIndex :: { ArrayIndex }
ArrayIndex
    : or(PrimaryNoArr, ExpName) many1(brackets(Exp)) { ArrayIndex (either id id $1) $2 }

Exp :: { Exp }
Exp : LamExp    { $1 }
    | Assignment { $1 }
    | Exp '?' Exp ':' Exp { Cond $1 $3 $5 }
    | Exp BinOp Exp { BinOp $1 $2 $3 }

BinOp :: { Op }
BinOp
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
    | '>'  { GTan }
    | '>=' { GTanE }
    | '<=' { LTanE }
    | '>' '>' { RShift }
    | '<<' { LShift }
    | '>' '>' '>' { RRShift }

UnaryExp :: { Exp }
UnaryExp
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
CastExp
    : '(' Type opt(AdditionalBound) ')' or(UnaryExp, LamExp)
        { if isJust $3 then error "Additional bounds not implemented" else Cast $2 (either id id $5) }

AdditionalBound :: { Type }
AdditionalBound
    : '&' ClassType { $2 }

PostIncExp :: { Exp }
PostIncExp
    : UnaryExp '++' %prec UNARY { PostIncrement $1 }

PostDecExp :: { Exp }
PostDecExp
    : UnaryExp '--' %prec UNARY { PostDecrement $1 }

PostfixExp :: { Exp }
PostfixExp
    : Primary { $1 }
    | ExpName { $1 }
    | PostIncExp { $1 }
    | PostDecExp { $1 }

Primary :: { Exp }
Primary
    : PrimaryNoArr { $1 }
    | ArrayCreation { ArrayCreate $1 }

ArrayCreation :: { Exp }
ArrayCreation
    : Type DimsOrInit { $2 $1 }

DimsOrInit :: { Type -> Exp }
DimsOrInit
    : dims(Exp) opt(dims(Empty)) { \t -> ArrayCreate t $1 (maybe 1 length $2) }
    | dims(Empty) ArrayInit { \t -> ArrayCreateInit t (length $1) $2 }

dims(exp)
    : many1(annotated(brackets(exp))) { $1 }

annotated(p)
    : many(Annotation) p { ($1, $2) }

PrimaryNoArr :: { Exp }
PrimaryNoArr
    : Lit   { Lit $1 }
    | ClassLitTarget '.' class { ClassLit $1 }
    | this { This }
    | TypeName '.' this { ThisClass $1 }
    | '(' Exp ')' { $1 }
    | InstanceCreationExp { InstanceCreation $1 }
    | FieldAccess { $1 }
    | ArrayIndex  { $1 }
    | MethodInvocation { $1 }
    | MethodRefTarget '::' opt(TypeArgs) or(ident, new)
        { MethodRef $1 (fromMaybe [] $3 ) (either Just (const Nothing) $4) }

MethodInvocation :: { Exp }
MethodInvocation
    : ident Args { MethodCall $1 $2 }
    | MethodSelect '.' opt(TypeArgs) ident Args { $1 (fromMaybe [] $2 ) $3 $4 }

MethodSelect :: { MethodInvocation }
MethodSelect
    : Primary { PrimaryMethodCall $1 }
    | super   { SuperMethodCall }
    | TypeName '.' super { ClassMethodCall $1 }
    | TypeName { TypeMethodCall $1 }

ClassLitTarget :: { Type }
ClassLitTarget
    : void { PrimType VoidT }
    | ClassLitTarget0 opt(brackets(Empty))
      { if isJust $2
          then $1
          else RefType $ ArrayType $1
      }

ClassLitTarget0 :: { Type }
ClassLitTarget0
    : PrimType { PrimType $1 }
    | TypeName { RefType $ ClassRefType $ ClassType $ map (,[]) $ $1 }

TypeName :: { Name }
TypeName : ExpName { $1 }

FieldAccess :: { FieldAccess }
FieldAccess
    : FieldAccessPrefix '.' Ident { $1 $3 }
    | Ident                       { ClassFieldAccess Nothing $1 }

FieldAccessPrefix :: { Ident -> FieldAccess }
FieldAccessPrefix
    : Ident   { ClassFieldAccess (Just $1 ) }
    | Primary { PrimaryFieldAccess $1 }
    | super { SuperFieldAccess Nothing }
    | ExpName '.' super { SuperFieldAccess (Just $1) }

MethodRefTarget :: { MethodRefTarget }
MethodRefTarget
    : super               { SuperTarget Nothing }
    | ClassType '.' super { SuperTarget (Just $ RefType $ ClassRefType $1) }
    | ClassType           { TypeTarget $ RefType $ ClassRefType $1 }

Lit :: { Literal }
Lit
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
InstanceCreationExp
    : ExpName '.' UnqualInstanceCreationExp { notImplemented }
    | Primary '.' UnqualInstanceCreationExp { notImplemented }
    | UnqualInstanceCreationExp { $1 }

UnqualInstanceCreationExp :: { Exp }
UnqualInstanceCreationExp
    : new opt(TypeArgs) TypeToInstantiate parens(many_sep(Exp, ',')) opt(ClassBody)
        { InstanceCreation (fromMaybe [] $2) $3 $4 $5 }

ClassType :: { ClassType }
ClassType
    : many_sep1(AnnParamIdent, '.') { ClassType $1 }

-- Currently just ignores the annotations, because the AST does not support it FIXIT!
AnnParamIdent :: { (Ident, [TypeParam]) }
AnnParamIdent
    : many(Annotation) ident OptTypeParams { ($2, fromMaybe [] $3) }

-- TODO check that there are not simultaneously typearguments and a diamond
TypeToInstantiate :: { TypeDeclSpecifier }
TypeToInstantiate
    : ClassType opt(Diamond) { TypeDeclSpecifier $1 (isJust $2) }

Diamond :: { () }
Diamond
    : '<' '>'   { () }

{


parseError :: Token -> Alex a
parseError tok = do
    loc <- getPosition
    error $ "Parse error at location " ++ show loc ++ " (token " ++ show tok ++ ")"

test = "public class Foo { }"
testFile file = do
  i <- readFile file
  let r = compilationUnit i
  putStrLn$ either (("Parsing error:\n"++) . show) (show . pretty) r

notImplemented :: a
notImplemented = error "Not implemented"

}