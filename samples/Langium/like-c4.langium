grammar LikeC4

entry LikeC4Grammar:
  (
    imports+=ImportsFromPoject |
    specifications+=SpecificationRule |
    models+=Model  |
    views+=ModelViews |
    globals+=Globals |
    deployments+=ModelDeployments  |
    likec4lib+=LikeC4Lib
  )*
;

// Lib -------------------------------------
LikeC4Lib:
  'likec4lib' '{'
    'icons' '{'
      (icons+=LibIcon)+
    '}'
  '}';

LibIcon: name=IconId;

// Specification -------------------------------------

ElementKind: name=Id;
Tag: name=Id;
RelationshipKind: name=Id;
CustomColor: name=CustomColorId;
DeploymentNodeKind: name=Id;

SpecificationRule:
  name='specification' '{'
    (
      elements+=SpecificationElementKind |
      tags+=SpecificationTag |
      relationships+=SpecificationRelationshipKind |
      colors+=SpecificationColor |
      deploymentNodes+=SpecificationDeploymentNodeKind
    )*
  '}';

SpecificationElementKind:
  'element' kind=ElementKind ('{'
    tags=Tags?
    props+=(
      SpecificationElementStringProperty |
      LinkProperty |
      ElementStyleProperty
    )*
  '}')?;

SpecificationElementStringProperty:
  key=('title' | 'description' | 'technology' | 'notation') ':'? value=MarkdownOrString ';'?;

SpecificationDeploymentNodeKind:
  'deploymentNode' kind=DeploymentNodeKind ('{'
    tags=Tags?
    props+=(
      SpecificationElementStringProperty |
      LinkProperty |
      ElementStyleProperty
    )*
  '}')?;

SpecificationTag:
  'tag' tag=Tag ('{'
    ('color' color=ColorLiteral)?
  '}')?;

SpecificationColor:
  'color' name=CustomColor color=ColorLiteral;

SpecificationRelationshipKind:
  'relationship' kind=RelationshipKind ('{'
    props+=(
      RelationshipStyleProperty |
      SpecificationRelationshipStringProperty
    )*
  '}')?
;

SpecificationRelationshipStringProperty:
  key=('technology' | 'notation') ':'? value=MarkdownOrString ';'?;

// Imports -------------------------------------

ImportsFromPoject:
  'import' ( '{' imports=Imported '}' | imports=Imported) 'from' project=String ';'?
;

Imported:
  imported=[Element:Id] ({infer Imported.prev=current} ',' imported=[Element:Id])* ';'?
;

// Model -------------------------------------

// Allow use spaces and commas in tags
Tags:
  (values+=TagRef)+ ({infer Tags.prev=current} ',' (values+=TagRef)*)* ';'?
;

Model:
  name='model' '{'
    elements+=(
      ExtendElement |
      Relation<true> |
      Element
    )*
  '}'
;

Element:
  (
    kind=[ElementKind] name=Id |
    name=Id Eq kind=[ElementKind]
  )
  (props+=String  // title
    (props+=String  // description
      (props+=String  // technology
        (props+=String)?   // tags (ignored by likec4, but still parsed for Structurizr compatibility)
      )?
    )?
  )?
  body=ElementBody?
;

ElementBody: '{'
  tags=Tags?
  props+=ElementProperty*
  elements+=(
    Relation<false> |
    Element
  )*
'}'
;

ElementProperty:
  ElementStringProperty |
  ElementStyleProperty |
  LinkProperty |
  IconProperty |
  MetadataProperty;

ElementStringProperty:
  key=('title' | 'technology' | 'description') ':'? value=MarkdownOrString ';'?;

ExtendElement:
  'extend' element=StrictFqnElementRef body=ExtendElementBody
;

ExtendElementBody:  '{'
  tags=Tags?
  props+=ExtendElementProperty*

  elements+=(
    Relation<true> |
    Element
  )*
'}'
;

ExtendElementProperty:
  LinkProperty |
  MetadataProperty;

// Hack, to strictly enforce resolution of model elements
ElementRef:
  modelElement=FqnRef;

StrictFqnElementRef:
  el=[Element:Id] ({infer StrictFqnElementRef.parent=current} StickyDot el=[Element:Id])*;


type ModelReferenceable = Element | Imported;
type Referenceable = DeploymentNode | DeployedInstance | Element | Imported;
// Reference to Elements in logical or deployment model
// Scope computation is based on the parent element:
//  - If parent is DeploymentNode, then it can reference nested DeploymentNodes and DeployedInstances
//  - If parent is DeployedInstance, then it can reference  nested  Element
//  - If parent is Element, then it can reference  nested  Element
//  Mix of DeploymentRef and ElementRef
FqnRef: value=[Referenceable:Id] ({infer FqnRef.parent=current} StickyDot value=[Referenceable:Id])*;


// Strict reference allows only direct children of parent
StrictFqnRef:
  value=[Referenceable:Id] ({infer StrictFqnRef.parent=current} StickyDot value=[Referenceable:Id])*;

Relation<isExplicit>:
  (<isExplicit> source=FqnRef | <!isExplicit> source=FqnRef?)
  (
    dotKind=RelationKindDotRef |
    '-[' kind=[RelationshipKind] ']->' |
    '->'
  )
  target=FqnRef
  (title=String  // title
    (description=String  // description
      (technology=String)?
    )?
  )?
  tags=Tags?
  body=RelationBody?
;

RelationBody: '{'
  tags=Tags?
  props+=RelationProperty*
'}'
;

RelationProperty:
  RelationStringProperty |
  RelationNavigateToProperty |
  RelationStyleProperty |
  LinkProperty |
  MetadataProperty
;

RelationStringProperty:
  key=('title' | 'technology' | 'description') ':'? value=MarkdownOrString ';'?;

RelationStyleProperty:
  key='style' '{'
    props+=RelationshipStyleProperty*
  '}'
;

MetadataProperty:
  'metadata' MetadataBody
;

MetadataBody: '{'
  props+=(MetadataAttribute)*
'}'
;

MetadataAttribute:
  key=IdTerminal ':'? value=MarkdownOrString ';'?
;

// Views -------------------------------------

ModelViews:
  name='views' '{' (
      views+=LikeC4ViewRule |
      styles+=ViewRuleStyleOrGlobalRef
    )*
  '}';

type LikeC4View = ElementView | DynamicView | DeploymentView;
LikeC4ViewRule returns LikeC4View:
  ElementView | DynamicView | DeploymentView;

ElementView:
  'view' name=Id? (
    'extends' extends=ElementViewRef |
    'of' viewOf=ElementRef
  )?
  body=ElementViewBody?
;

DynamicView:
  'dynamic' 'view' name=Id body=DynamicViewBody?
;

ViewRef:
  view=[LikeC4View];

ElementViewRef:
  view=[ElementView];

DynamicViewRef:
  view=[DynamicView];

ElementViewBody: '{'
  tags=Tags?
  props+=ViewProperty*
  rules+=ViewRule*
'}'
;

DynamicViewBody: '{'
  tags=Tags?
  props+=ViewProperty*
  (
    steps+=(DynamicViewParallelSteps | DynamicViewStep) |
    rules+=DynamicViewRule
  )*
'}'
;


type StringProperty =
  ElementStringProperty |
  ViewStringProperty |
  RelationStringProperty |
  MetadataAttribute |
  SpecificationElementStringProperty |
  SpecificationRelationshipStringProperty |
  NotationProperty |
  NotesProperty
;

ViewProperty:
  ViewStringProperty | LinkProperty
;

ViewStringProperty:
  key=('title' | 'description') ':'? value=MarkdownOrString ';'?;

ViewLayoutDirection returns string:
  'TopBottom' | 'LeftRight' | 'BottomTop' | 'RightLeft';

ViewRule:
  ViewRulePredicate |
  ViewRuleGlobalPredicateRef |
  ViewRuleGroup |
  ViewRuleStyleOrGlobalRef |
  ViewRuleAutoLayout
;

ViewRuleGroup:
  'group' title=String? '{'
    props+=(
      ColorProperty |
      BorderProperty |
      OpacityProperty
    )*
    groupRules+=(
      ViewRulePredicate |
      ViewRuleGroup
    )*
  '}';

DynamicViewRule:
  DynamicViewIncludePredicate |
  DynamicViewGlobalPredicateRef |
  ViewRuleStyleOrGlobalRef |
  ViewRuleAutoLayout
;

DynamicViewParallelSteps:
  ('parallel'|'par') '{'
    (steps+=DynamicViewStep)*
  '}'
;

DynamicViewStep:
  source=ElementRef
  (isBackward?='<-' |  '->' | '-[' kind=[RelationshipKind] ']->' | dotKind=RelationKindDotRef )
  target=ElementRef
  title=String?
  custom=CustomRelationProperties?
;

ViewRulePredicate:
  (isInclude?='include' | 'exclude')
  exprs=Expressions
;

ViewRuleGlobalPredicateRef:
  'global' 'predicate' predicate=[GlobalPredicateGroup];

DynamicViewIncludePredicate:
  'include' exprs=Expressions
;

DynamicViewGlobalPredicateRef:
  'global' 'predicate' predicate=[GlobalDynamicPredicateGroup];


ViewRuleStyle:
  'style' targets=FqnExpressions '{'
    props+=(
      StyleProperty |
      NotationProperty
    )*
  '}';

ViewRuleGlobalStyle:
  'global' 'style' style=[GlobalStyleId];

ViewRuleStyleOrGlobalRef:
  ViewRuleStyle | ViewRuleGlobalStyle;

ViewRuleAutoLayout:
  'autoLayout' direction=ViewLayoutDirection (
    rankSep=Number (
      nodeSep=Number)?
    )?;

NotationProperty:
  key='notation' ':'? value=MarkdownOrString ';'?
;
NotesProperty:
  key='notes' ':'? value=MarkdownOrString ';'?
;

CustomElementProperties: '{'
  props+=(
    NavigateToProperty |
    ElementStringProperty |
    NotationProperty |
    StyleProperty
  )*
'}'
;

CustomRelationProperties: '{'
  props+=(
    RelationNavigateToProperty |
    RelationStringProperty |
    NotationProperty |
    NotesProperty |
    RelationshipStyleProperty
  )*
'}'
;

NavigateToProperty:
  key='navigateTo' value=ViewRef;

RelationNavigateToProperty:
  key='navigateTo' value=DynamicViewRef;

// Expressions -------------------------------------

Expressions:
  value=ExpressionV2 ({infer Expressions.prev=current} ',' (value=ExpressionV2)?)*
;

ExpressionV2:
  RelationExprOrWith |
  FqnExprOrWith
;

FqnExprOrWith:
  FqnExprOrWhere ({infer FqnExprWith.subject=current} 'with' custom=CustomElementProperties?)?
;

FqnExprOrWhere:
  FqnExpr ({infer FqnExprWhere.subject=current} 'where' where=WhereElementExpression?)?
;

RelationExprOrWith:
  RelationExprOrWhere ({infer RelationExprWith.subject=current} 'with' custom=CustomRelationProperties?)?
;

RelationExprOrWhere:
  RelationExpr ({infer RelationExprWhere.subject=current} 'where' where=WhereRelationExpression?)?
;

FqnExpr:
  {infer WildcardExpression} isWildcard?='*' |
  // These are not valid in FqnExpr, but we keep for backward compatibility
  {infer ElementTagExpression} 'element.tag' IsEqual tag=TagRef |
  {infer ElementKindExpression} 'element.kind' IsEqual kind=[ElementKind] |
  FqnRefExpr
;

FqnRefExpr:
  ref=FqnRef selector=(DotUnderscore | DotWildcard)?
;

RelationExpr:
  InOutRelationExpr |
  DirectedRelationExpr
;

InOutRelationExpr infers RelationExpr:
  IncomingRelationExpr ({infer InOutRelationExpr.inout=current} '->')?
;

IncomingRelationExpr:
  '->' to=FqnExpr;

DirectedRelationExpr infers RelationExpr:
  OutgoingRelationExpr ({infer DirectedRelationExpr.source=current} target=FqnExpr)?
;

OutgoingRelationExpr:
  from=FqnExpr
  (isBidirectional?='<->' | '->' | '-[' kind=[RelationshipKind:Id] ']->' | dotKind=RelationKindDotRef)
;

FqnExpressions:
  value=FqnExpr ({infer FqnExpressions.prev=current} ',' (value=FqnExpr)?)*
;

WhereElementExpression:
  WhereElementOr
;
WhereElementOr infers WhereElementExpression:
  WhereElementAnd ({infer WhereBinaryExpression.left=current} operator='or' right=WhereElementAnd)*
;
WhereElementAnd infers WhereElementExpression:
  WhereElementPrimary ({infer WhereBinaryExpression.left=current} operator='and' right=WhereElementPrimary)*
;

WhereElementPrimary infers WhereElementExpression:
  '(' WhereElementExpression ')' |
  WhereElementNegation |
  WhereElement
;

WhereElementNegation:
  'not' value=WhereElementExpression;

WhereElement:
  {infer WhereElementTag} 'tag' EqOperator value=TagRef |
  {infer WhereElementKind} 'kind' EqOperator value=[DeploymentNodeOrElementKind]
;

WhereRelationExpression:
  WhereRelationOr
;

WhereRelationOr infers WhereRelationExpression:
  WhereRelationAnd ({infer WhereBinaryExpression.left=current} operator='or' right=WhereRelationAnd)*
;
WhereRelationAnd infers WhereRelationExpression:
  WhereRelationPrimary ({infer WhereBinaryExpression.left=current} operator='and' right=WhereRelationPrimary)*
;

WhereRelationPrimary infers WhereRelationExpression:
  '(' WhereRelationExpression ')' |
  WhereRelationNegation |
  WhereRelation
;

WhereRelationNegation:
  'not' value=WhereRelationExpression;

WhereRelation:
  {infer WhereRelationTag} 'tag' EqOperator value=TagRef |
  {infer WhereRelationKind} 'kind' EqOperator value=[RelationshipKind:Id] |
  {infer WhereRelationParticipantTag} participant=Participant StickyDot 'tag' EqOperator value=TagRef |
  {infer WhereRelationParticipantKind} participant=Participant StickyDot 'kind' EqOperator value=[DeploymentNodeOrElementKind:Id]
;
type DeploymentNodeOrElementKind = ElementKind | DeploymentNodeKind
type WhereTagEqual = WhereElementTag | WhereRelationTag | WhereRelationParticipantTag;
type WhereKindEqual = WhereElementKind | WhereRelationKind | WhereRelationParticipantKind;

type WhereExpression = WhereElementExpression | WhereRelationExpression;

// Deployment -------------------------------------

ModelDeployments:
  name='deployment' '{'
    elements+=(
      DeploymentNode |
      DeploymentRelation<true> |
      ExtendDeployment
    )*
  '}';

type DeploymentElement = DeploymentNode | DeployedInstance;

DeploymentNode:
  (
    kind=[DeploymentNodeKind:Id] name=Id |
    name=Id Eq kind=[DeploymentNodeKind:Id]
  )
  title=String?
  body=DeploymentNodeBody?
;

DeploymentNodeBody: '{'
  tags=Tags?
  props+=ElementProperty*
  elements+=(
    DeployedInstance |
    DeploymentRelation<false> |
    DeploymentNode
  )*
'}';

DeployedInstance:
  (name=Id Eq)?
  'instanceOf'
  target=ElementRef
  title=String?
  body=DeployedInstanceBody?
  ';'?
;

DeployedInstanceBody: '{'
  tags=Tags?
  props+=ElementProperty*
  elements+=(
    DeploymentRelation<false>
  )*
'}';

ExtendDeployment:
  'extend' deploymentNode=StrictFqnRef body=ExtendDeploymentBody
;

ExtendDeploymentBody:  '{'
  tags=Tags?
  props+=ExtendElementProperty*

  elements+=(
    DeployedInstance |
    DeploymentRelation<true> |
    DeploymentNode
  )*
'}'
;


DeploymentRelation<isExplicit>:
  (<isExplicit> source=FqnRef | <!isExplicit> source=FqnRef?)
  (
    dotKind=RelationKindDotRef |
    '-[' kind=[RelationshipKind] ']->' |
    '->'
  )
  target=FqnRef
  (title=String  // title
    (description=String  // description
      (technology=String)?
    )?
  )?
  tags=Tags?
  body=DeploymentRelationBody?
;

DeploymentRelationBody: '{'
  tags=Tags?
  props+=RelationProperty*
'}'
;

DeploymentView:
  'deployment' 'view' name=Id body=DeploymentViewBody?
;

DeploymentViewBody: '{'
  tags=Tags?
  props+=ViewProperty*
  rules+=DeploymentViewRule*
'}';

DeploymentViewRule:
  DeploymentViewRulePredicate |
  DeploymentViewRuleStyle |
  ViewRuleAutoLayout
;

DeploymentViewRuleStyle:
  'style' targets=FqnExpressions '{'
    props+=(
      StyleProperty |
      NotationProperty
    )*
  '}';


DeploymentViewRulePredicate:
  (isInclude?='include' | 'exclude')
  expr=Expressions
;
// Global -------------------------------------

Globals:
  name='global' '{'
    (
      predicates+=(GlobalPredicateGroup | GlobalDynamicPredicateGroup)*
      styles+=(GlobalStyle | GlobalStyleGroup)*
    )*
  '}';

GlobalPredicateGroup:
  'predicateGroup' name=IdTerminal '{'
    predicates+=ViewRulePredicate*
  '}';

GlobalDynamicPredicateGroup:
  'dynamicPredicateGroup' name=IdTerminal '{'
    predicates+=DynamicViewIncludePredicate*
  '}';

GlobalStyleId:
  name=IdTerminal;

GlobalStyle:
  'style' id=GlobalStyleId targets=FqnExpressions '{'
    props+=(
      StyleProperty |
      NotationProperty
    )*
  '}';

GlobalStyleGroup:
  'styleGroup' id=GlobalStyleId '{'
    styles+=ViewRuleStyle*
  '}';

type FqnReferenceable = Referenceable | Element | ExtendElement | ExtendDeployment;

// Common properties -------------------------------------

LinkProperty:
  key='link' ':'? value=Uri title=String? ';'?;
ColorProperty:
  key='color' ':'? (themeColor=ThemeColor | customColor=[CustomColor:CustomColorId]) ';'?;

OpacityProperty:
  key='opacity' ':'? value=Percent ';'?;

MultipleProperty:
  key='multiple' ':'? value=BOOLEAN ';'?;

// Element properties -------------------------------------
IconProperty:
  key='icon' ':'? (libicon=[LibIcon:IconId] | value=('none'|Uri)) ';'?;

ShapeProperty:
  key='shape' ':'? value=ElementShape ';'?;

BorderStyleValue returns string:
  LineOptions | 'none';

BorderProperty:
  key='border' ':'? value=BorderStyleValue ';'?;

SizeValue returns string:
  'xs' |
  'sm' |
  'md' |
  'lg' |
  'xl' |
  'xsmall' |
  'small' |
  'medium' |
  'large' |
  'xlarge';

ShapeSizeProperty:
  key='size' ':'? value=SizeValue ';'?;

PaddingSizeProperty:
  key='padding' ':'? value=SizeValue ';'?;

TextSizeProperty:
  key='textSize' ':'? value=SizeValue ';'?;

type SizeProperty = ShapeSizeProperty | PaddingSizeProperty | TextSizeProperty;

StyleProperty:
  ColorProperty |
  ShapeProperty |
  BorderProperty |
  OpacityProperty |
  IconProperty |
  MultipleProperty |
  ShapeSizeProperty |
  PaddingSizeProperty |
  TextSizeProperty
;

ElementStyleProperty:
  key='style' '{'
    props+=StyleProperty*
  '}';


// -------------------------
// Relationship Style Properties
LineProperty:
  key='line' ':'? value=LineOptions ';'?;
ArrowProperty:
  key=('head' | 'tail') ':'? value=ArrowType ';'?;

RelationshipStyleProperty:
  ColorProperty | LineProperty | ArrowProperty;

TagRef :
  Hash tag=[Tag:Id]
;

RelationKindDotRef :
  Dot kind=[RelationshipKind:Id]
;

LineOptions returns string:
  'solid' |
  'dashed' |
  'dotted'
;

ArrowType returns string:
  'none' |
  'normal' |
  'onormal' |
  'dot' |
  'odot' |
  'diamond' |
  'odiamond' |
  'crow' |
  'open' |
  'vee'
;

ThemeColor returns string:
  'primary' | 'secondary' | 'muted' | 'slate' | 'blue' | 'indigo' | 'sky' | 'red' | 'gray' | 'green' | 'amber';
ElementShape returns string:
  'rectangle' | 'person' | 'browser' | 'mobile' | 'cylinder' | 'storage' | 'queue';
Participant returns string:
  'source' | 'target';

ColorLiteral:
  RGBAColor | HexColor;

RGBAColor:
  ('rgba' | 'rgb') '(' red=Number ','? green=Number ','? blue=Number ','? alpha=(Float | Number | Percent)? ')';

// Validated later
HexColor:
  Hash hex=(Hex | Number | IdTerminal);

IconId returns string:
  LIB_ICON;

Uri returns string:
  URI_WITH_SCHEMA | URI_RELATIVE;

MarkdownOrString:
  markdown=MarkdownString | text=String;

CustomColorId returns string:
  IdTerminal | ElementShape | ArrowType | LineOptions | 'element' | 'model';

Id returns string:
  IdTerminal | ElementShape | ThemeColor | ArrowType | LineOptions | Participant | SizeValue | 'element' | 'model' | 'group' | 'node' | 'deployment' | 'instance';

fragment EqOperator:
  (
    operator=(Eq | NotEqual) |
    operator="is" not?='not'?
  )
;

fragment IsEqual:
  isEqual?=Eq | NotEqual;

// -----------------------------------
// Symbols
// terminal fragment HASH: '#';
// terminal fragment UNDERSCORE: '_';
// terminal fragment DASH: '-';
// terminal fragment LETTER: /[^\W\d_]/;
// terminal fragment DIGIT: /[0-9]/;
// terminal fragment NEWLINE: /[\r?\n]/;
// terminal fragment SPACE: /[^\S\r\n]/;

// -------------------------
// Comments
hidden terminal BLOCK_COMMENT: /\/\*[\s\S]*?\*\//;
hidden terminal LINE_COMMENT: /\/\/[^\n\r]*/;
hidden terminal WS: /[\t ]+/;
hidden terminal NL: /[\r\n]+/;

// -----------------------------------
// Terminals
//terminal LineStartWithDash: /(?<=([\r?\n][^\S\r\n]*))-/;
// terminal Boolean returns boolean: 'true' | 'false';

terminal BOOLEAN returns boolean: /\b(true|false)\b/;
// LibIcons
terminal LIB_ICON: /(aws|azure|gcp|tech):[-\w]*/;

terminal URI_WITH_SCHEMA: /\w+:\/{2}\S+/;
terminal URI_RELATIVE: /\.{0,2}\/[^\/]\S+/;

terminal DotUnderscore: /\b\._(?![_a-zA-Z])/;
terminal DotWildcard: /\b\.\*{1,2}/;
terminal Hash: '#';

// No space allowed before dot
terminal StickyDot: /\b\./;
terminal Dot: '.';
terminal NotEqual: /\!\={1,2}/;
terminal Eq: /\={1,2}/;
terminal Percent: /\b\d+%/;

terminal MarkdownString: '\'\'\'' -> '\'\'\'' | '"""' -> '"""';
terminal String: /"(?:[^"\\]|\\.)*"|'(?:[^'\\]|\\.)*'/;

// terminal fragment QuotedString: ;

// terminal  TagId: HASH LETTER (LETTER | DIGIT | UNDERSCORE | DASH)*;
// terminal IdTerminal: (LETTER | UNDERSCORE+ (LETTER | DIGIT)) (LETTER | DIGIT | UNDERSCORE | DASH)*;
terminal Float returns number: /\b\d+\.\d+\b/;
terminal Number returns number: /\b\d+\b/;
terminal IdTerminal:  /[_]*[a-zA-Z][-\w]*/;
terminal Hex: /[a-fA-F0-9]{3,}(?![-_g-zG-Z])/;
