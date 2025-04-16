type component = {displayName: string};

let module Bar = {
  let createElement c::c=? children => {
    displayName: "test"
  };
};

let module Nesting = {
  let createElement children => {
    displayName: "test"
  };
};

let module Much = {
  let createElement children => {
    displayName: "test"
  };
};

let module Foo = {
  let createElement a::a=? b::b=? children => {
    displayName: "test"
  };
};

let module One = {
  let createElement
      test::test=?
      foo::foo=?
      children => {
    displayName: "test"
  };
  let createElementobvioustypo
      test::test
      children => {
    displayName: "test"
  };
};

let module Two = {
  let createElement foo::foo=? children => {
    displayName: "test"
  };
};

let module Sibling = {
  let createElement
      foo::foo=?
      (children: list component) => {
    displayName: "test"
  };
};

let module Test = {
  let createElement yo::yo=? children => {
    displayName: "test"
  };
};

let module So = {
  let createElement children => {
    displayName: "test"
  };
};

let module Foo2 = {
  let createElement children => {
    displayName: "test"
  };
};

let module Text = {
  let createElement children => {
    displayName: "test"
  };
};

let module Exp = {
  let createElement children => {
    displayName: "test"
  };
};

let module Pun = {
  let createElement intended::intended=? children => {
    displayName: "test"
  };
};

let module Namespace = {
  let module Foo = {
    let createElement
        intended::intended=?
        anotherOptional::x=100
        children => {
      displayName: "test"
    };
  };
};

let module LotsOfArguments = {
  let createElement
      argument1::argument1=?
      argument2::argument2=?
      argument3::argument3=?
      argument4::argument4=?
      argument5::argument5=?
      argument6::argument6=?
      children => {
    displayName: "test"
  };
};

let div argument1::argument1=? children => {
  displayName: "test"
};

let module List1 = {
  let createElement children => {
    displayName: "test"
  };
};

let module List2 = {
  let createElement children => {
    displayName: "test"
  };
};

let module List3 = {
  let createElement children => {
    displayName: "test"
  };
};

let (/><) a b => a + b;

let (><) a b => a + b;

let (/>) a b => a + b;

let (><\/) a b => a + b;

let tag1 = 5 />< 6;

let tag2 = 5 >< 7;

let tag3 = 5 /> 7;

let tag4 = 5 ><\/ 7;

let b = 2;

let selfClosing = <Foo />;

let selfClosing2 = <Foo a=1 b=true />;

let selfClosing3 =
  <Foo
    a="really long values that should"
    b="cause the entire thing to wrap"
  />;

let a = <Foo> <Bar c=(fun a => a + 2) /> </Foo>;

let a3 = <So> <Much> <Nesting /> </Much> </So>;

let a4 =
  <Sibling>
    <One test=true foo=b />
    <Two foo=b />
  </Sibling>;

let a5 = <Foo> "testing a string here" </Foo>;

let a6 =
  <Foo2>
    <Text> "testing a string here" </Text>
    <Test yo=1 />
    <Text> "another string" </Text>
    <Bar />
    <Exp> (2 + 4) </Exp>
  </Foo2>;

let intended = true;

let punning = <Pun intended />;

let namespace = <Namespace.Foo />;

let c = <Foo />;

let d = <Foo />;

let spaceBefore =
  <So> <Much> <Nesting /> </Much> </So>;

let spaceBefore2 = <So> <Much /> </So>;

let siblingNotSpaced =
  <So> <Much /> <Much /> </So>;

let jsxInList = [<Foo />];

let jsxInList2 = [<Foo />];

let jsxInListA = [<Foo />];

let jsxInListB = [<Foo />];

let jsxInListC = [<Foo />];

let jsxInListD = [<Foo />];

let jsxInList3 = [<Foo />, <Foo />, <Foo />];

let jsxInList4 = [<Foo />, <Foo />, <Foo />];

let jsxInList5 = [<Foo />, <Foo />];

let jsxInList6 = [<Foo />, <Foo />];

let jsxInList7 = [<Foo />, <Foo />];

let jsxInList8 = [<Foo />, <Foo />];

let testFunc b => b;

let jsxInFnCall = testFunc <Foo />;

let lotsOfArguments =
  <LotsOfArguments
    argument1=1
    argument2=2
    argument3=3
    argument4=4
    argument5=5
    argument6="test">
    <Namespace.Foo />
  </LotsOfArguments>;

let lowerCase = <div argument1=1 />;

let b = 0;

let d = 0;

/*
 * Should pun the first example:
 */
let a = <Foo a> 5 </Foo>;

let a = <Foo a=b> 5 </Foo>;

let a = <Foo a=b b=d> 5 </Foo>;

let a = <Foo a> 0.55 </Foo>;

let a = Foo.createElement "" [@JSX];

let ident = <Foo> a </Foo>;

let fragment1 = <> <Foo /> <Foo /> </>;

let fragment2 = <> <Foo /> <Foo /> </>;

let fragment3 = <> <Foo /> <Foo /> </>;

let fragment4 = <> <Foo /> <Foo /> </>;

let fragment5 = <> <Foo /> <Foo /> </>;

let fragment6 = <> <Foo /> <Foo /> </>;

let fragment7 = <> <Foo /> <Foo /> </>;

let fragment8 = <> <Foo /> <Foo /> </>;

let fragment9 = <> 2 2 2 2 </>;

let fragment10 = <> 2.2 3.2 4.6 1.2 </>;

let fragment11 = <> "str" </>;

let fragment12 = <> (6 + 2) (6 + 2) (6 + 2) </>;

let fragment13 = <> fragment11 fragment11 </>;

let listOfItems1 = <List1> 1 2 3 4 5 </List1>;

let listOfItems2 =
  <List2> 1.0 2.8 3.8 4.0 5.1 </List2>;

let listOfItems3 =
  <List3> fragment11 fragment11 </List3>;

/*
 * Several sequential simple jsx expressions must be separated with a space.
 */
let thisIsRight a b => ();

let tagOne children => ();

let tagTwo children => ();

/* thisIsWrong <tagOne /><tagTwo />; */
thisIsRight <tagOne /> <tagTwo />;

/* thisIsWrong <tagOne> </tagOne><tagTwo> </tagTwo>; */
thisIsRight <tagOne /> <tagTwo />;

let a children => ();

let b children => ();

let thisIsOkay =
  <List1> <a /> <b /> <a /> <b /> </List1>;

let thisIsAlsoOkay =
  <List1> <a /> <b /> </List1>;

/* Doesn't make any sense, but suppose you defined an
   infix operator to compare jsx */
<a /> < <b />;

<a /> > <b />;

<a /> < <b />;

<a /> > <b />;

let listOfListOfJsx = [<> </>];

let listOfListOfJsx = [<> <Foo /> </>];

let listOfListOfJsx = [
  <> <Foo /> </>,
  <> <Bar /> </>
];

let listOfListOfJsx = [
  <> <Foo /> </>,
  <> <Bar /> </>,
  ...listOfListOfJsx
];

let sameButWithSpaces = [<> </>];

let sameButWithSpaces = [<> <Foo /> </>];

let sameButWithSpaces = [
  <> <Foo /> </>,
  <> <Bar /> </>
];

let sameButWithSpaces = [
  <> <Foo /> </>,
  <> <Bar /> </>,
  ...sameButWithSpaces
];

/*
 * Test named tag right next to an open bracket.
 */
let listOfJsx = [];

let listOfJsx = [<Foo />];

let listOfJsx = [<Foo />, <Bar />];

let listOfJsx = [<Foo />, <Bar />, ...listOfJsx];

let sameButWithSpaces = [];

let sameButWithSpaces = [<Foo />];

let sameButWithSpaces = [<Foo />, <Bar />];

let sameButWithSpaces = [
  <Foo />,
  <Bar />,
  ...sameButWithSpaces
];


/**
 * Test no conflict with polymorphic variant types.
 */
type thisType = [ | `Foo | `Bar];

type t 'a = [< thisType] as 'a;

let asd =
  <One test=true foo=2> "a" "b" </One> [@foo];

let asd2 =
  One.createElementobvioustypo
  test::false
  ["a", "b"]
  [@JSX]
  [@foo];

let span
    test::(test: bool)
    foo::(foo: int)
    children => 1;

let asd =
  <span test=true foo=2> "a" "b" </span> [@foo];

/* "video" call doesn't end with a list, so the expression isn't converted to JSX */
let video test::(test: bool) children => children;

let asd2 = video test::false 10 [@JSX] [@foo];

let div children => 1;

((fun () => div) ()) [] [@JSX];

let myFun () =>
  <>
    <Namespace.Foo
      intended=true
      anotherOptional=200
    />
    <Namespace.Foo
      intended=true
      anotherOptional=200
    />
    <Namespace.Foo
      intended=true anotherOptional=200>
      <Foo />
      <Foo />
      <Foo />
      <Foo />
      <Foo />
      <Foo />
      <Foo />
    </Namespace.Foo>
  </>;

let myFun () => <> </>;

let myFun () =>
  <>
    <Namespace.Foo
      intended=true
      anotherOptional=200
    />
    <Namespace.Foo
      intended=true
      anotherOptional=200
    />
    <Namespace.Foo
      intended=true anotherOptional=200>
      <Foo />
      <Foo />
      <Foo />
      <Foo />
      <Foo />
      <Foo />
      <Foo />
    </Namespace.Foo>
  </>;


/**
 * Children should wrap without forcing attributes to.
 */
<Foo a=10 b=0>
  <Bar />
  <Bar />
  <Bar />
  <Bar />
</Foo>;
/**
 * Failing test cases:
 */
/* let res = <Foo a=10 b=(<Foo a=200 />) > */
/*   <Bar /> */
/* </Foo>; */
/* let res = <Foo a=10 b=(<Foo a=200 />) />; */
