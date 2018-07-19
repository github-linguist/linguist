Program TernaryLogic (output);

type
  trit = (terTrue, terMayBe, terFalse);

function terNot (a: trit): trit;
  begin
    case a of
      terTrue:  terNot := terFalse;
      terMayBe: terNot := terMayBe;
      terFalse: terNot := terTrue;
    end;
  end;

function terAnd (a, b: trit): trit;
  begin
    terAnd := terMayBe;
    if (a = terFalse) or (b = terFalse) then
      terAnd := terFalse
    else
      if (a = terTrue) and (b = terTrue) then
        terAnd := terTrue;
  end;

function terOr (a, b: trit): trit;
  begin
    terOr := terMayBe;
    if (a = terTrue) or (b = terTrue) then
      terOr := terTrue
    else
      if (a = terFalse) and (b = terFalse) then
        terOr := terFalse;
  end;

function terEquals (a, b: trit): trit;
  begin
    if a = b then
      terEquals := terTrue
    else
      if a <> b then
        terEquals := terFalse;
    if (a = terMayBe) or (b = terMayBe) then
      terEquals := terMayBe;
  end;

function terIfThen (a, b: trit): trit;
  begin
    terIfThen := terMayBe;
    if (a = terTrue) or (b = terFalse)  then
      terIfThen := terTrue
    else
      if (a = terFalse) and (b = terTrue) then
        terIfThen := terFalse;
  end;

function terToStr(a: trit): string;
  begin
    case a of
      terTrue:  terToStr := 'True ';
      terMayBe: terToStr := 'Maybe';
      terFalse: terToStr := 'False';
    end;
  end;

begin
  writeln('Ternary logic test:');
  writeln;
  writeln('NOT ', ' True ', ' Maybe', ' False');
  writeln('     ', terToStr(terNot(terTrue)), ' ', terToStr(terNot(terMayBe)), ' ', terToStr(terNot(terFalse)));
  writeln;
  writeln('AND   ', ' True ', ' Maybe', ' False');
  writeln('True   ', terToStr(terAnd(terTrue,terTrue)),  ' ', terToStr(terAnd(terMayBe,terTrue)),  ' ', terToStr(terAnd(terFalse,terTrue)));
  writeln('Maybe  ', terToStr(terAnd(terTrue,terMayBe)), ' ', terToStr(terAnd(terMayBe,terMayBe)), ' ', terToStr(terAnd(terFalse,terMayBe)));
  writeln('False  ', terToStr(terAnd(terTrue,terFalse)), ' ', terToStr(terAnd(terMayBe,terFalse)), ' ', terToStr(terAnd(terFalse,terFalse)));
  writeln;
  writeln('OR    ', ' True ', ' Maybe', ' False');
  writeln('True   ', terToStr(terOR(terTrue,terTrue)),  ' ', terToStr(terOR(terMayBe,terTrue)),  ' ', terToStr(terOR(terFalse,terTrue)));
  writeln('Maybe  ', terToStr(terOR(terTrue,terMayBe)), ' ', terToStr(terOR(terMayBe,terMayBe)), ' ', terToStr(terOR(terFalse,terMayBe)));
  writeln('False  ', terToStr(terOR(terTrue,terFalse)), ' ', terToStr(terOR(terMayBe,terFalse)), ' ', terToStr(terOR(terFalse,terFalse)));
  writeln;
  writeln('IFTHEN', ' True ', ' Maybe', ' False');
  writeln('True   ', terToStr(terIfThen(terTrue,terTrue)),  ' ', terToStr(terIfThen(terMayBe,terTrue)),  ' ', terToStr(terIfThen(terFalse,terTrue)));
  writeln('Maybe  ', terToStr(terIfThen(terTrue,terMayBe)), ' ', terToStr(terIfThen(terMayBe,terMayBe)), ' ', terToStr(terIfThen(terFalse,terMayBe)));
  writeln('False  ', terToStr(terIfThen(terTrue,terFalse)), ' ', terToStr(terIfThen(terMayBe,terFalse)), ' ', terToStr(terIfThen(terFalse,terFalse)));
  writeln;
  writeln('EQUAL ', ' True ', ' Maybe', ' False');
  writeln('True   ', terToStr(terEquals(terTrue,terTrue)),  ' ', terToStr(terEquals(terMayBe,terTrue)),  ' ', terToStr(terEquals(terFalse,terTrue)));
  writeln('Maybe  ', terToStr(terEquals(terTrue,terMayBe)), ' ', terToStr(terEquals(terMayBe,terMayBe)), ' ', terToStr(terEquals(terFalse,terMayBe)));
  writeln('False  ', terToStr(terEquals(terTrue,terFalse)), ' ', terToStr(terEquals(terMayBe,terFalse)), ' ', terToStr(terEquals(terFalse,terFalse)));
  writeln;
end.
