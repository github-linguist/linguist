// Copyright 2010-2025 Google LLC
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

// This .swig file exposes the code declared in ../constraint_solver.h and
// ../constraint_solveri.h.
//
// USAGE EXAMPLES (most of which are also unit tests):
// - ./pywrapcp_test.py

%include "ortools/base/base.swig"
%include "ortools/util/python/proto.swig"

// PY_CONVERT_HELPER_* macros.
%include "ortools/constraint_solver/python/constraint_solver_helpers.swig"

%include "ortools/util/python/functions.swig"
%include "ortools/util/python/vector.swig"

%include "ortools/constraint_solver/python/pywrapcp_util.swig"

// We *do* need to use SWIGTYPE_... type names directly, because the
// (recommended replacement) $descriptor macro fails, as of 2019-07, with
// types such as operations_research::Solver.
// The absence of whitespace before 'swiglint' is mandatory.
//swiglint: disable swigtype-name

// We need to forward-declare the proto here, so that PROTO_INPUT involving it
// works correctly. The order matters very much: this declaration needs to be
// before the %{ #include ".../constraint_solver.h" %}.
namespace operations_research {
class AssignmentProto;
class ConstraintSolverParameters;
}  // namespace operations_research

%{
#include <setjmp.h>  // For FailureProtect. See below.

// Used in the PROTECT_FROM_FAILURE macro. See below.
struct FailureProtect {
  jmp_buf exception_buffer;
  void JumpBack() { longjmp(exception_buffer, 1); }
};

// This #includes constraint_solver.h, and inlines some C++ helpers.
#include "ortools/constraint_solver/assignment.pb.h"
#include "ortools/constraint_solver/search_limit.pb.h"
#include "ortools/constraint_solver/solver_parameters.pb.h"
%}

// We need to fully support C++ inheritance, because it is heavily used by the
// exposed C++ classes. Eg:
// class BaseClass {
//   virtual void Foo() { ... }
//   virtual void Bar() { Foo(); ... }
// };
// ...
// class SubClass {
//   // Overrides Foo; and expects the inherited Bar() to use
//   // the overriden Foo().
//   virtual void Foo() { ... }
// };
//
// See the occurrences of "director" in this file.
%module(directors="1") operations_research
// The %feature and %exception below let python exceptions that occur within
// director method propagate to the user as they were originally. See
// http://www.swig.org/Doc1.3/Python.html#Python_nn36 for example.
%feature("director:except") {
    if ($error != NULL) {
        throw Swig::DirectorMethodException();
    }
}
%exception {
  try { $action }
  catch (Swig::DirectorException &e) { SWIG_fail; }
}

// ============= Type conversions ==============

// See ./constraint_solver_helpers.swig
PY_CONVERT_HELPER_PTR(Constraint);
PY_CONVERT_HELPER_PTR(Decision);
PY_CONVERT_HELPER_PTR(IntervalVar);
PY_CONVERT_HELPER_PTR(SequenceVar);
PY_CONVERT_HELPER_INTEXPR_AND_INTVAR();

// Actual conversions. This also includes the conversion to std::vector<Class>.
PY_CONVERT(IntVar);
PY_CONVERT(IntExpr);
PY_CONVERT(Constraint);
PY_CONVERT(Decision);
PY_CONVERT(IntervalVar);
PY_CONVERT(SequenceVar);

// Support passing std::function<void(Solver*)> as argument.
// See ../utils/python/functions.swig, from which this was copied and adapted.

%{
static void PyFunctionSolverToVoid(PyObject* pyfunc,
                                   operations_research::Solver* s) {
  // () needed to force creation of one-element tuple
  PyObject* const pysolver =
      SWIG_NewPointerObj(s, SWIGTYPE_p_operations_research__Solver,
                         SWIG_POINTER_EXCEPTION);
  PyObject* const pyresult = PyObject_CallFunction(pyfunc, "(O)", pysolver);
  if (!pyresult) {
    PyErr_SetString(PyExc_RuntimeError,
                    "std::function<void(Solver*)> invocation failed.");
  } else {
    Py_DECREF(pyresult);
  }
}
%}

%typecheck(SWIG_TYPECHECK_POINTER) std::function<void(
    operations_research::Solver*)> {
  $1 = PyCallable_Check($input);
}

%typemap(in) std::function<void(operations_research::Solver*)> {
  $1 = [$input](operations_research::Solver* s) {
    return PyFunctionSolverToVoid($input, s);
  };
}

// ============= Extensions ==============

// Add display methods on BaseObject and Solver.
%extend operations_research::BaseObject {
  std::string __str__() {
    return $self->DebugString();
  }
}
%extend operations_research::Solver {
  std::string __str__() {
    return $self->DebugString();
  }
%pythoncode {
  def Add(self, ct):
    if isinstance(ct, PyConstraint):
      self.__python_constraints.append(ct)
    self.AddConstraint(ct)
  }  // %pythoncode
}
%feature("pythonappend") operations_research::Solver::Solver %{
  self.__python_constraints = []
%}

// Extend IntervalVar to provide a nicer pythonic API for precedence
// and scheduling constraints. The macros below help do that concisely.
%define PRECEDENCE_CONSTRAINT(PythonMethodName, CppEnumName)
Constraint* PythonMethodName(IntervalVar* other) {
  return $self->solver()->MakeIntervalVarRelation(
      $self, operations_research::Solver::CppEnumName, other);
}

Constraint* PythonMethodName##WithDelay(IntervalVar* other, int64_t delay) {
  return $self->solver()->MakeIntervalVarRelationWithDelay(
      $self, operations_research::Solver::CppEnumName, other, delay);
}
%enddef
%define SCHEDULING_CONSTRAINT(PythonMethodName, CppEnumName)
Constraint* PythonMethodName(int64_t date) {
  return $self->solver()->MakeIntervalVarRelation(
      $self, operations_research::Solver::CppEnumName, date);
}
%enddef
%extend operations_research::IntervalVar {
  PRECEDENCE_CONSTRAINT(EndsAfterEnd, ENDS_AFTER_END)
  PRECEDENCE_CONSTRAINT(EndsAfterStart, ENDS_AFTER_START)
  PRECEDENCE_CONSTRAINT(EndsAtEnd, ENDS_AT_END)
  PRECEDENCE_CONSTRAINT(EndsAtStart, ENDS_AT_START)
  PRECEDENCE_CONSTRAINT(StartsAfterEnd, STARTS_AFTER_END)
  PRECEDENCE_CONSTRAINT(StartsAfterStart, STARTS_AFTER_START)
  PRECEDENCE_CONSTRAINT(StartsAtEnd, STARTS_AT_END)
  PRECEDENCE_CONSTRAINT(StartsAtStart, STARTS_AT_START)
}
#undef PRECEDENCE_CONSTRAINT
#undef SCHEDULING_CONSTRAINT

// Use DebugString() for the native string conversion in python, for objects
// that support it.
%define PY_STRINGIFY_DEBUGSTRING(Class)
%extend operations_research::Class {
  std::string __repr__() {
    return $self->DebugString();
  }

  std::string __str__() {
    return $self->DebugString();
  }
}
%enddef
PY_STRINGIFY_DEBUGSTRING(BaseObject);
PY_STRINGIFY_DEBUGSTRING(IntervalVar);
PY_STRINGIFY_DEBUGSTRING(SequenceVar);
PY_STRINGIFY_DEBUGSTRING(IntVar);
PY_STRINGIFY_DEBUGSTRING(IntExpr);
PY_STRINGIFY_DEBUGSTRING(Constraint);
#undef PY_STRINGIFY_DEBUGSTRING

// Extend the solver with a few nicer pythonic methods.
%extend operations_research::Solver {
  Constraint* TreeNoCycle(const std::vector<IntVar*>& nexts,
                          const std::vector<IntVar*>& active,
                          Solver::IndexFilter1 callback = nullptr) {
    return $self->MakeNoCycle(nexts, active, callback, false);
  }

  SearchMonitor* SearchLogWithCallback(int period,
                                       std::function<std::string()> callback) {
    return $self->MakeSearchLog(period, callback);
  }

  IntExpr* ElementFunction(std::function<int64_t(int64_t)> values,
                           IntVar* const index) {
    return $self->MakeElement(values, index);
  }
}

// Add arithmetic operators to integer expressions.
%extend operations_research::IntExpr {
  IntExpr* __add__(IntExpr* other) {
    return $self->solver()->MakeSum($self, other);
  }
  IntExpr* __add__(Constraint* other) {
    return $self->solver()->MakeSum($self, other->Var());
  }
  IntExpr* __add__(int64_t v) {
    return $self->solver()->MakeSum($self, v);
  }
  IntExpr* __radd__(int64_t v) {
    return $self->solver()->MakeSum($self, v);
  }
  IntExpr* __sub__(IntExpr* other) {
    return $self->solver()->MakeDifference($self, other);
  }
  IntExpr* __sub__(Constraint* other) {
    return $self->solver()->MakeDifference($self, other->Var());
  }
  IntExpr* __sub__(int64_t v) {
    return $self->solver()->MakeSum($self, -v);
  }
  IntExpr* __rsub__(int64_t v) {
    return $self->solver()->MakeDifference(v, $self);
  }
  IntExpr* __mul__(IntExpr* other) {
    return $self->solver()->MakeProd($self, other);
  }
  IntExpr* __mul__(Constraint* other) {
    return $self->solver()->MakeProd($self, other->Var());
  }
  IntExpr* __mul__(int64_t v) {
    return $self->solver()->MakeProd($self, v);
  }
  IntExpr* __rmul__(int64_t v) {
    return $self->solver()->MakeProd($self, v);
  }
  IntExpr* __floordiv__(int64_t v) {
    return $self->solver()->MakeDiv($self, v);
  }
  IntExpr* __floordiv__(IntExpr* e) {
    return $self->solver()->MakeDiv($self, e);
  }
  IntExpr* __mod__(int64_t v) {
    return $self->solver()->MakeModulo($self, v);
  }
  IntExpr* __mod__(IntExpr* e) {
    return $self->solver()->MakeModulo($self, e);
  }
  IntExpr* __neg__() {
    return $self->solver()->MakeOpposite($self);
  }
  IntExpr* __abs__() {
    return $self->solver()->MakeAbs($self);
  }
  IntExpr* Square() {
    return $self->solver()->MakeSquare($self);
  }

  Constraint* __eq__(int64_t v) {
    return $self->solver()->MakeEquality($self, v);
  }
  Constraint* __ne__(int64_t v) {
    return $self->solver()->MakeNonEquality($self->Var(), v);
  }
  Constraint* __ge__(int64_t v) {
    return $self->solver()->MakeGreaterOrEqual($self, v);
  }
  Constraint* __gt__(int64_t v) {
    return $self->solver()->MakeGreater($self, v);
  }
  Constraint* __le__(int64_t v) {
    return $self->solver()->MakeLessOrEqual($self, v);
  }
  Constraint* __lt__(int64_t v) {
    return $self->solver()->MakeLess($self, v);
  }
  Constraint* __eq__(IntExpr* other) {
    return $self->solver()->MakeEquality($self->Var(), other->Var());
  }
  Constraint* __ne__(IntExpr* other) {
    return $self->solver()->MakeNonEquality($self->Var(), other->Var());
  }
  Constraint* __ge__(IntExpr* other) {
    return $self->solver()->MakeGreaterOrEqual($self->Var(), other->Var());
  }
  Constraint* __gt__(IntExpr* other) {
    return $self->solver()->MakeGreater($self->Var(), other->Var());
  }
  Constraint* __le__(IntExpr* other) {
    return $self->solver()->MakeLessOrEqual($self->Var(), other->Var());
  }
  Constraint* __lt__(IntExpr* other) {
    return $self->solver()->MakeLess($self->Var(), other->Var());
  }
  Constraint* __eq__(Constraint* other) {
    return $self->solver()->MakeEquality($self->Var(), other->Var());
  }
  Constraint* __ne__(Constraint* other) {
    return $self->solver()->MakeNonEquality($self->Var(), other->Var());
  }
  Constraint* __ge__(Constraint* other) {
    return $self->solver()->MakeGreaterOrEqual($self->Var(), other->Var());
  }
  Constraint* __gt__(Constraint* other) {
    return $self->solver()->MakeGreater($self->Var(), other->Var());
  }
  Constraint* __le__(Constraint* other) {
    return $self->solver()->MakeLessOrEqual($self->Var(), other->Var());
  }
  Constraint* __lt__(Constraint* other) {
    return $self->solver()->MakeLess($self->Var(), other->Var());
  }
  Constraint* MapTo(const std::vector<IntVar*>& vars) {
    return $self->solver()->MakeMapDomain($self->Var(), vars);
  }
  IntExpr* IndexOf(const std::vector<int64_t>& vars) {
    return $self->solver()->MakeElement(vars, $self->Var());
  }
  IntExpr* IndexOf(const std::vector<IntVar*>& vars) {
    return $self->solver()->MakeElement(vars, $self->Var());
  }
  IntVar* IsMember(const std::vector<int64_t>& values) {
    return $self->solver()->MakeIsMemberVar($self->Var(), values);
  }
  Constraint* Member(const std::vector<int64_t>& values) {
    return $self->solver()->MakeMemberCt($self->Var(), values);
  }
  Constraint* NotMember(const std::vector<int64_t>& starts,
                        const std::vector<int64_t>& ends) {
    return $self->solver()->MakeNotMemberCt($self, starts, ends);
  }
}

// Extend IntVar to provide natural iteration over its domains.
%extend operations_research::IntVar {
  %pythoncode {
  def DomainIterator(self):
    return iter(self.DomainIteratorAux(False))

  def HoleIterator(self):
    return iter(self.HoleIteratorAux(False))
  }  // %pythoncode
}

// Extend IntVarIterator to make it iterable in python.
%extend operations_research::IntVarIterator {
  %pythoncode {
  def __iter__(self):
    self.Init()
    return self

  def next(self):
    if self.Ok():
      result = self.Value()
      self.Next()
      return result
    else:
      raise StopIteration()

  def __next__(self):
    return self.next()
  }  // %pythoncode
}

// ============= Exposed C++ API : Solver class ==============

%ignoreall

%unignore operations_research;

namespace operations_research {

// Solver: Basic API.
%unignore Solver;
%unignore Solver::Solver;
%unignore Solver::~Solver;
%unignore Solver::AddConstraint;
%unignore Solver::Solve;

// Solver: Debug and performance counters.
%rename (WallTime) Solver::wall_time;
%rename (Solutions) Solver::solutions;
%rename (Failures) Solver::failures;
%rename (Constraints) Solver::constraints;

// Solver: IntVar creation. We always strip the "Make" prefix in python.
%rename (IntVar) Solver::MakeIntVar;
%rename (BoolVar) Solver::MakeBoolVar;
%rename (IntConst) Solver::MakeIntConst;

// Constraint
// Ignored:
// - Accept()
// - IsCastConstraint()
// Note(user): we prefer setting the 'director' feature on the individual
// methods of a class that require it, but sometimes we must actually set
// 'director' on the class itself, because it is a C++ abstract class and
// the client needs to construct it. In these cases, we don't bother
// setting the 'director' feature on individual methods, since it is done
// automatically when setting it on the class.
%feature("director") Constraint;
%unignore Constraint;
%unignore Constraint::Constraint;
%unignore Constraint::~Constraint;
%unignore Constraint::Post;
%rename (InitialPropagateWrapper) Constraint::InitialPropagate;
%unignore Constraint::Var;
%unignore Constraint::DebugString;

}  // namespace operations_research

PY_PROTO_TYPEMAP(ortools.constraint_solver.assignment_pb2,
                 AssignmentProto,
                 operations_research::AssignmentProto)
PY_PROTO_TYPEMAP(ortools.constraint_solver.solver_parameters_pb2,
                 ConstraintSolverParameters,
                 operations_research::ConstraintSolverParameters)

%include "ortools/constraint_solver/constraint_solver.h"

// Define templates instantiation after wrapping.
namespace operations_research {
%rename (RevInteger) Rev<int64_t>;
%rename (RevInteger) Rev<int64_t>::Rev;
%unignore Rev<int64_t>::Value;
%unignore Rev<int64_t>::SetValue;
%template(RevInteger) Rev<int64_t>;

#define PARENTHIZE(X...) X
%define RENAME_ASSIGNMENT_CONTAINER(TYPE, NEW_NAME)
%rename (NEW_NAME) TYPE;
%unignore TYPE::Contains;
%rename (Element) TYPE::MutableElement(int);
%unignore TYPE::Size;
%unignore TYPE::Store;
%unignore TYPE::Restore;
%template (NEW_NAME) TYPE;
%enddef

RENAME_ASSIGNMENT_CONTAINER(
    PARENTHIZE(AssignmentContainer<IntVar, IntVarElement>),
    IntVarContainer)

#undef RENAME_ASSIGNMENT_CONTAINER
#undef PARENTHIZE

}  // namespace operations_research

// ============= Custom python wrappers around C++ objects ==============
// (this section must be after the constraint_solver*.h %includes)

%pythoncode {
class PyDecision(Decision):
  def ApplyWrapper(self, solver):
    try:
       self.Apply(solver)
    except Exception as e:
      if 'CP Solver fail' in str(e):
        solver.ShouldFail()
      else:
        raise

  def RefuteWrapper(self, solver):
    try:
       self.Refute(solver)
    except Exception as e:
      if 'CP Solver fail' in str(e):
        solver.ShouldFail()
      else:
        raise

  def DebugString(self):
    return "PyDecision"

class PyConstraint(Constraint):
  def __init__(self, solver):
    super().__init__(solver)
    self.__demons = []

  def Demon(self, method, *args):
    demon = PyConstraintDemon(self, method, False, *args)
    self.__demons.append(demon)
    return demon

  def DelayedDemon(self, method, *args):
    demon = PyConstraintDemon(self, method, True, *args)
    self.__demons.append(demon)
    return demon

  def InitialPropagateDemon(self):
    return self.solver().ConstraintInitialPropagateCallback(self)

  def DelayedInitialPropagateDemon(self):
    return self.solver().DelayedConstraintInitialPropagateCallback(self)

  def InitialPropagateWrapper(self):
    try:
      self.InitialPropagate()
    except Exception as e:
      if 'CP Solver fail' in str(e):
        self.solver().ShouldFail()
      else:
        raise

  def DebugString(self):
    return "PyConstraint"
}  // %pythoncode
