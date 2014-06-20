MODULE ObxControls;
(**
    project         = "BlackBox"
    organization    = "www.oberon.ch"
    contributors    = "Oberon microsystems"
    version         = "System/Rsrc/About"
    copyright       = "System/Rsrc/About"
    license         = "Docu/BB-License"
    changes         = ""
    issues          = ""

**)

IMPORT Dialog, Ports, Properties, Views;

CONST beginner = 0; advanced = 1; expert = 2; guru = 3;    (* user classes *)

TYPE
    View = POINTER TO RECORD (Views.View)
        size: INTEGER    (* border size in mm *)
    END;

VAR
    data*: RECORD
        class*: INTEGER;    (* current user class *)
        list*: Dialog.List;    (* list of currently available sizes, derived from class *)
        width*: INTEGER    (* width of next view to be opened. Derived from
                                    class, or entered through a text entry field *)
    END;

    predef: ARRAY 6 OF INTEGER;    (* table of predefined sizes *)


PROCEDURE SetList;
BEGIN
    IF data.class = beginner THEN
        data.list.SetLen(1);
        data.list.SetItem(0, "default")
    ELSIF data.class = advanced THEN
        data.list.SetLen(4);
        data.list.SetItem(0, "default");
        data.list.SetItem(1, "small");
        data.list.SetItem(2, "medium");
        data.list.SetItem(3, "large");
    ELSE
        data.list.SetLen(6);
        data.list.SetItem(0, "default");
        data.list.SetItem(1, "small");
        data.list.SetItem(2, "medium");
        data.list.SetItem(3, "large");
        data.list.SetItem(4, "tiny");
        data.list.SetItem(5, "huge");
    END
END SetList;

(* View *)

PROCEDURE (v: View) CopyFromSimpleView (source: Views.View);
BEGIN
    v.size := source(View).size
END CopyFromSimpleView;

PROCEDURE (v: View) Restore (f: Views.Frame; l, t, r, b: INTEGER);
BEGIN    (* fill view with a red square of size v.size *)
    IF v.size = 0 THEN v.size := predef[0] END;    (* lazy initialization of size *)
    f.DrawRect(0, 0, v.size, v.size, Ports.fill, Ports.red)
END Restore;

PROCEDURE (v: View) HandlePropMsg (VAR msg: Views.PropMessage);
BEGIN
    WITH msg: Properties.SizePref DO
        IF v.size = 0 THEN v.size := predef[0] END;    (* lazy initialization of size *)
        msg.w := v.size; msg.h := v.size    (* tell environment about desired width and height *)
    ELSE    (* ignore other messages *)
    END
END HandlePropMsg;

(* notifiers *)

PROCEDURE ClassNotify* (op, from, to: INTEGER);
BEGIN    (* react to change in data.class *)
    IF op = Dialog.changed THEN
        IF (to = beginner) OR (to = advanced) & (data.list.index > 3) THEN
            (* if class is reduced, make sure that selection contains legal elements *)
            data.list.index := 0; data.width := predef[0];    (* modify interactor *)
            Dialog.Update(data)    (* redraw controls where necessary *)
        END;
        SetList;
        Dialog.UpdateList(data.list)    (* reconstruct list box contents *)
    END
END ClassNotify;

PROCEDURE ListNotify* (op, from, to: INTEGER);
BEGIN    (* reacto to change in data.list (index to was selected) *)
    IF op = Dialog.changed THEN
        data.width := predef[to];    (* modify interactor *)
        Dialog.Update(data)    (* redraw controls where necessary *)
    END
END ListNotify;

(* guards *)

PROCEDURE ListGuard* (VAR par: Dialog.Par);
BEGIN    (* disable list box for a beginner *)
    par.disabled := data.class = beginner
END ListGuard;

PROCEDURE WidthGuard* (VAR par: Dialog.Par);
BEGIN    (* make text entry field read-only if user is not guru *)
    par.readOnly := data.class # guru
END WidthGuard;

(* commands *)

PROCEDURE Open*;
    VAR v: View;
BEGIN
    NEW(v);    (* create and initialize a new view *)
    v.size := data.width * Ports.mm;    (* define view's size in function of class *)
    Views.OpenAux(v, "Example")    (* open the view in a window *)
END Open;

BEGIN    (* initialization of global variables *)
    predef[0] := 40; predef[1] := 30; predef[2] := 50;    (* predefined sizes *)
    predef[3] := 70; predef[4] := 20; predef[5] := 100;
    data.class := beginner;    (* default values *)
    data.list.index := 0;
    data.width := predef[0];
    SetList
END ObxControls.
