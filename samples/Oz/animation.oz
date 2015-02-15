functor
import
   Application
   QTk at 'x-oz://system/wp/QTk.ozf'
define
   proc {Start}
      Label
      Window = {CreateWindow ?Label}
      Animation = {New LabelAnimation init(Label delay:125)}
   in
      {Window show}
      {Animation go}
   end

   fun {CreateWindow ?Label}
      Courier = {QTk.newFont font(family:courier size:14)}
      GUI = td(
               title:"Basic Animation"
               label(text:"Hello World! " handle:Label font:Courier)
               action:proc {$} {Application.exit 0} end
               )
   in
      {QTk.build GUI}
   end

   class LabelAnimation from Time.repeat
      attr
         activeShifter:ShiftRight
         otherShifter:ShiftLeft
      feat
         label

      meth init(Label delay:Delay<=100)
         self.label = Label
         {self setRepAll(action:Animate delay:Delay)}
         {Label bind(event:"<Button-1>" action:self#Revert)}
      end

      meth Animate
         OldText = {self.label get(text:$)}
         NewText = {@activeShifter OldText}
      in
         {self.label set(text:NewText)}
      end

      meth Revert
         otherShifter := (activeShifter := @otherShifter)
      end
   end

   fun {ShiftRight Xs}
      {List.last Xs}|{List.take Xs {Length Xs}-1}
   end

   fun {ShiftLeft Xs}
      {Append Xs.2 [Xs.1]}
   end

   {Start}
end
