package Multiple_Interfaces is
   type Camera is tagged null record;
   type Mobile_Phone is limited Interface;
   type Camera_Phone is new Camera and Mobile_Phone with null record;
end Multiple_Interfaces;
